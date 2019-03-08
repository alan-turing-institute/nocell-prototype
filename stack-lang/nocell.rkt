#lang racket

(require "main.rkt"
         racket/syntax)

(provide stack-print
         sum
         product
         len
         nth
         (rename-out (=& =))
         (rename-out (+& +))
         (rename-out (-& -))
         (rename-out (*& *))
         (rename-out (/& /))
         (rename-out (expt& expt))
         (rename-out (define& define))

         (rename-out (if& if))
         if*

         (rename-out (datum #%datum))
         #%app
         #%module-begin
         #%top
         #%top-interaction

         require
         provide)


;; Datum
;;----------------------------------------------------------------------

(define (name-generator name [name-counter 0])
  (lambda () (make-name name (post-inc! name-counter))))

(define next-datum-name (name-generator "e"))

;;   #%datum
;; A datum expands to a stack with a single element
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     #'(list (make-assignment #:id    (next-datum-name)
                              #:expr  (#%datum . d)
                              #:val   (#%datum . d)))]))


;; Defining stack functions
;;----------------------------------------------------------------------

;; (define-primitive-stack-fn (f args ...) (f-str f-symb) body ...)
;;
;; Helper macro for defining the stack functions provided by this
;; module.  It expands to a definition of a function named "f", whose
;; implementation is given by the racket forms "body", and whose
;; results are labelled on the stack by symbols constructed from
;; "f-str" and a number (with a counter kept internally).  These names
;; are interned, so the name provided must be unique amoung all such
;; stack function definitions.
;;
(define-syntax (define-primitive-stack-fn stx)
  (syntax-case stx ()
    ;; f        : id?
    ;; args ... : stack? ...
    ;; f-str    : string?
    ;; f-name   : symbol?
    ;; body ... : expression? ...
    [(_ (f args ...) (f-str f-symb) body ...)
     (with-syntax ([(arg-vals ...) #'((val (stack-top args)) ...)]
                   [(arg-ids ...) #'((id (stack-top args)) ...)]
                   [(rargs ...) (syntax-reverse #'(args ...))])
       #'(define f
           (let ((name-counter 0))
             (lambda (args ...)
               (let* ((result
                       ;; shadow the actual args (which are stacks)
                       ;; when evaluating the body (which is a racket
                       ;; expression expecting the value contents of
                       ;; the stacks)
                       (let ((args arg-vals) ...) body ...))
                      (args-stack (remove-duplicates-before
                       ;; args put on the stack in reverse order
                       (append rargs ...)))
                      (res-name (make-name f-str (post-inc! name-counter))))
                 (stack-push
                  (make-assignment
                   #:id    res-name
                   #:expr  `([f-symb ,(shape arg-vals) ...] ,arg-ids ...)
                   #:val   result)
                  args-stack))))))]))

(define next-argument-name (name-generator "arg"))

(define (make-arg name s) (stack-push
                           (make-assignment
                            #:id      (next-argument-name)
                            #:name    (list name)
                            #:expr    (id (stack-top s))
                            #:val     (val (stack-top s))
                            #:context 'arg)
                           s))

;; Given a number of stacks, combine them by putting the top of each
;; of the stacks first (in order), followed by the rest of each stack.
;; This is useful for combining stacks of arguments, where the value
;; of all of the arguments is followed by the values from which they
;; are computed (rather then interspersing the results with these
;; intermediate values, as a simple "append" would produce).
;;
(define (splice-argument-stacks . stacks)
  (apply append
         (map (lambda (x) (car x)) stacks)
         (map cdr stacks)))

;; Like define-primitive-stack-fn, but the function body is assumed to
;; already be in terms of stack functions (that is, accepting stacks
;; as arguments and returning a stack).  Names of these functions do
;; not appear on the expression stack (although their primitive
;; subexpressions do).  This is a helper for implementing "define" in
;; the language.
;;
(define-syntax (define-stack-fn stx)
  (syntax-case stx ()
    ;; f        : id?
    ;; args ... : stack? ...
    ;; body ... : expression? ...
    [(_ (f args ...) body ...)
     (with-syntax ([(rargs ...) (syntax-reverse #'(args ...))]
                   [f-str #'(symbol->string 'f)])
       #'(define f
           (let ((name-counter 0))
             (lambda (args ...)
               (let* ((args (make-arg 'args args)) ...
                      (res-name   (make-name f-str (post-inc! name-counter)))
                      (result-stack
                       (parameterize ((current-calls (cons (cons 'f res-name)
                                                           (current-calls))))
                         body ...))
                      (top        (stack-top result-stack))
                      (result-val (val top))
                      (args-stack (splice-argument-stacks rargs ...)))
                 (remove-duplicates-before
                  (stack-push
                   (make-assignment #:id      res-name
                                    #:expr    (id top)
                                    #:val     (val top)
                                    #:context 'result)
                   (append
                    result-stack
                    args-stack))))))))]))

;; Given a function fn of two variables, return a function taking
;; either scalar or vector arguments and broadcasting any scalar
;; arguments to the length of any vector arguments (which are assumed
;; to have the same length).
;;
(define ((vectorize fn) . args)
  (let* ((ns (map (lambda (a)
                    (and (vector? a) (vector-length a)))
                  args))
         (ns* (remove #f ns))
         ;; all vector arguments are assumed to have the same length,
         ;; so take the length of the first, if there are any
         (n (and (cons? ns*) (car ns*))))
    (if n
        (apply vector-map fn (map (curry vector-broadcast n) args))
        (apply fn args))))


;; Stack operator definitions
;;-----------------------------------------------------------------------

;; (define id expr)
;; (define (id args ...) body ...)
;;
;; The first form causes the result of evaluating expr to be bound by
;; id, and in addition renames the result at the top of the stack to
;; 'id.
;;
;; The second form defines a function taking the stack arguments args
;; and returning the result of evaluating body.
;;

(define (rename-define d stack)
  (define top (stack-top stack))
  (stack-push
   (struct-copy assignment top
                [name (cons d (name top))])                
   (cdr stack)))

(define (copy-define d stack)
  (define top (stack-top stack))
  (stack-push
   (struct-copy assignment top
                [id (next-datum-name)]
                [name (list d)]
                [expr (id top)]
                [context 'body])
   stack))

;; Grant an *unnamed* stack-top value a name, or copy a named variable
;; to one with the given name (d).
(define (rename-or-copy d stack)
  (if (null? (name (stack-top stack)))
      (rename-define d stack)
      (copy-define d stack)))

(define-syntax (define& stx)
  (syntax-case stx ()
    [(_ (id args ...) body ...)
     #'(define-stack-fn (id args ...) body ...)]

    [(_ id expr)
     #'(define id (rename-or-copy 'id expr))]))

;; if is a function
(define-primitive-stack-fn (if& test-expr then-expr else-expr)
  ("branch" if)
  (if test-expr then-expr else-expr))

(define-primitive-stack-fn (+& a b)
  ("sum" +) ;; results show up on the stack with names like "sum0"
            ;; and with expressions in terms of "+"
  ((vectorize +) a b))

(define-primitive-stack-fn (-& a b)
  ("diff" -)
  ((vectorize -) a b))

(define-primitive-stack-fn (*& a b)
  ("prod" *)
  ((vectorize *) a b))

(define-primitive-stack-fn (/& a b)
  ("quot" /)
  ((vectorize /) a b))

(define-primitive-stack-fn (=& a b)
  ("eq?" =)
  ((vectorize =) a b))

(define-primitive-stack-fn (expt& a b)
  ("expn" expt)
  ((vectorize expt) a b))

(define-primitive-stack-fn (sum xs)
  ("vsum" sum)
  (if (vector? xs)
      (sequence-fold + 0 xs)
      xs))

(define-primitive-stack-fn (product xs)
  ("vprod" product)
  (if (vector? xs)
      (sequence-fold * 1 xs)
      xs))

;; (define-primitive-stack-fn (nth n a)
;;   ("n" nth)
;;   (if (vector? a)
;;       (vector-ref a n)
;;       a))

(define-primitive-stack-fn (len a)
  ("l" len)
  (if (vector? a)
      (vector-length a)
      0))

(define ((indicator n) i) (if (= i n) 1 0))

(define nth
  (let ((name-counter 0))
    (lambda (n a)
      (let* ((n*      (val (stack-top n)))
             (len-a*  (val (stack-top (len a))))
             (select* (build-vector len-a* (indicator n*)))
             (id      (make-name 'select (post-inc! name-counter)))
             (select  (list (make-assignment #:id      id
                                             #:expr    select*
                                             #:val     select*))))
        (sum (*& select a))))))

(define-syntax (if* stx)
  (syntax-case stx ()
    [(_ test-expr then-expr else-expr)
     #'(if (val (stack-top test-expr))
           then-expr
           else-expr)]))
