#lang racket

(require "base.rkt"
         racket/syntax)

(provide stack-print
         sum
         product
         len
         nth
         (rename-out (+& +))
         (rename-out (-& -))
         (rename-out (*& *))
         (rename-out (/& /))
         (rename-out (expt& expt))
         (rename-out (define& define))
         (rename-out (if& if))

         (rename-out (datum #%datum))
         #%app
         #%module-begin
         #%top
         #%top-interaction

         provide)


;; Datum
;;----------------------------------------------------------------------

;;   #%datum
;; A datum expands to a stack with a single element
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     (with-syntax ([(id) (generate-temporaries '(|%e|))])
       #'(stack-push (assignment 'id
                                 null
                                 (current-calls)
                                 (#%datum . d)
                                 (#%datum . d))
                     (make-stack)))]))


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
                  (assignment res-name
                              null
                              (current-calls)
                              `([f-symb ,(shape arg-vals) ...] ,arg-ids ...)
                              result)
                  args-stack))))))]))

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
               (let* ((res-name   (make-name f-str (post-inc! name-counter)))
                      (result-stack
                       (parameterize ((current-calls (cons (cons 'f res-name)
                                                           (current-calls))))
                         
                         body ...))
                      (top        (stack-top result-stack))
                      (result-val (val top))
                      (args-stack (append rargs ...)))
                 (remove-duplicates-before
                  (stack-push
                   (assignment res-name
                               null
                               (current-calls)
                               (id top)
                               (val top))
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
(define-syntax (define& stx)
  (syntax-case stx ()
    [(_ (id args ...) body ...)
     #'(define-stack-fn (id args ...) body ...)]

    [(_ id expr)
     #'(define id (let* ((result expr)
                         (top (stack-top result)))
                    (stack-push
                     (struct-copy assignment top
                                  ;; add to list of names
                                  (name (cons 'id (name top))))
                     (cdr result))))]))

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
             (select  (list (assignment id null (current-calls)
                                        select* select*))))
        (sum (*& select a))))))
