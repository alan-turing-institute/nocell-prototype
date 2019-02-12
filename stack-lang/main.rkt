#lang racket

(require racket/syntax)

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

;; Utilities
;;-----------------------------------------------------------------------

(define (shape x)
  (if (vector? x)
      (list (vector-length x))
      (list)))

(define-syntax-rule (post-inc! x)
  (begin0
      x
    (set! x (+ x 1))))

(define (make-name base n)
  (format-symbol "%~a~a" base n))

;; Like remove-duplicates, but the *last* occurrence of any duplicate
;; is kept instead of the first occurrence
(define (remove-duplicates-before xs)
  (reverse (remove-duplicates (reverse xs) #:key id)))

;; vector-broadcast : vector? -> vector?
;;
;; If the vector a has a single element, then extend it to a vector
;; with n copies of this element; otherwise return the original
;; vector.
;;
(define (vector-broadcast n a)
  (if (vector? a)
      a
      (build-vector n (const a))))

;; syntax-reverse : syntax? -> syntax?
;;
;; Giving a syntax object comprising a list, return a new syntax
;; object otherwise identical but with this list reversed.
(define-for-syntax (syntax-reverse stx)
  (datum->syntax stx (reverse (syntax->list stx))))


;; Stacks
;; -----------------------------------------------------------------------
;; A stack is a list of lists, of the form "(name scope expr val)"
;; where name (: symbol?) is the name of the entry on the stack, scope
;; (: list?) is a list of any enclosing definitions, expr (: list) is
;; the expression used to compute it (which can refer to names deeper
;; on the stack) and val is the value of the result.
;;
(define (make-stack)        (list))
(define (stack-push v s)    (cons v s))
(define (stack-top s)       (car s))

;; Our stack is a list with elements that are the following struct,
;; each representing a named assignment, with the following elements:
;;
;; - id (: symbol?) is the unique name of the entry on the stack,
;;
;; - name (: (List symbol?)) is a (possibly empty) list of
;; user-defined names given to a stack entry
;;
;; - calls (: (List pair?)) are the current calls to user defined
;; functions for a stack entry (the call stack), where each entry is a
;; pair of the function name and the result id
;; 
;; - expr (: list?) is the expression used to compute it (which can
;; refer to names deeper on the stack).  The function name is
;; annotated with the shape of its arguments, for example, an entry of
;; ([+ () (3)] %e1 %e2) means the sum of the scalar %e1 and the vector
;; of three elements %e2 (with broadcasting).
;;
;; - val is the value of the result of evaluating expr
;;
(struct assignment (id name calls expr val) #:transparent)

;; shorter names for things...
(define (id    v) (assignment-id   v))
(define (name  v) (assignment-name  v))
(define (calls v) (assignment-calls v))
(define (expr  v) (assignment-expr  v))
(define (val   v) (assignment-val   v))

;; the call-stack of functions
(define current-calls (make-parameter '()))


;; Pretty print stacks
;;
(define (vector-format a)
  (let ((format-elt (lambda (x)
                      (if (number? x)
                          (~a (~r x #:min-width 9 #:precision 4)
                              #:width 10 #:align 'right)
                          (~a x #:width 10)))))
    (if (vector? a)
        (string-join (map format-elt (vector->list a)) " ")
        (format-elt a))))

(define (stack-print stack)
  (let* ((calls-fmt   (map (compose ~a calls) stack))
         (name-fmt    (map (compose ~a name) stack))
         (id-fmt      (map (compose ~a id) stack))
         (val-fmt     (map (compose vector-format val) stack))
         (expr-fmt    (map (compose ~a expr) stack))
         (calls-width (apply max (map string-length calls-fmt)))
         (name-width  (apply max (map string-length name-fmt)))
         (id-width    (apply max (map string-length id-fmt)))
         (val-width   (apply max (map string-length val-fmt)))
         (expr-width  (apply max (map string-length expr-fmt))))
    (display
     (foldl (lambda (c n k v e acc)
              (string-append
               (format "~a ~a ~a | ~a | ~a~%"
                       (~a c #:min-width calls-width)
                       (~a n #:min-width name-width)
                       (~a k #:min-width id-width)
                       (~a e #:min-width expr-width)
                       (~a v #:min-width val-width))
               acc))
            "" calls-fmt name-fmt id-fmt val-fmt expr-fmt))))


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

(define-primitive-stack-fn (nth n a)
  ("n" nth)
  (if (vector? a)
      (vector-ref a n)
      a))

(define-primitive-stack-fn (len a)
  ("l" len)
  (if (vector? a)
      (vector-length a)
      0))
