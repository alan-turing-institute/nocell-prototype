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

(define-syntax-rule (post-inc! x)
  (begin0
      x
    (set! x (+ x 1))))

(define (make-name base n)
  (format-symbol "%~a~a" base n))

;; Like remove-duplicates, but the *last* occurrence of any duplicate
;; is kept instead of the first occurrence
(define (remove-duplicates-before xs)
  (reverse (remove-duplicates (reverse xs))))

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
(define (stack-peek name s) (assv name s))
(define (stack-top s)       (car s))
(define (name v)            (car v))
(define (scope v)           (cadr v))
(define (expr v)            (caddr v))
(define (val  v)            (cadddr v))

;; The stack language supports nested function definitions: the scope
;; of a name is provided by the list of enclosing definitions
;; (innermost first; the top-level scope is empty).
(define current-scope (make-parameter '()))

;; stack-top-rename : symbol? stack? -> stack?
;;
;; Given a stack, return a copy with the name of the top item changed
;; to new-id and the scope set to the current scope.
(define (stack-top-rename new-id stack)
  (let ((top (stack-top stack)))
    (cons (list* new-id (current-scope) (cddr top)) (cdr stack))))

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
  (let* ((scope-fmt   (map (compose ~a scope) stack))
         (name-fmt    (map (compose ~a name) stack))
         (val-fmt     (map (compose vector-format val) stack))
         (expr-fmt    (map (compose ~a expr) stack))
         (scope-width (apply max (map string-length scope-fmt)))
         (name-width  (apply max (map string-length name-fmt)))
         (val-width   (apply max (map string-length val-fmt)))
         (expr-width  (apply max (map string-length expr-fmt))))
    (display
     (foldl (lambda (s n v e acc)
              (string-append
               (format "~a ~a | ~a | ~a~%"
                       (~a s #:min-width scope-width)
                       (~a n #:min-width name-width)
                       (~a e #:min-width expr-width)
                       (~a v #:min-width val-width))
               acc))
            "" scope-fmt name-fmt val-fmt expr-fmt))))


;; Datum
;;----------------------------------------------------------------------

;;   #%datum
;; A datum expands to a stack with a single element
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     (with-syntax ([(name) (generate-temporaries '(|%e|))])
       #'(stack-push (list 'name (current-scope) (#%datum . d) (#%datum . d))
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
     (with-syntax ([(arg-vals ...)  #'((val  (stack-top args)) ...)]
                   [(arg-names ...) #'((name (stack-top args)) ...)]
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
                 (stack-push (list res-name
                                   (current-scope)
                                   (quasiquote (f-symb (unquote arg-names) ...))
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
               (let* ((result-stack
                       (parameterize ((current-scope (cons 'f (current-scope))))
                         body ...))
                      (result-val (val (stack-top result-stack)))
                      (args-stack (append rargs ...))
                      (res-name (make-name f-str (post-inc! name-counter))))
                 (remove-duplicates-before
                  (append
                   (stack-top-rename res-name result-stack)
                   args-stack)))))))]))

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
    [(_ id expr)
     #'(define id (stack-top-rename 'id expr))]

    [(_ (id args ...) body ...)
     #'(define-stack-fn (id args ...) body ...)]))

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
      (sequence-fold * 0 xs)
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
