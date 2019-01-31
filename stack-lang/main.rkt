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
         (rename-out (let& let))
         (rename-out (let*& let*))
         (rename-out (define& define))
         (rename-out (if& if))

         (rename-out (datum #%datum))
         #%app
         #%module-begin
         #%top
         #%top-interaction
         
         ;; ;; so we can call regular racket functions in the top level
         ;; (rename-out (#%app racket-app))
         ;; (except-out (all-from-out racket)
         ;;             + - * / expt let let* define if display
         ;;             #%app #%datum #%module-begin
         ;;             #%top #%top-interaction)
         )

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
;;-----------------------------------------------------------------------
;; A stack is a list of lists, of the form "(name expr val)" where
;; name (: symbol?) is the name of the entry on the stack, expr (: list)
;; is the expression used to compute it (which can refer to names
;; deeper on the stack)
;;
(define (make-stack)        (list))
(define (stack-push v s)    (cons v s))
(define (stack-peek name s) (assv name s))
(define (stack-top s)       (car s))
(define (name v)            (car v))
(define (expr v)            (cadr v))
(define (val  v)            (caddr v))

;; stack-top-rename : symbol? stack? -> stack?
;;
;; Given a stack, return a copy with the name of the top item changed
;; to new-id.
(define (stack-top-rename new-id stack)
  (let ((top (stack-top stack)))
    (cons (cons new-id (cdr top)) (cdr stack))))

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

(define-syntax-rule (stack-print stack)
  (let* ((name-fmt (map (compose ~a name) stack))
         (val-fmt  (map (compose vector-format val) stack))
         (expr-fmt (map (compose ~a expr) stack))
         (name-width (apply max (map string-length name-fmt)))
         (val-width  (apply max (map string-length val-fmt)))
         (expr-width (apply max (map string-length expr-fmt))))
    (display
     (foldl (lambda (n v e acc)
              (string-append
               (format "~a | ~a | ~a~%"
                       (~a n #:min-width name-width)
                       (~a e #:min-width expr-width)
                       (~a v #:min-width val-width))
               acc))
           "" name-fmt val-fmt expr-fmt))))


;; Datum
;;----------------------------------------------------------------------

;;   #%datum
;; A datum expands to a stack with a single element
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     (with-syntax ([(name) (generate-temporaries '(|%e|))])
       #'(stack-push (list 'name (#%datum . d) (#%datum . d))
                     (make-stack)))]))


;; Defining stack functions
;;----------------------------------------------------------------------

;; (define-stack-fn (f args ...) (f-str f-symb) body ...)
;;
;; Macro that expands to a definition of a function named "f", whose
;; implementation is given by the racket forms body, and whose results
;; are labelled on the stack by symbols constructed from "f-str" and a
;; number (with a counter kept internally).  These names are interned,
;; so the name provided must be unique amoung all such stack function
;; definitions.
;;
(define-syntax (define-stack-fn stx)
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
                                   (quasiquote (f-symb (unquote arg-names) ...))
                                   result)
                             args-stack))))))]))

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

;; let should cause the value at the top of the stack returned by expr
;; to be renamed to id.
;;
(define-syntax-rule (let& ((id expr) ...) body ...)
  (let ((id (stack-top-rename 'id expr)) ...)
    body ...))
(define-syntax-rule (let*& ((id expr) ...) body ...)
  (let* ((id (stack-top-rename 'id expr)) ...)
    body ...))

(define-syntax-rule (define& id expr)
  (define id (let& ((id expr)) id)))

;; if is a function 
(define-stack-fn (if& test-expr then-expr else-expr)
  ("branch" if)
  (if test-expr then-expr else-expr))

(define-stack-fn (+& a b)
  ("sum" +) ;; results show up on the stack with names like "sum0"
            ;; and with expressions in terms of "+"
  ((vectorize +) a b))

(define-stack-fn (-& a b)
  ("diff" -)
  ((vectorize -) a b))

(define-stack-fn (*& a b)
  ("prod" *)
  ((vectorize *) a b))

(define-stack-fn (/& a b)
  ("quot" /)
  ((vectorize /) a b))

(define-stack-fn (expt& a b)
  ("expn" expt)
  ((vectorize expt) a b))

(define-stack-fn (sum xs)
  ("vsum" sum)
  (if (vector? xs)
      (sequence-fold + 0 xs)
      xs))

(define-stack-fn (product xs)
  ("vprod" product)
  (if (vector? xs)
      (sequence-fold * 0 xs)
      xs))

(define-stack-fn (nth n a)
  ("n" nth)
  (if (vector? a)
      (vector-ref a n)
      a)) ;; "broadcast" scalars to whatever we ask for

(define-stack-fn (len a)
  ("l" len)
  (if (vector? a)
      (vector-length a)
      0))

;; Alternative definitions for binary ops
;; -----------------------------------------------------------------------
;;
;; macro that expands to a definition of a function named "f", whose
;; implementation is given by the racket function "f-impl" and whose
;; results are labelled "f-name" on the stack.  These names are
;; interned, so should be unique to a given function definition.
;;
;; f : id?
;; f-impl : procedure?
;; f-name : string?
;;
#|
(define-syntax-rule (define-stack-fn f f-impl f-name)
  (define f
    (let ((name-counter 0))
      (lambda (a b current-stack)
        (let* ((result   (f-impl (val (stack-top a))
                                 (val (stack-top b))))
               (ab-stack (append b a current-stack))
               (res-name (make-name f-name (post-inc! name-counter))))
          (cons (list res-name
                      `(f-impl ,(name (stack-top a)) ,(name (stack-top b)))
                      result)
                ab-stack))))))


(define-stack-fn +& + "sum")
(define-stack-fn -& - "diff")
(define-stack-fn *& * "prod")
(define-stack-fn /& / "quot")
|#
