#lang racket

(require racket/syntax
         racket/struct)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

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

;; the call-stack of functions
(define current-calls (make-parameter '()))


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
;; - sampler is a thunk returning samples from the distribution of the
;; result, intended to be used from inside e.g. a MH sampler
;;
;; - context is either 'arg, 'body, 'result, 'result-mean, 'result-stdev
;;
;; - note is a list of zero or one strings, intended to provide a
;; human-readable comment or annotation of the meaning of the value
(struct assignment (id name calls expr val sampler context note) #:transparent
  ;; ignore the sampler when comparing assignments
  #:methods gen:equal+hash
  [(define (equal-proc a b recursive-equal?)
     (equal? (struct->list (struct-copy assignment a [sampler #f]))
             (struct->list (struct-copy assignment b [sampler #f]))))
   (define (hash-proc a recursive-equal-hash)
     (equal-hash-code (struct->list (struct-copy assignment a [sampler #f]))))
   (define (hash2-proc a recursive-equal-hash)
     (add1 (equal-secondary-hash-code
            (struct->list (struct-copy assignment a [sampler #f])))))])

;; more convenient constructor
(define (make-assignment #:id      id
                         #:name    [name null]
                         #:calls   [calls (current-calls)]
                         #:expr    expr
                         #:val     val
                         #:sampler [sampler (const val)]
                         #:context [context 'body]
                         #:note    [note null])
  (assignment id name calls expr val sampler context note))

;; shorter names for things...
(define (id      v) (assignment-id      v))
(define (name    v) (assignment-name    v))
(define (calls   v) (assignment-calls   v))
(define (expr    v) (assignment-expr    v))
(define (val     v) (assignment-val     v))
(define (sampler v) (assignment-sampler v))
(define (context v) (assignment-context v))
(define (note    v) (assignment-note    v))

(define (stack-top-val s) (val (stack-top s)))


;; Pretty print stacks
;;
(define (vector-format a)
  (let ((format-elt (lambda (x)
                      (if (rational? x)
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
         (expr-fmt    (map (compose (λ (e) (let ((e* (if (pair? e) (cons (caar e) (cdr e)) e))) (~a e*))) expr) stack))
         (calls-width (apply max (map string-length calls-fmt)))
         (name-width  (apply max (map string-length name-fmt)))
         (id-width    (apply max (map string-length id-fmt)))
         (val-width   (apply max (map string-length val-fmt)))
         (expr-width  (apply max (map string-length expr-fmt))))
    (display
     (foldl (lambda (n k v e acc)
              (string-append
               (format "~a ~a | ~a | ~a~%"
                       ;(~a c #:min-width calls-width)
                       (~a n #:min-width name-width)
                       (~a k #:min-width id-width)
                       (~a e #:min-width expr-width)
                       (~a v #:min-width val-width))
               acc))
            "" name-fmt id-fmt val-fmt expr-fmt))))
