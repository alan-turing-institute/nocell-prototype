#lang racket

(require racket/syntax)

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

(define (stack-top-val s) (val (stack-top s)))

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
