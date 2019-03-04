#lang racket

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(require racket/syntax
         (rename-in "stack.rkt"
                    [assignment-id    id]
                    [assignment-name  name]
                    [assignment-calls calls]
                    [assignment-expr  expr]
                    [assignment-val   val]))

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
