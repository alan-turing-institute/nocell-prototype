#lang racket

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(require racket/syntax
         "../cell/assignment.rkt"
         math/array)

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
  (reverse (remove-duplicates (reverse xs) #:key assignment-id)))

;; vector-broadcast : vector? -> vector?
;;
;; If the vector a has a single element, then extend it to a vector
;; with n copies of this element; otherwise return the original
;; vector.
;;
;; (define (vector-broadcast n a)
;;   (if (vector? a)
;;       a
;;       (build-vector n (const a))))

;; syntax-reverse : syntax? -> syntax?
;;
;; Giving a syntax object comprising a list, return a new syntax
;; object otherwise identical but with this list reversed.
(define-for-syntax (syntax-reverse stx)
  (datum->syntax stx (reverse (syntax->list stx))))

(define (vector-min v) (vector-argmin identity v))

(define (array-diag arr)
  (let* ([s (array-shape arr)]
         [d (vector-length s)]
         [n (vector-min s)])
    (build-array (vector n)
                 (λ (idx) (array-ref arr (make-vector d (vector-ref idx 0)))))))
