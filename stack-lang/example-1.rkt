#lang racket

(module example "main.rkt"
  (define (a x)
    (define z 100)
    (define (b)
      (define c 1)
      (+ x (+ z c)))
    (define tmp (b))
    (+ tmp 0))
  (define z 0)
  (define result (a (+ z 2)))
  (provide result))

(require rackunit)
(require 'example)

(module+ test
  (let ((expected '((result () (+ tmp %e3) 103)
                    (%e3 (a) 0 0)
                    (tmp (a) (+ %sum0 %sum1) 103)
                    (%sum1 (b a) (+ z c) 101)
                    (c (b a) 1 1)
                    (z (a) 100 100)
                    (%sum0 () (+ z %e5) 2)
                    (%e5 () 2 2)
                    (z () 0 0))))
    (check-equal?
     result
     expected)))
