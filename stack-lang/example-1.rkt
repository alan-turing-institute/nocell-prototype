#lang racket

(module example "nocell.rkt"
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

(require "main.rkt"
         rackunit
         'example)

(module+ test
  (let ((expected
         (list
          (assignment '%a0 '(result) '() '%sum3 103)
          (assignment '%sum3 '() '((a . %a0)) '((+ () ()) %b0 %e6) 103)
          (assignment '%e6 '() '((a . %a0)) 0 0)
          (assignment '%b0 '(tmp) '((a . %a0)) '%sum2 103)
          (assignment '%sum2 '() '((b . %b0) (a . %a0)) '((+ () ()) %sum0 %sum1) 103)
          (assignment '%sum1 '() '((b . %b0) (a . %a0)) '((+ () ()) %e1 %e3) 101)
          (assignment '%e3 '(c) '((b . %b0) (a . %a0)) 1 1)
          (assignment '%e1 '(z) '((a . %a0)) 100 100)
          (assignment '%sum0 '() '() '((+ () ()) %e7 %e9) 2)
          (assignment '%e9 '() '() 2 2)
          (assignment '%e7 '(z) '() 0 0))))
    (check-equal?
     result
     expected)))
