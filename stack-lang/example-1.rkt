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
            (assignment '%sum3 '() '((a . %a0)) '((+ () ()) %b0 %e4) 103)
            (assignment '%e4 '() '((a . %a0)) 0 0)
            (assignment '%b0 '(tmp) '((a . %a0)) '%sum2 103)
            (assignment '%sum2 '() '((b . %b0) (a . %a0)) '((+ () ()) %sum0 %sum1) 103)
            (assignment '%sum1 '() '((b . %b0) (a . %a0)) '((+ () ()) %e2 %e3) 101)
            (assignment '%e3 '(c) '((b . %b0) (a . %a0)) 1 1)
            (assignment '%e2 '(z) '((a . %a0)) 100 100)
            (assignment '%sum0 '() '() '((+ () ()) %e0 %e1) 2)
            (assignment '%e1 '() '() 2 2)
            (assignment '%e0 '(z) '() 0 0))))
    (check-equal?
     result
     expected)))
