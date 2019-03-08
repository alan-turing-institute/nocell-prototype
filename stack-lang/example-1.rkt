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
          (assignment '%a0 '(result) '() '%sum3 103 'result)
          (assignment '%sum3 '() '((a . %a0)) '((+ () ()) %b0 %e4) 103 'body)
          (assignment '%e4 '() '((a . %a0)) 0 0 'body)
          (assignment '%b0 '(tmp) '((a . %a0)) '%sum2 103 'result)
          (assignment
           '%sum2
           '()
           '((b . %b0) (a . %a0))
           '((+ () ()) %arg0 %sum1)
           103
           'body)
          (assignment '%sum1 '() '((b . %b0) (a . %a0)) '((+ () ()) %e2 %e3) 101 'body)
          (assignment '%e3 '(c) '((b . %b0) (a . %a0)) 1 1 'body)
          (assignment '%e2 '(z) '((a . %a0)) 100 100 'body)
          (assignment '%arg0 '(x) '() '%sum0 2 'arg)
          (assignment '%sum0 '() '() '((+ () ()) %e0 %e1) 2 'body)
          (assignment '%e1 '() '() 2 2 'body)
          (assignment '%e0 '(z) '() 0 0 'body))))
    (check-equal?
     result
     expected)))
