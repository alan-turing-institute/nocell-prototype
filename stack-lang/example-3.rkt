#lang racket

(module example "nocell.rkt"
  (define z 100)
  (define (a)
    (define (b x)
      (define c x)
      c)
    (b z))
  (define c 1)
  (define result (+ (a) c))
  (provide result))

(require "main.rkt"
         rackunit
         'example)

(module+ test
  (let ((expected (list
                   (assignment '%sum0 '(result) '() '((+ () ()) %a0 %e1) 101)
                   (assignment '%e1 '(c) '() 1 1)
                   (assignment '%a0 '() '() '%b0 100)
                   (assignment '%b0 '() '((a . %a0)) '%e0 100)
                   (assignment '%e0 '(z) '() 100 100))))
    (check-equal?
     result
     expected)))
