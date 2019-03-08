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
  (let ((expected
         (list
          (assignment '%sum0 '(result) '() '((+ () ()) %a0 %e1) 101 'body)
          (assignment '%e1 '(c) '() 1 1 'body)
          (assignment '%a0 '() '() '%b0 100 'result)
          (assignment '%b0 '() '((a . %a0)) '%e2 100 'result)
          (assignment '%e2 '(c) '((a . %a0)) '%arg0 100 'body)
          (assignment '%arg0 '(x) '((a . %a0)) '%e0 100 'arg)
          (assignment '%e0 '(z) '() 100 100 'body))))
    (check-equal?
     result
     expected)))
