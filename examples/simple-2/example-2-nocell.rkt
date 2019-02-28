#lang racket

(module example "../../nocell.rkt"
  (define z 100)
  (define (a)
    (define (b x)
      (define c x)
      c)
    (b z))
  (define c 1)
  (define result (+ (a) c))
  (provide result))

(module+ test
  (require "../../cell.rkt"
           rackunit
           (submod ".." example))

  (let ((expected (list
                   (assignment '%sum0 '(result) '() '((+ () ()) %a0 %e4) 101)
                   (assignment '%e4 '(c) '() 1 1)
                   (assignment '%a0 '() '() '%b0 100)
                   (assignment '%b0 '() '((a . %a0)) '%e1 100)
                   (assignment '%e1 '(z) '() 100 100))))
    (check-equal?
     result
     expected)))
