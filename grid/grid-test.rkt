#lang racket/base

(module+ test
         (require rackunit "grid.rkt")

         (test-case "This is where tests will go"
                    (check-equal? (+ 1 1) 2 "1+1=2")
                    (check-equal? (+ 1 2) 2 "1+2=2")
                    )
         )



