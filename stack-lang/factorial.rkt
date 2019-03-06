#lang racket
(module factorial "nocell.rkt"
  (define (fact n)
    (if* (= n 0)
         1
         (* n (fact (- n 1)))))

  (define (fact* n acc)
    (if* (= n 0)
         acc
         (fact* (- n 1) (* n acc))))
  
  (define result (fact 5))
  (define result* (fact* 5 1))
  
  (provide result result*))

(require "main.rkt"
         'factorial)
