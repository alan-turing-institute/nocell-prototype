#lang racket

(module example "nocell.rkt"
  (define (a x y) (* x y))
  (define (b x) (* x x))
  (define z 0)
  (define w 1)
  (define result (a (b w) (+ z 0)))
  (provide result))

(module example2 "nocell.rkt"
  (define (a x) (* x x))
  (define (my-number) 5)
  (define result2 (a (my-number)))
  (provide result2))

(module example3 "nocell.rkt"
  (define (a) 4)
  (define result3 (a))
  (provide result3))

(module example4 "nocell.rkt"
  (define (f x) (define r x) r)
  (define result4 (f (f 5)))
  (provide result4))

(module example5 "nocell.rkt"
  (define (f x) (define a x)
    (define (f y) (define b y) b)
    (f a))
  (define result5 (f 1))
  (provide result5))

(require "main.rkt"
         rackunit
         'example
         'example2
         'example3
         'example4
         'example5)

(provide result5)
