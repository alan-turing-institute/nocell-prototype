#lang racket

(provide (all-defined-out))

;; Depth limit of function calls
(define current-max-branching-depth (make-parameter 8))

;; use a fixed random seed in the mh sampling (to make the output of
;; the sampler deterministic)
(define deterministic-sampler?      (make-parameter #t))

;; Metropolis-Hastings sampler properties
(define current-mh-burn-steps       (make-parameter 2000))
(define current-mh-sample-steps     (make-parameter 20000))

;; The call stack of nocell functions
(define current-calls               (make-parameter '()))
