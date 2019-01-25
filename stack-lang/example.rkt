#lang s-exp "main.rkt"

(define years     #(2010 2011 2012 2013))
(define cash-flow #(-100  -50  150  500))

(define terminal-growth 0.03)
(define discount-rate 0.1)

(define n (len cash-flow))
(define final-year-cash-flow (nth (- n 1) cash-flow))

(define discounted-cash-flow
  (/ cash-flow
     (expt (+ 1 discount-rate) (- years (nth 0 years)))))

(define terminal-value
  (/ (* final-year-cash-flow (+ terminal-growth 1))
     (- discount-rate terminal-growth)))

(define discounted-terminal-value
  (/ terminal-value
     (expt (+ 1 discount-rate) (- years (nth 0 years)))))

(define value
  (+ (sum discounted-cash-flow)
     (nth (- n 1) discounted-terminal-value)))

(stack-print value)
