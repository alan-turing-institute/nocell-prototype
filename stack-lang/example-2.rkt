#lang racket

(module example "main.rkt"
  (define years     #(2010 2011 2012 2013))
  (define cash-flow #(-100  -50  150  500))
  (define terminal-growth 0.03)
  (define discount-rate 0.1)

  (define (dcf years cash-flow terminal-growth discount-rate)
    (define m (len cash-flow))
    (define final-year-cash-flow (nth (- m 1) cash-flow))
    
    (define discounted-cash-flow
      (/ cash-flow
         (expt (+ 1 discount-rate) (- years (nth 0 years)))))
    
    (define terminal-value
      (/ (* final-year-cash-flow (+ terminal-growth 1))
         (- discount-rate terminal-growth)))
    
    (define discounted-terminal-value
      (/ terminal-value
         (expt (+ 1 discount-rate) (- years (nth 0 years)))))

    (+ (sum discounted-cash-flow)
       (nth (- m 1) discounted-terminal-value)))

  (define result (dcf years cash-flow terminal-growth discount-rate))

  (provide result))

(require rackunit)
(require 'example)

(module+ test
  (define expected
    '((result () (+ %vsum0 %n3) 5881.700118063753)
      (%n3 (dcf) (nth %diff4 discounted-terminal-value) 5527.530320918749)
      (discounted-terminal-value
       (dcf)
       (/ terminal-value %expn1)
       #(7357.142857142857 6688.311688311687 6080.2833530106245 5527.530320918749))
      (%expn1
       (dcf)
       (expt %sum2 %diff3)
       #(1 1.1 1.2100000000000002 1.3310000000000004))
      (%diff3 (dcf) (- years %n2) #(0 1 2 3))
      (%n2 (dcf) (nth %e10 years) 2010)
      (%e10 (dcf) 0 0)
      (%sum2 (dcf) (+ %e9 discount-rate) 1.1)
      (%e9 (dcf) 1 1)
      (terminal-value (dcf) (/ %prod0 %diff2) 7357.142857142857)
      (%diff2 (dcf) (- discount-rate terminal-growth) 0.07)
      (%prod0 (dcf) (* final-year-cash-flow %sum1) 515.0)
      (%sum1 (dcf) (+ terminal-growth %e8) 1.03)
      (%e8 (dcf) 1 1)
      (final-year-cash-flow (dcf) (nth %diff0 cash-flow) 500)
      (%diff0 (dcf) (- m %e5) 3)
      (%e5 (dcf) 1 1)
      (%diff4 (dcf) (- m %e11) 3)
      (%e11 (dcf) 1 1)
      (m (dcf) (len cash-flow) 4)
      (%vsum0 (dcf) (sum discounted-cash-flow) 354.16979714500366)
      (discounted-cash-flow
       (dcf)
       (/ cash-flow %expn0)
       #(-100 -45.45454545454545 123.96694214876031 375.65740045078877))
      (%expn0
       (dcf)
       (expt %sum0 %diff1)
       #(1 1.1 1.2100000000000002 1.3310000000000004))
      (%diff1 (dcf) (- years %n1) #(0 1 2 3))
      (%n1 (dcf) (nth %e7 years) 2010)
      (%e7 (dcf) 0 0)
      (%sum0 (dcf) (+ %e6 discount-rate) 1.1)
      (%e6 (dcf) 1 1)
      (discount-rate () 0.1 0.1)
      (terminal-growth () 0.03 0.03)
      (cash-flow () #(-100 -50 150 500) #(-100 -50 150 500))
      (years () #(2010 2011 2012 2013) #(2010 2011 2012 2013))))

  (let ((epsilon 1e-10))
    (check-within result expected epsilon)))
