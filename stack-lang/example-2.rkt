#lang racket

(module example "nocell.rkt"
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

(require "main.rkt"
         rackunit
         'example)

(module+ test
  (define expected
    (list
     (assignment '%dcf0 '(result) '() '%sum3 5881.700118063753)
     (assignment
      '%sum3
      '()
      '((dcf . %dcf0))
      '((+ () ()) %vsum0 %n3)
      5881.700118063753)
     (assignment
      '%n3
      '()
      '((dcf . %dcf0))
      '((nth () (4)) %diff4 %quot2)
      5527.530320918749)
     (assignment
      '%quot2
      '(discounted-terminal-value)
      '((dcf . %dcf0))
      '((/ () (4)) %quot1 %expn1)
      '#(7357.142857142857 6688.311688311687 6080.2833530106245 5527.530320918749))
     (assignment
      '%expn1
      '()
      '((dcf . %dcf0))
      '((expt () (4)) %sum2 %diff3)
      '#(1 1.1 1.2100000000000002 1.3310000000000004))
     (assignment '%diff3 '() '((dcf . %dcf0)) '((- (4) ()) %e1 %n2) '#(0 1 2 3))
     (assignment '%n2 '() '((dcf . %dcf0)) '((nth () (4)) %e18 %e1) 2010)
     (assignment '%e18 '() '((dcf . %dcf0)) 0 0)
     (assignment '%sum2 '() '((dcf . %dcf0)) '((+ () ()) %e17 %e7) 1.1)
     (assignment '%e17 '() '((dcf . %dcf0)) 1 1)
     (assignment
      '%quot1
      '(terminal-value)
      '((dcf . %dcf0))
      '((/ () ()) %prod0 %diff2)
      7357.142857142857)
     (assignment '%diff2 '() '((dcf . %dcf0)) '((- () ()) %e7 %e5) 0.07)
     (assignment '%prod0 '() '((dcf . %dcf0)) '((* () ()) %n0 %sum1) 515.0)
     (assignment '%sum1 '() '((dcf . %dcf0)) '((+ () ()) %e5 %e15) 1.03)
     (assignment '%e15 '() '((dcf . %dcf0)) 1 1)
     (assignment
      '%n0
      '(final-year-cash-flow)
      '((dcf . %dcf0))
      '((nth () (4)) %diff0 %e3)
      500)
     (assignment '%diff0 '() '((dcf . %dcf0)) '((- () ()) %l0 %e10) 3)
     (assignment '%e10 '() '((dcf . %dcf0)) 1 1)
     (assignment '%diff4 '() '((dcf . %dcf0)) '((- () ()) %l0 %e20) 3)
     (assignment '%e20 '() '((dcf . %dcf0)) 1 1)
     (assignment '%l0 '(m) '((dcf . %dcf0)) '((len (4)) %e3) 4)
     (assignment
      '%vsum0
      '()
      '((dcf . %dcf0))
      '((sum (4)) %quot0)
      354.16979714500366)
     (assignment
      '%quot0
      '(discounted-cash-flow)
      '((dcf . %dcf0))
      '((/ (4) (4)) %e3 %expn0)
      '#(-100 -45.45454545454545 123.96694214876031 375.65740045078877))
     (assignment
      '%expn0
      '()
      '((dcf . %dcf0))
      '((expt () (4)) %sum0 %diff1)
      '#(1 1.1 1.2100000000000002 1.3310000000000004))
     (assignment '%diff1 '() '((dcf . %dcf0)) '((- (4) ()) %e1 %n1) '#(0 1 2 3))
     (assignment '%n1 '() '((dcf . %dcf0)) '((nth () (4)) %e13 %e1) 2010)
     (assignment '%e13 '() '((dcf . %dcf0)) 0 0)
     (assignment '%sum0 '() '((dcf . %dcf0)) '((+ () ()) %e12 %e7) 1.1)
     (assignment '%e12 '() '((dcf . %dcf0)) 1 1)
     (assignment '%e7 '(discount-rate) '() 0.1 0.1)
     (assignment '%e5 '(terminal-growth) '() 0.03 0.03)
     (assignment '%e3 '(cash-flow) '() '#(-100 -50 150 500) '#(-100 -50 150 500))
     (assignment
      '%e1
      '(years)
      '()
      '#(2010 2011 2012 2013)
      '#(2010 2011 2012 2013))))

  (let ((epsilon 1e-10))
    (check-within result expected epsilon)))
