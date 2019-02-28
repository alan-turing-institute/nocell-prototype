#lang racket

(module example "../../nocell.rkt"
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

(module+ test
  (require "../../cell.rkt"
           rackunit
           (submod ".." example))
  (define expected
    (list
     (assignment '%dcf0 '(result) '() '%sum3 5881.700118063753)
     (assignment
      '%sum3
      '()
      '((dcf . %dcf0))
      '((+ () ()) %vsum3 %vsum4)
      5881.700118063753)
     (assignment
      '%vsum4
      '()
      '((dcf . %dcf0))
      '((sum (4)) %prod4)
      5527.530320918749)
     (assignment
      '%prod4
      '()
      '((dcf . %dcf0))
      '((* (4) (4)) %select3 %quot2)
      '#(0 0 0 5527.530320918749))
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
     (assignment '%diff3 '() '((dcf . %dcf0)) '((- (4) ()) %e1 %vsum2) '#(0 1 2 3))
     (assignment '%vsum2 '() '((dcf . %dcf0)) '((sum (4)) %prod3) 2010)
     (assignment
      '%prod3
      '()
      '((dcf . %dcf0))
      '((* (4) (4)) %select2 %e1)
      '#(2010 0 0 0))
     (assignment '%select2 '() '((dcf . %dcf0)) '#(1 0 0 0) '#(1 0 0 0))
     (assignment '%sum2 '() '((dcf . %dcf0)) '((+ () ()) %e17 %e7) 1.1)
     (assignment '%e17 '() '((dcf . %dcf0)) 1 1)
     (assignment
      '%quot1
      '(terminal-value)
      '((dcf . %dcf0))
      '((/ () ()) %prod2 %diff2)
      7357.142857142857)
     (assignment '%diff2 '() '((dcf . %dcf0)) '((- () ()) %e7 %e5) 0.07)
     (assignment '%prod2 '() '((dcf . %dcf0)) '((* () ()) %vsum0 %sum1) 515.0)
     (assignment '%sum1 '() '((dcf . %dcf0)) '((+ () ()) %e5 %e15) 1.03)
     (assignment '%e15 '() '((dcf . %dcf0)) 1 1)
     (assignment
      '%vsum0
      '(final-year-cash-flow)
      '((dcf . %dcf0))
      '((sum (4)) %prod0)
      500)
     (assignment
      '%prod0
      '()
      '((dcf . %dcf0))
      '((* (4) (4)) %select0 %e3)
      '#(0 0 0 500))
     (assignment '%select0 '() '((dcf . %dcf0)) '#(0 0 0 1) '#(0 0 0 1))
     (assignment '%select3 '() '((dcf . %dcf0)) '#(0 0 0 1) '#(0 0 0 1))
     (assignment
      '%vsum3
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
     (assignment '%diff1 '() '((dcf . %dcf0)) '((- (4) ()) %e1 %vsum1) '#(0 1 2 3))
     (assignment '%vsum1 '() '((dcf . %dcf0)) '((sum (4)) %prod1) 2010)
     (assignment
      '%prod1
      '()
      '((dcf . %dcf0))
      '((* (4) (4)) %select1 %e1)
      '#(2010 0 0 0))
     (assignment '%select1 '() '((dcf . %dcf0)) '#(1 0 0 0) '#(1 0 0 0))
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
