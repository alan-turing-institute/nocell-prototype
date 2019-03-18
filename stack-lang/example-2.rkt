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
   (assignment '%dcf0 '(result) '() '%sum3 5881.700118063753 'result)
   (assignment
    '%sum3
    '()
    '((dcf . %dcf0))
    '((+ () ()) %vsum3 %vsum4)
    5881.700118063753
    'body)
   (assignment
    '%vsum4
    '()
    '((dcf . %dcf0))
    '((sum (4)) %prod4)
    5527.530320918749
    'body)
   (assignment
    '%prod4
    '()
    '((dcf . %dcf0))
    '((* (4) (4)) %select3 %quot2)
    '#(0 0 0 5527.530320918749)
    'body)
   (assignment
    '%quot2
    '(discounted-terminal-value)
    '((dcf . %dcf0))
    '((/ () (4)) %quot1 %expn1)
    '#(7357.142857142857 6688.311688311687 6080.2833530106245 5527.530320918749)
    'body)
   (assignment
    '%expn1
    '()
    '((dcf . %dcf0))
    '((expt () (4)) %sum2 %diff3)
    '#(1 1.1 1.2100000000000002 1.3310000000000004)
    'body)
   (assignment
    '%diff3
    '()
    '((dcf . %dcf0))
    '((- (4) ()) %arg0 %vsum2)
    '#(0 1 2 3)
    'body)
   (assignment '%vsum2 '() '((dcf . %dcf0)) '((sum (4)) %prod3) 2010 'body)
   (assignment
    '%prod3
    '()
    '((dcf . %dcf0))
    '((* (4) (4)) %select2 %arg0)
    '#(2010 0 0 0)
    'body)
   (assignment '%select2 '() '((dcf . %dcf0)) '#(1 0 0 0) '#(1 0 0 0) 'body)
   (assignment '%sum2 '() '((dcf . %dcf0)) '((+ () ()) %e8 %arg3) 1.1 'body)
   (assignment '%e8 '() '((dcf . %dcf0)) 1 1 'body)
   (assignment
    '%quot1
    '(terminal-value)
    '((dcf . %dcf0))
    '((/ () ()) %prod2 %diff2)
    7357.142857142857
    'body)
   (assignment '%diff2 '() '((dcf . %dcf0)) '((- () ()) %arg3 %arg2) 0.07 'body)
   (assignment
    '%prod2
    '()
    '((dcf . %dcf0))
    '((* () ()) %vsum0 %sum1)
    515.0
    'body)
   (assignment '%sum1 '() '((dcf . %dcf0)) '((+ () ()) %arg2 %e7) 1.03 'body)
   (assignment '%e7 '() '((dcf . %dcf0)) 1 1 'body)
   (assignment
    '%vsum0
    '(final-year-cash-flow)
    '((dcf . %dcf0))
    '((sum (4)) %prod0)
    500
    'body)
   (assignment
    '%prod0
    '()
    '((dcf . %dcf0))
    '((* (4) (4)) %select0 %arg1)
    '#(0 0 0 500)
    'body)
   (assignment '%select0 '() '((dcf . %dcf0)) '#(0 0 0 1) '#(0 0 0 1) 'body)
   (assignment '%select3 '() '((dcf . %dcf0)) '#(0 0 0 1) '#(0 0 0 1) 'body)
   (assignment
    '%vsum3
    '()
    '((dcf . %dcf0))
    '((sum (4)) %quot0)
    354.16979714500366
    'body)
   (assignment
    '%quot0
    '(discounted-cash-flow)
    '((dcf . %dcf0))
    '((/ (4) (4)) %arg1 %expn0)
    '#(-100 -45.45454545454545 123.96694214876031 375.65740045078877)
    'body)
   (assignment
    '%expn0
    '()
    '((dcf . %dcf0))
    '((expt () (4)) %sum0 %diff1)
    '#(1 1.1 1.2100000000000002 1.3310000000000004)
    'body)
   (assignment
    '%diff1
    '()
    '((dcf . %dcf0))
    '((- (4) ()) %arg0 %vsum1)
    '#(0 1 2 3)
    'body)
   (assignment '%vsum1 '() '((dcf . %dcf0)) '((sum (4)) %prod1) 2010 'body)
   (assignment
    '%prod1
    '()
    '((dcf . %dcf0))
    '((* (4) (4)) %select1 %arg0)
    '#(2010 0 0 0)
    'body)
   (assignment '%select1 '() '((dcf . %dcf0)) '#(1 0 0 0) '#(1 0 0 0) 'body)
   (assignment '%sum0 '() '((dcf . %dcf0)) '((+ () ()) %e5 %arg3) 1.1 'body)
   (assignment '%e5 '() '((dcf . %dcf0)) 1 1 'body)
   (assignment '%arg3 '(discount-rate) '() '%e3 0.1 'arg)
   (assignment '%arg2 '(terminal-growth) '() '%e2 0.03 'arg)
   (assignment '%arg1 '(cash-flow) '() '%e1 '#(-100 -50 150 500) 'arg)
   (assignment '%arg0 '(years) '() '%e0 '#(2010 2011 2012 2013) 'arg)
   (assignment '%e3 '(discount-rate) '() 0.1 0.1 'body)
   (assignment '%e2 '(terminal-growth) '() 0.03 0.03 'body)
   (assignment
    '%e1
    '(cash-flow)
    '()
    '#(-100 -50 150 500)
    '#(-100 -50 150 500)
    'body)
   (assignment
    '%e0
    '(years)
    '()
    '#(2010 2011 2012 2013)
    '#(2010 2011 2012 2013)
    'body)))

  (let ((epsilon 1e-10))
    (check-within result expected epsilon)))
