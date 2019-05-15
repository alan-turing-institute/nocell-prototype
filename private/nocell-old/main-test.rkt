#lang racket

(module+ test
  (require rackunit "main.rkt")

  (define (check-stack actual-nocell expected-stack expected-value)
    (let ([stack (nocell->stack actual-nocell)])
      (check-equal? stack expected-stack)
      (unless (equal? expected-value 'no-value) 
        (check-equal? (stack->value stack) expected-value))
      )
    )

  (test-case "Safety check - an emtpy nocell creates an empty result"
             (check-stack '() '((0 "result" () (output) ())) '())
             )

  (test-case "Atom check - an atom just gets dumped"
             (check-stack '(1) '((0 "result" 1 (output) ())) 1)
             )

  (test-case "List check - a list just gets dumped"
             (check-stack '((+ 1 1)) '((0 "result" (+ 1 1) (output) ())) 2)
             )

  (test-case "Defining a variable - gets dumped and then correctly referenced"
             (check-stack '((let ([a 1]) (+ 1 a))) '(
                                                     (0 a 1 (last-parameter) ())
                                                     (1 "result" (+ 1 (row 0)) (output) ())
                                                     ) 2)
             )

  (test-case "Multiple variable check - gets dumped and then correctly referenced"
             (check-stack '((let ([a 1] [b 2]) (+ b a))) '(
                                                           (0 a 1 (parameter) ())
                                                           (1 b 2 (last-parameter) ())
                                                           (2 "result" (+ (row 1) (row 0)) (output) ())
                                                           ) 3)
             )

  (test-case "Lambda check - gets dumped as its parameters dumped, then the function, then reference"
             (check-stack '((let ([a 1] [b (lambda (q) (/ (+ q 9) 2))]) (+ (b 5) a))) '(
                                                                                        (0 a 1 (last-parameter) ())
                                                                                        (1 q 5 (last-parameter) (b))
                                                                                        (2 b (/ (+ (row 1) 9) 2) (return) (b) )
                                                                                        (3 "result" (+ (row 2) (row 0)) (output) () )
                                                                                        ) 8)
             )

  (test-case "Body check - should be ok to have multiple expressions in a body, though earlier ones are only interesting for their side effects (such as definitions, below)"
             (check-stack '((let ([a 1]) (+ 2 2) (+ 1 a))) '(
                                                             (0 a 1 (last-parameter) ())
                                                             (1 "" (+ 2 2) () ())
                                                             (2 "result" (+ 1 (row 0)) (output) ())
                                                             ) 2)
             )

  (test-case "define variable check - should be ok to have defines at the top level"
             (check-stack '((define c 9) (let ([a 1]) (+ c a))) '(
                                                                  (0 c 9 () ())
                                                                  (1 a 1 (last-parameter) ())
                                                                  (2 "result" (+ (row 0) (row 1)) (output) ())
                                                                  ) 10)
             )

  (test-case "define variable check - should be ok to have defines at the lower levels, that shadow the higher"
             (check-stack '((define c 9) (let ([a 1]) (define c 10) (+ c a))) '(
                                                                                (0 c 9 () ())
                                                                                (1 a 1 (last-parameter) ())
                                                                                (2 c 10 () ())
                                                                                (3 "result" (+ (row 2) (row 1)) (output) () )
                                                                                ) 11)
             )

  (test-case "Defined function check - gets dumped as its parameters dumped, then the function, then reference"
             (check-stack '((define (b q) (/ (+ q 9) 2)) (let ([a 1]) (+ (b 5) a))) '(
                                                                                      (0 a 1 (last-parameter) ())
                                                                                      (1 q 5 (last-parameter) (b))
                                                                                      (2 b (/ (+ (row 1) 9) 2) (return) (b))
                                                                                      (3 "result" (+ (row 2) (row 0)) (output) () )
                                                                                      ) 8)
             )

  (test-case "Defined nested function check - gets dumped as its parameters dumped, then the function, then reference"
             (check-stack '((define (b q) (define (r s) (/ s 2)) (r (+ q 9))) (let ([a 1]) (+ (b 5) a))) '((0 a 1 (last-parameter) ())
                                                                                                           (1 q 5 (last-parameter) (b))
                                                                                                           (2 s (+ (row 1) 9) (last-parameter) (b r))
                                                                                                           (3 r (/ (row 2) 2) (return) (b r) )
                                                                                                           (4 b (row 3) (return) (b) )
                                                                                                           (5 "result" (+ (row 4) (row 0)) (output) () )) 8)
             )

  (test-case "Lambdas as function arguments"
             (check-stack '((let ([list (1 2 3)]) ((some-function (lambda (a) (+ 1 a)) list)))) '(
                                                                                                  (0 list (1 2 3) (last-parameter) ())
                                                                                                  (1 "result" ((some-function (lambda (a) (+ 1 a)) list)) (output) ())
                                                                                                  ) 'no-value)
             )

  (test-case "function names as function arguments"
             (check-stack '((define (add1 a) (+ 1 a)) (let ([list (1 2 3)]) (some-function add1 list))) '(
                                                                                                          (0 list (1 2 3) (last-parameter) ())
                                                                                                          (1 "result" (some-function (lambda (a) (+ 1 a)) (row 0)) (output) ())
                                                                                                          ) 'no-value)
             )

  (test-case "unwinding a map"
             (check-stack '((map (lambda (a) (+ 1 a)) (1 2 3))) '((0 a ((col 0 (1 2 3)) (col 1 (1 2 3)) (col 2 (1 2 3))) (last-parameter) () )
                                                                  (1 "" ((+ 1 (col 0 (row 0))) (+ 1 (col 1 (row 0))) (+ 1 (col 2 (row 0)))) (return) ())
                                                                  (2 "result" ((col 0 (row 1)) (col 1 (row 1)) (col 2 (row 1))) (output) () ))  `(2 3 4))
             )

  (test-case "unwinding a foldl"
             (check-stack '((foldl (lambda (x accumulator) (+ accumulator x)) 5 (1 2 3))) '(
                                                                                            (0 x ((col 0 (1 2 3)) (col 1 (1 2 3)) (col 2 (1 2 3))) (parameter) () )
                                                                                            (1 accumulator (5 (col 0 (row 2)) (col 1 (row 2))) (last-parameter) ()) 
                                                                                            (2 "" (
                                                                                                   (+ (col 0 (row 1)) (col 0 (row 0))) 
                                                                                                   (+ (col 1 (row 1)) (col 1 (row 0)))
                                                                                                   (+ (col 2 (row 1)) (col 2 (row 0)))  
                                                                                                   ) (return) ())
                                                                                            (3 "result" (col 2 (row 2)) (output) ())
                                                                                            )  11) ) 

  (test-case "A discounted cash flow model"
             (define dcf '((define (dcf-value years cashflows terminal-growth discount-rate)
                             (define (total discounted-cashflows discounted-terminal-value)
                               (+ (foldl (lambda (t c) (+ t c)) 0 discounted-cashflows) discounted-terminal-value) 
                               )
                             (define (terminal-value)
                               (/ (* (last cashflows) (+ 1 terminal-growth)) (- discount-rate terminal-growth))
                               )
                             (define (discount year value)
                               (/ value (expt (+ 1 discount-rate) (- year (first years))))
                               )
                             (let ([discounted-cashflows (map (lambda (year cashflow) (discount year cashflow)) years cashflows)])
                               (total discounted-cashflows (discount (last years) (terminal-value)))
                               )
                             )
                           (dcf-value (2020 2021 2022 2023) (-100 -50 150 500) 0.03 0.1)))

             (define dcf-expected  
               '((0 years (2020 2021 2022 2023) (parameter) (dcf-value))
                 (1 cashflows (-100 -50 150 500) (parameter) (dcf-value))
                 (2 terminal-growth 0.03 (parameter) (dcf-value))
                 (3 discount-rate 0.1 (last-parameter) (dcf-value))
                 (4
                  year
                  ((col 0 (row 0)) (col 1 (row 0)) (col 2 (row 0)) (col 3 (row 0)))
                  (parameter)
                  (dcf-value))
                 (5
                  cashflow
                  ((col 0 (row 1)) (col 1 (row 1)) (col 2 (row 1)) (col 3 (row 1)))
                  (last-parameter)
                  (dcf-value))
                 (6
                  year
                  ((col 0 (row 4)) (col 1 (row 4)) (col 2 (row 4)) (col 3 (row 4)))
                  (parameter)
                  (dcf-value discount))
                 (7
                  value
                  ((col 0 (row 5)) (col 1 (row 5)) (col 2 (row 5)) (col 3 (row 5)))
                  (last-parameter)
                  (dcf-value discount))
                 (8
                  discount
                  ((/
                     (col 0 (row 7))
                     (expt (+ 1 (row 3)) (- (col 0 (row 6)) (col 0 (row 0)))))
                   (/
                     (col 1 (row 7))
                     (expt (+ 1 (row 3)) (- (col 1 (row 6)) (col 0 (row 0)))))
                   (/
                     (col 2 (row 7))
                     (expt (+ 1 (row 3)) (- (col 2 (row 6)) (col 0 (row 0)))))
                   (/
                     (col 3 (row 7))
                     (expt (+ 1 (row 3)) (- (col 3 (row 6)) (col 0 (row 0))))))
                  (return)
                  (dcf-value discount))
                 (9
                  ""
                  ((col 0 (row 8)) (col 1 (row 8)) (col 2 (row 8)) (col 3 (row 8)))
                  (return)
                  (dcf-value))
                 (10
                  discounted-cashflows
                  ((col 0 (row 9)) (col 1 (row 9)) (col 2 (row 9)) (col 3 (row 9)))
                  (last-parameter)
                  (dcf-value))
                 (11 discounted-cashflows (row 10) (parameter) (dcf-value total))
                 (12 year (col 3 (row 0)) (last-parameter) (dcf-value total discount))
                 (13
                  terminal-value
                  (/ (* (col 3 (row 1)) (+ 1 (row 2))) (- (row 3) (row 2)))
                  (return)
                  (dcf-value total discount terminal-value))
                 (14 value (row 13) (last-parameter) (dcf-value total discount))
                 (15
                  discount
                  (/ (row 14) (expt (+ 1 (row 3)) (- (row 12) (col 0 (row 0)))))
                  (return)
                  (dcf-value total discount))
                 (16 discounted-terminal-value (row 15) (last-parameter) (dcf-value total))
                 (17
                  t
                  ((col 0 (row 11)) (col 1 (row 11)) (col 2 (row 11)) (col 3 (row 11)))
                  (parameter)
                  (dcf-value total))
                 (18
                  c
                  (0 (col 0 (row 19)) (col 1 (row 19)) (col 2 (row 19)))
                  (last-parameter)
                  (dcf-value total))
                 (19
                  ""
                  ((+ (col 0 (row 17)) (col 0 (row 18)))
                   (+ (col 1 (row 17)) (col 1 (row 18)))
                   (+ (col 2 (row 17)) (col 2 (row 18)))
                   (+ (col 3 (row 17)) (col 3 (row 18))))
                  (return)
                  (dcf-value total))
                 (20 total (+ (col 3 (row 19)) (row 16)) (return) (dcf-value total))
                 (21 dcf-value (row 20) (return) (dcf-value))
                 (22 "result" (row 21) (output) ()))

               )

             (check-stack dcf dcf-expected 5881.700118063753)
             )
  )

