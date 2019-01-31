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
                    (check-stack '() '((0 "result" ())) '())
                    )

         (test-case "Atom check - an atom just gets dumped"
                    (check-stack '(1) '((0 "result" 1)) 1)
                    )

         (test-case "List check - a list just gets dumped"
                    (check-stack '((+ 1 1)) '((0 "result" (+ 1 1))) 2)
                    )

         (test-case "Defining a variable - gets dumped and then correctly referenced"
                    (check-stack '((let ([a 1]) (+ 1 a))) '(
                                                            (0 a 1)
                                                            (1 "result" (+ 1 (row 0)))
                                                            ) 2)
                    )

         (test-case "Multiple variable check - gets dumped and then correctly referenced"
                    (check-stack '((let ([a 1] [b 2]) (+ b a))) '(
                                                                  (0 a 1)
                                                                  (1 b 2)
                                                                  (2 "result" (+ (row 1) (row 0)))
                                                                  ) 3)
                    )

         (test-case "Lambda check - gets dumped as its parameters dumped, then the function, then reference"
                    (check-stack '((let ([a 1] [b (lambda (q) (/ (+ q 9) 2))]) (+ (b 5) a))) '(
                                                                                               (0 a 1)
                                                                                               (1 q 5)
                                                                                               (2 b (/ (+ (row 1) 9) 2))
                                                                                               (3 "result" (+ (row 2) (row 0)))
                                                                                               ) 8)
                    )

         (test-case "Body check - should be ok to have multiple expressions in a body, though earlier ones are only interesting for their side effects (such as definitions, below)"
                    (check-stack '((let ([a 1]) (+ 2 2) (+ 1 a))) '(
                                                                    (0 a 1)
                                                                    (1 "" (+ 2 2))
                                                                    (2 "result" (+ 1 (row 0)))
                                                                    ) 2)
                    )

         (test-case "define variable check - should be ok to have defines at the top level"
                    (check-stack '((define c 9) (let ([a 1]) (+ c a))) '(
                                                                         (0 c 9)
                                                                         (1 a 1)
                                                                         (2 "result" (+ (row 0) (row 1)))
                                                                         ) 10)
                    )

         (test-case "define variable check - should be ok to have defines at the lower levels, that shadow the higher"
                    (check-stack '((define c 9) (let ([a 1]) (define c 10) (+ c a))) '(
                                                                                       (0 c 9)
                                                                                       (1 a 1)
                                                                                       (2 c 10)
                                                                                       (3 "result" (+ (row 2) (row 1)))
                                                                                       ) 11)
                    )

         (test-case "Defined function check - gets dumped as its parameters dumped, then the function, then reference"
                    (check-stack '((define (b q) (/ (+ q 9) 2)) (let ([a 1]) (+ (b 5) a))) '(
                                                                                             (0 a 1)
                                                                                             (1 q 5)
                                                                                             (2 b (/ (+ (row 1) 9) 2))
                                                                                             (3 "result" (+ (row 2) (row 0)))
                                                                                             ) 8)
                    )

         (test-case "Defined nested function check - gets dumped as its parameters dumped, then the function, then reference"
                    (check-stack '((define (b q) (define (r s) (/ s 2)) (r (+ q 9))) (let ([a 1]) (+ (b 5) a))) '((0 a 1)
                                                                                                                  (1 q 5)
                                                                                                                  (2 s (+ (row 1) 9))
                                                                                                                  (3 r (/ (row 2) 2))
                                                                                                                  (4 b (row 3))
                                                                                                                  (5 "result" (+ (row 4) (row 0)))) 8)
                    )

         (test-case "Lambdas as function arguments"
                    (check-stack '((let ([list (1 2 3)]) ((some-function (lambda (a) (+ 1 a)) list)))) '(
                                                                                                         (0 list (1 2 3))
                                                                                                         (1 "result" ((some-function (lambda (a) (+ 1 a)) list)))
                                                                                                         ) 'no-value)
                    )

         (test-case "function names as function arguments"
                    (check-stack '((define (add1 a) (+ 1 a)) (let ([list (1 2 3)]) (some-function add1 list))) '(
                                                                                                                 (0 list (1 2 3))
                                                                                                                 (1 "result" (some-function (lambda (a) (+ 1 a)) (row 0)))
                                                                                                                 ) 'no-value)
                    )

         (test-case "unwinding a map"
                    (check-stack '((map (lambda (a) (+ 1 a)) (1 2 3)))  '((0 a (col 0 (1 2 3)))
                                                                          (1 "" (+ 1 (row 0)))
                                                                          (2 a (col 1 (1 2 3)))
                                                                          (3 "" (+ 1 (row 2)))
                                                                          (4 a (col 2 (1 2 3)))
                                                                          (5 "" (+ 1 (row 4)))
                                                                          (6 "result" ((row 1) (row 3) (row 5)))) `(2 3 4))
                    )

         (test-case "unwinding a foldl"
                    (check-stack '((foldl (lambda (x accumulator) (+ accumulator x)) 5 (1 2 3))) '((0 x (col 0 (1 2 3)))
                                                                                                   (1 accumulator 5)
                                                                                                   (2 "" (+ (row 1) (row 0)))
                                                                                                   (3 x (col 1 (1 2 3)))
                                                                                                   (4 accumulator (row 2))
                                                                                                   (5 "" (+ (row 4) (row 3)))
                                                                                                   (6 x (col 2 (1 2 3)))
                                                                                                   (7 accumulator (row 5))
                                                                                                   (8 "" (+ (row 7) (row 6)))
                                                                                                   (9 "result" (row 8))) 11) 
                    )

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
                                 (dcf-value '(2020 2021 2022 2023) '(-100 -50 150 500) 0.03 0.1)))

                   (define dcf-expected   '((0 years '(2020 2021 2022 2023))
                                            (1 cashflows '(-100 -50 150 500))
                                            (2 terminal-growth 0.03)
                                            (3 discount-rate 0.1)
                                            (4 year (col 0 (row 0)))
                                            (5 cashflow (col 0 (row 1)))
                                            (6 year (row 4))
                                            (7 value (row 5))
                                            (8 discount (/ (row 7) (expt (+ 1 (row 3)) (- (row 6) (first (row 0))))))
                                            (9 "" (row 8))
                                            (10 year (col 1 (row 0)))
                                            (11 cashflow (col 1 (row 1)))
                                            (12 year (row 10))
                                            (13 value (row 11))
                                            (14 discount (/ (row 13) (expt (+ 1 (row 3)) (- (row 12) (first (row 0))))))
                                            (15 "" (row 14))
                                            (16 year (col 2 (row 0)))
                                            (17 cashflow (col 2 (row 1)))
                                            (18 year (row 16))
                                            (19 value (row 17))
                                            (20 discount (/ (row 19) (expt (+ 1 (row 3)) (- (row 18) (first (row 0))))))
                                            (21 "" (row 20))
                                            (22 year (col 3 (row 0)))
                                            (23 cashflow (col 3 (row 1)))
                                            (24 year (row 22))
                                            (25 value (row 23))
                                            (26 discount (/ (row 25) (expt (+ 1 (row 3)) (- (row 24) (first (row 0))))))
                                            (27 "" (row 26))
                                            (28 discounted-cashflows ((row 9) (row 15) (row 21) (row 27)))
                                            (29 discounted-cashflows (row 28))
                                            (30 year (last (row 0)))
                                            (31 terminal-value (/ (* (last (row 1)) (+ 1 (row 2))) (- (row 3) (row 2))))
                                            (32 value (row 31))
                                            (33 discount (/ (row 32) (expt (+ 1 (row 3)) (- (row 30) (first (row 0))))))
                                            (34 discounted-terminal-value (row 33))
                                            (35 t (col 0 (row 29)))
                                            (36 c 0)
                                            (37 "" (+ (row 35) (row 36)))
                                            (38 t (col 1 (row 29)))
                                            (39 c (row 37))
                                            (40 "" (+ (row 38) (row 39)))
                                            (41 t (col 2 (row 29)))
                                            (42 c (row 40))
                                            (43 "" (+ (row 41) (row 42)))
                                            (44 t (col 3 (row 29)))
                                            (45 c (row 43))
                                            (46 "" (+ (row 44) (row 45)))
                                            (47 total (+ (row 46) (row 34)))
                                            (48 dcf-value (row 47))
                                            (49 "result" (row 48))))

                   (check-stack dcf dcf-expected 5881.700118063753)
                   )
         )

