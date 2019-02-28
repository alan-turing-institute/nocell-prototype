#lang racket

(module+ test
         (require rackunit "main.rkt")

         (define (check-grid actual-stack expected-grid)
           (let ([grid (stack->grid actual-stack)])
             (check-equal? grid expected-grid)
             )
           )

         (test-case "Safety check - an empty stack creates an empty grid"
                    (check-grid '() '())
                    )

         (test-case "Empty result creates one cell with the label"
                    (check-grid 
                      '(
                        (0 label ())
                        ) 
                      '(
                        ("label")
                        )
                      )
                    )
          
         (test-case "Value result creates two cells, label and value"
                    (check-grid 
                      '(
                        (0 label 1)
                        ) 
                      '(
                        ("label" 1)
                        )
                      )
                    )

         (test-case "List of values result creates a row of cells"
                    (check-grid 
                      '(
                        (0 years (2020 2021 2022 2023))
                        (1 label 1)
                        ) 
                      '(
                        ("years" 2020 2021 2022 2023)
                        ("label" 1)
                        )
                      )
                    )

         (test-case "Function application creates a formula"
                    (check-grid 
                      '(
                        (0 simple (+ 1 1))
                        (1 complex (+ (+ 1 2) (+ 3 4)))
                        ) 
                      '(
                        ("simple" (2 . "(1+1)"))
                        ("complex" (10 . "((1+2)+(3+4))"))
                        )
                      )
                    )

         (test-case "Row of function applications creates a row of formulae"
                    (check-grid 
                      '(
                        (0 result ( (+ 1 1) (- 5 1) (* 6 5) (/ 24 3) ))
                        ) 
                      '(
                        ("result" (2 . "(1+1)") (4 . "(5-1)") (30 . "(6*5)") (8 . "(24/3)"))
                        )
                      )
                    )

         (test-case "col row references get turned into references"
                    (check-grid 
                      '(
                        (0 years (2020))
                        (1 short ((- (col 0 (row 0)) 2000)))
                        ) 
                      '(
                        ("years" 2020)
                        ("short" (20 . "([.B1]-2000)"))
                        )
                      )
                    )

        (test-case "row references get turned into range references"
            (check-grid 
              '(
                (0 years (10 20))
                (1 short (last (row 0)))
                ) 
              '(
                ("years" 10 20)
                ("short" (20 . "last([.B1:.C1])"))
                )
              )
            )

        (test-case "row references where row has one value get turned into cell not range references"
            (check-grid 
              '(
                (0 rate 10)
                (1 thing (+ (row 0) 10))
                ) 
              '(
                ("rate" 10)
                ("thing" (20 . "([.B1]+10)"))
                )
              )
            )

        (test-case "row references as only item on row get turned into a row of cell references"
            (check-grid 
              '(
                (0 years (10 20))
                (1 years (row 0))
                ) 
              '(
                ("years" 10 20)
                ("years" (10 . "[.B1]") (20 . "[.C1]"))
                )
              )
            )


         (test-case "Putting it all together for DCF model"
            (check-grid '(
                          (0 years (2020 2021 2022 2023))
                                            (1 cashflows (-100 -50 150 500))
                                            (2 terminal-growth 0.03)
                                            (3 discount-rate 0.1)
                                            (4 year ((col 0 (row 0)) (col 1 (row 0)) (col 2 (row 0)) (col 3 (row 0))))
                                            (5
                                             cashflow
                                             ((col 0 (row 1)) (col 1 (row 1)) (col 2 (row 1)) (col 3 (row 1))))
                                            (6 year ((col 0 (row 4)) (col 1 (row 4)) (col 2 (row 4)) (col 3 (row 4))))
                                            (7 value ((col 0 (row 5)) (col 1 (row 5)) (col 2 (row 5)) (col 3 (row 5))))
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
                                                (expt (+ 1 (row 3)) (- (col 3 (row 6)) (col 0 (row 0)))))))
                                            (9 "" ((col 0 (row 8)) (col 1 (row 8)) (col 2 (row 8)) (col 3 (row 8))))
                                            (10
                                             discounted-cashflows
                                             ((col 0 (row 9)) (col 1 (row 9)) (col 2 (row 9)) (col 3 (row 9))))
                                            (11 discounted-cashflows (row 10))
                                            (12 year (col 3 (row 0)))
                                            (13 terminal-value (/ (* (col 3 (row 1)) (+ 1 (row 2))) (- (row 3) (row 2))))
                                            (14 value (row 13))
                                            (15 discount (/ (row 14) (expt (+ 1 (row 3)) (- (row 12) (col 0 (row 0))))))
                                            (16 discounted-terminal-value (row 15))
                                            (17 t ((col 0 (row 11)) (col 1 (row 11)) (col 2 (row 11)) (col 3 (row 11))))
                                            (18 c (0 (col 0 (row 19)) (col 1 (row 19)) (col 2 (row 19))))
                                            (19
                                             ""
                                             ((+ (col 0 (row 17)) (col 0 (row 18)))
                                              (+ (col 1 (row 17)) (col 1 (row 18)))
                                              (+ (col 2 (row 17)) (col 2 (row 18)))
                                              (+ (col 3 (row 17)) (col 3 (row 18)))))
                                            (20 total (+ (col 3 (row 19)) (row 16)))
                                            (21 dcf-value (row 20))
                                            (22 "result" (row 21))
                                            )
              
                        '(("years" 2020 2021 2022 2023)
                              ("cashflows" -100 -50 150 500)
                                  ("terminal-growth" 0.03)
                                      ("discount-rate" 0.1)
                                          ("year"  (2020 .  "[.B1]")  (2021 .  "[.C1]")  (2022 .  "[.D1]")  (2023 .  "[.E1]"))
                                              ("cashflow"  (-100 .  "[.B2]")  (-50 .  "[.C2]")  (150 .  "[.D2]")  (500 .  "[.E2]"))
                                                  ("year"  (2020 .  "[.B5]")  (2021 .  "[.C5]")  (2022 .  "[.D5]")  (2023 .  "[.E5]"))
                                                      ("value"  (-100 .  "[.B6]")  (-50 .  "[.C6]")  (150 .  "[.D6]")  (500 .  "[.E6]"))
                                                          ("discount"
                                                                (-100 .  "([.B8]/((1+[.B4])^([.B7]-[.B1])))")
                                                                     (-45.45454545454545 . "([.C8]/((1+[.B4])^([.C7]-[.B1])))")
                                                                          (123.96694214876031 .  "([.D8]/((1+[.B4])^([.D7]-[.B1])))")
                                                                               (375.65740045078877 .  "([.E8]/((1+[.B4])^([.E7]-[.B1])))"))
                                                              (""
                                                                    (-100 .  "[.B9]")
                                                                         (-45.45454545454545 . "[.C9]")
                                                                              (123.96694214876031 .  "[.D9]")
                                                                                   (375.65740045078877 .  "[.E9]"))
                                                                  ("discounted-cashflows"
                                                                        (-100 .  "[.B10]")
                                                                             (-45.45454545454545 . "[.C10]")
                                                                                  (123.96694214876031 .  "[.D10]")
                                                                                       (375.65740045078877 .  "[.E10]"))
                                                                      ("discounted-cashflows"
                                                                            (-100 .  "[.B11]")
                                                                                 (-45.45454545454545 . "[.C11]")
                                                                                      (123.96694214876031 .  "[.D11]")
                                                                                           (375.65740045078877 .  "[.E11]"))
                                                                          ("year"  (2023 .  "[.E1]"))
                                                                              ("terminal-value"  (7357.142857142857 . "(([.E2]*(1+[.B3]))/([.B4]-[.B3]))"))
                                                                                  ("value"  (7357.142857142857 . "[.B14]"))
                                                                                      ("discount"  (5527.530320918749 .  "([.B15]/((1+[.B4])^([.B13]-[.B1])))"))
                                                                                          ("discounted-terminal-value"  (5527.530320918749 .  "[.B16]"))
                                                                                              ("t"
                                                                                                    (-100 .  "[.B12]")
                                                                                                         (-45.45454545454545 . "[.C12]")
                                                                                                              (123.96694214876031 .  "[.D12]")
                                                                                                                   (375.65740045078877 .  "[.E12]"))
                                                                                                  ("c"
                                                                                                        0
                                                                                                             (-100 .  "[.B20]")
                                                                                                                  (-145.45454545454544 . "[.C20]")
                                                                                                                       (-21.48760330578513 .  "[.D20]"))
                                                                                                      (""
                                                                                                            (-100 .  "([.B18]+[.B19])")
                                                                                                                 (-145.45454545454544 . "([.C18]+[.C19])")
                                                                                                                      (-21.48760330578513 .  "([.D18]+[.D19])")
                                                                                                                           (354.16979714500366 .  "([.E18]+[.E19])"))
                                                                                                          ("total"  (5881.700118063753 .  "([.E20]+[.B17])"))
                                                                                                              ("dcf-value"  (5881.700118063753 .  "[.B21]"))
                                                                                                                  ("result"  (5881.700118063753 .  "[.B22]")))
                
              )
            )   
)
