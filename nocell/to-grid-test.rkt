#lang racket

(module+ test
  (require rackunit "main.rkt")

  (define (check-grid actual-stack expected-grid)
    (let ([grid (stack->grid actual-stack)])
      (check-equal? grid expected-grid)
      )
    )
  (define (style-output) '("output" "TwoDecimalPlaces" ((background-color "#b3df8d"))))

  (test-case "Safety check - an empty stack creates an empty grid"
             (check-grid '() '(() ()))
             )

  (test-case "Empty result creates one cell with the label"
             (check-grid 
               '(
                 (0 label () (output))
                 ) 
               `(
                 (
                  ("output" ("label"))
                  ) 
                 (
                  ,(style-output)
                  )
                 )
               )
             )

  (test-case "Value result creates two cells, label and value"
             (check-grid 
               '(
                 (0 label 1 ())
                 ) 
               '(
                 ((() ("label" 1)))
                 ()
                 )
               )
             )

  (test-case "List of values result creates a row of cells"
             (check-grid 
               '(
                 (0 years (2020 2021 2022 2023) ())
                 (1 label 1 ())
                 ) 
               '(
                 (
                  (() ("years" 2020 2021 2022 2023))
                  (() ("label" 1))
                  )
                 ()
                 )
               )
             )

  (test-case "Function application creates a formula"
             (check-grid 
               '(
                 (0 simple (+ 1 1) ())
                 (1 complex (+ (+ 1 2) (+ 3 4)) ())
                 ) 
               '(
                 (
                  (() ("simple" (2 "(1+1)")))
                  (() ("complex" (10 "((1+2)+(3+4))")))
                  )
                 ()
                 )
               )
             )

  (test-case "Row of function applications creates a row of formulae"
             (check-grid 
               '(
                 (0 result ( (+ 1 1) (- 5 1) (* 6 5) (/ 24 3) ) ())
                 ) 
               '(
                 ((() ("result" (2 "(1+1)") (4 "(5-1)") (30 "(6*5)") (8 "(24/3)"))))
                 ()
                 )
               )
             )

  (test-case "col row references get turned into references"
             (check-grid 
               '(
                 (0 years (2020) ())
                 (1 short ((- (col 0 (row 0)) 2000)) ())
                 ) 
               '((
                  (() ("years" 2020))
                  (() ("short" (20 "([.C2]-2000)")))
                  ) ())
               )
             )

  (test-case "row references get turned into range references"
             (check-grid 
               '(
                 (0 years (10 20) ())
                 (1 short (last (row 0)) ())
                 ) 
               '((
                  (() ("years" 10 20))
                  (() ("short" (20 "last([.C2:.D2])")))
                  ) ())
               )
             )

  (test-case "row references where row has one value get turned into cell not range references"
             (check-grid 
               '(
                 (0 rate 10 ())
                 (1 thing (+ (row 0) 10) ())
                 ) 
               '((
                  (() ("rate" 10))
                  (() ("thing" (20 "([.C2]+10)")))
                  ) ())
               )
             )

  (test-case "row references as only item on row get turned into a row of cell references"
             (check-grid 
               '(
                 (0 years (10 20) ())
                 (1 years (row 0) ())
                 ) 
               '((
                  (() ("years" 10 20))
                  (() ("years" (10 "[.C2]") (20 "[.D2]")))
                  ) ())
               )
             )


  (test-case "Putting it all together for DCF model"
             (check-grid '(
                           (0 years (2020 2021 2022 2023) ())
                           (1 cashflows (-100 -50 150 500) ())
                           (2 terminal-growth 0.03 ())
                           (3 discount-rate 0.1 ())
                           (4 year ((col 0 (row 0)) (col 1 (row 0)) (col 2 (row 0)) (col 3 (row 0))) ())
                           (5
                            cashflow
                            ((col 0 (row 1)) (col 1 (row 1)) (col 2 (row 1)) (col 3 (row 1)))
                            ())
                           (6 year ((col 0 (row 4)) (col 1 (row 4)) (col 2 (row 4)) (col 3 (row 4))) ())
                           (7
                            value
                            ((col 0 (row 5)) (col 1 (row 5)) (col 2 (row 5)) (col 3 (row 5)))
                            ())
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
                            ())
                           (9 "" ((col 0 (row 8)) (col 1 (row 8)) (col 2 (row 8)) (col 3 (row 8))) ())
                           (10
                            discounted-cashflows
                            ((col 0 (row 9)) (col 1 (row 9)) (col 2 (row 9)) (col 3 (row 9)))
                            ())
                           (11 discounted-cashflows (row 10) ())
                           (12 year (col 3 (row 0)) ())
                           (13
                            terminal-value
                            (/ (* (col 3 (row 1)) (+ 1 (row 2))) (- (row 3) (row 2)))
                            ())
                           (14 value (row 13) ())
                           (15
                            discount
                            (/ (row 14) (expt (+ 1 (row 3)) (- (row 12) (col 0 (row 0)))))
                            ())
                           (16 discounted-terminal-value (row 15) ())
                           (17
                            t
                            ((col 0 (row 11)) (col 1 (row 11)) (col 2 (row 11)) (col 3 (row 11)))
                            ())
                           (18 c (0 (col 0 (row 19)) (col 1 (row 19)) (col 2 (row 19))) ())
                           (19
                            ""
                            ((+ (col 0 (row 17)) (col 0 (row 18)))
                             (+ (col 1 (row 17)) (col 1 (row 18)))
                             (+ (col 2 (row 17)) (col 2 (row 18)))
                             (+ (col 3 (row 17)) (col 3 (row 18))))
                            ())
                           (20 total (+ (col 3 (row 19)) (row 16)) ())
                           (21 dcf-value (row 20) ())
                           (22 "result" (row 21) (output)))

                         '(((() ("years" 2020 2021 2022 2023))
                            (() ("cashflows" -100 -50 150 500))
                            (() ("terminal-growth" 0.03))
                            (() ("discount-rate" 0.1))
                            (() ("year" (2020 "[.C2]") (2021 "[.D2]") (2022 "[.E2]") (2023 "[.F2]")))
                            (() ("cashflow" (-100 "[.C3]") (-50 "[.D3]") (150 "[.E3]") (500 "[.F3]")))
                            (() ("year" (2020 "[.C6]") (2021 "[.D6]") (2022 "[.E6]") (2023 "[.F6]")))
                            (() ("value" (-100 "[.C7]") (-50 "[.D7]") (150 "[.E7]") (500 "[.F7]")))
                            (()
                             ("discount"
                              (-100 "([.C9]/((1+[.C5])^([.C8]-[.C2])))")
                              (-45.45454545454545 "([.D9]/((1+[.C5])^([.D8]-[.C2])))")
                              (123.96694214876031 "([.E9]/((1+[.C5])^([.E8]-[.C2])))")
                              (375.65740045078877 "([.F9]/((1+[.C5])^([.F8]-[.C2])))")))
                            (()
                             (""
                              (-100 "[.C10]")
                              (-45.45454545454545 "[.D10]")
                              (123.96694214876031 "[.E10]")
                              (375.65740045078877 "[.F10]")))
                            (()
                             ("discounted-cashflows"
                              (-100 "[.C11]")
                              (-45.45454545454545 "[.D11]")
                              (123.96694214876031 "[.E11]")
                              (375.65740045078877 "[.F11]")))
                            (()
                             ("discounted-cashflows"
                              (-100 "[.C12]")
                              (-45.45454545454545 "[.D12]")
                              (123.96694214876031 "[.E12]")
                              (375.65740045078877 "[.F12]")))
                            (() ("year" (2023 "[.F2]")))
                            (()
                             ("terminal-value" (7357.142857142857 "(([.F3]*(1+[.C4]))/([.C5]-[.C4]))")))
                            (() ("value" (7357.142857142857 "[.C15]")))
                            (() ("discount" (5527.530320918749 "([.C16]/((1+[.C5])^([.C14]-[.C2])))")))
                            (() ("discounted-terminal-value" (5527.530320918749 "[.C17]")))
                            (()
                             ("t"
                              (-100 "[.C13]")
                              (-45.45454545454545 "[.D13]")
                              (123.96694214876031 "[.E13]")
                              (375.65740045078877 "[.F13]")))
                            (()
                             ("c"
                              0
                              (-100 "[.C21]")
                              (-145.45454545454544 "[.D21]")
                              (-21.48760330578513 "[.E21]")))
                            (()
                             (""
                              (-100 "([.C19]+[.C20])")
                              (-145.45454545454544 "([.D19]+[.D20])")
                              (-21.48760330578513 "([.E19]+[.E20])")
                              (354.16979714500366 "([.F19]+[.F20])")))
                            (() ("total" (5881.700118063753 "([.F21]+[.C18])")))
                            (() ("dcf-value" (5881.700118063753 "[.C22]")))
                            ("output" ("result" (5881.700118063753 "[.C23]"))))
                           (("output" "TwoDecimalPlaces" ((background-color "#b3df8d")))))
                         ))
  )
