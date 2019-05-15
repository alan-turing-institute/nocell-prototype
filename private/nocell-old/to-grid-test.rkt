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
                 (0 label () (output) ())
                 ) 

               '((("output-NoDecimalPlaces-#000000" ("label")))
                 (("output-NoDecimalPlaces-#000000"
                   "NoDecimalPlaces"
                   ((background-color "#b3df8d"))
                   ((font-color "#000000")))))
               )
             )

  (test-case "Value result creates two cells, label and value"
             (check-grid 
               '(
                 (0 label 1 () ())
                 ) 
               '((("default-NoDecimalPlaces-#000000" ("label" 1)))
                 (("default-NoDecimalPlaces-#000000"
                   "NoDecimalPlaces"
                   ()
                   ((font-color "#000000"))))) 
               )
             )

  (test-case "List of values result creates a row of cells"
             (check-grid 
               '(
                 (0 years (2020 2021 2022 2023) () ())
                 (1 label 1 () () )
                 ) 
               '((("default-NoDecimalPlaces-#000000" ("years" 2020 2021 2022 2023))
                  ("default-NoDecimalPlaces-#000000" ("label" 1)))
                 (("default-NoDecimalPlaces-#000000"
                   "NoDecimalPlaces"
                   ()
                   ((font-color "#000000")))))
               )
             )

  (test-case "Function application creates a formula"
             (check-grid 
               '(
                 (0 simple (+ 1 1) () ())
                 (1 complex (+ (+ 1 2) (+ 3 4)) () ())
                 ) 
               '((("default-NoDecimalPlaces-#000000" ("simple" (2 "(1+1)")))
                  ("default-NoDecimalPlaces-#000000" ("complex" (10 "((1+2)+(3+4))"))))
                 (("default-NoDecimalPlaces-#000000"
                   "NoDecimalPlaces"
                   ()
                   ((font-color "#000000")))))
               )
             )

  (test-case "Row of function applications creates a row of formulae"
             (check-grid 
               '(
                 (0 result ( (+ 1 1) (- 5 1) (* 6 5) (/ 24 3) ) () ())
                 ) 
               '((("default-NoDecimalPlaces-#000000"
                   ("result" (2 "(1+1)") (4 "(5-1)") (30 "(6*5)") (8 "(24/3)"))))
                 (("default-NoDecimalPlaces-#000000"
                   "NoDecimalPlaces"
                   ()
                   ((font-color "#000000")))))
               )
             )

  (test-case "col row references get turned into references"
             (check-grid 
               '(
                 (0 years (2020) () ())
                 (1 short ((- (col 0 (row 0)) 2000)) () ())
                 ) 
               '((("default-NoDecimalPlaces-#000000" ("years" 2020))
                  ("default-NoDecimalPlaces-#000000" ("short" (20 "([.C2]-2000)"))))
                 (("default-NoDecimalPlaces-#000000"
                   "NoDecimalPlaces"
                   ()
                   ((font-color "#000000")))))
               )
             )

  (test-case "row references get turned into range references"
             (check-grid 
               '(
                 (0 years (10 20) () ())
                 (1 short (last (row 0)) () ())
                 ) 
               '((("default-NoDecimalPlaces-#000000" ("years" 10 20))
                  ("default-NoDecimalPlaces-#000000" ("short" (20 "last([.C2:.D2])"))))
                 (("default-NoDecimalPlaces-#000000"
                   "NoDecimalPlaces"
                   ()
                   ((font-color "#000000")))))
               )
             )

  (test-case "row references where row has one value get turned into cell not range references"
             (check-grid 
               '(
                 (0 rate 10 () ())
                 (1 thing (+ (row 0) 10) () ())
                 ) 
               '((("default-NoDecimalPlaces-#000000" ("rate" 10))
                  ("default-NoDecimalPlaces-#000000" ("thing" (20 "([.C2]+10)"))))
                 (("default-NoDecimalPlaces-#000000"
                   "NoDecimalPlaces"
                   ()
                   ((font-color "#000000")))))
               )
             )

  (test-case "row references as only item on row get turned into a row of cell references"
             (check-grid 
               '(
                 (0 years (10 20) () ())
                 (1 years (row 0) () ())
                 ) 
               '((("default-NoDecimalPlaces-#000000" ("years" 10 20))
                  ("default-NoDecimalPlaces-#000000" ("years" (10 "[.C2]") (20 "[.D2]"))))
                 (("default-NoDecimalPlaces-#000000"
                   "NoDecimalPlaces"
                   ()
                   ((font-color "#000000")))))
               )
             )


  (test-case "Putting it all together for DCF model"
             (check-grid '((0 years (2020 2021 2022 2023) (parameter) (dcf-value))
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

                         '((("parameter-NoDecimalPlaces-#4d4d4d" ("years" 2020 2021 2022 2023))
                            ("parameter-NoDecimalPlaces-#4d4d4d" ("cashflows" -100 -50 150 500))
                            ("parameter-TwoDecimalPlaces-#4d4d4d" ("terminal-growth" 0.03))
                            ("last-parameter-TwoDecimalPlaces-#4d4d4d" ("discount-rate" 0.1))
                            ("parameter-NoDecimalPlaces-#4d4d4d"
                             ("year" (2020 "[.C2]") (2021 "[.D2]") (2022 "[.E2]") (2023 "[.F2]")))
                            ("last-parameter-NoDecimalPlaces-#4d4d4d"
                             ("cashflow" (-100 "[.C3]") (-50 "[.D3]") (150 "[.E3]") (500 "[.F3]")))
                            ("parameter-NoDecimalPlaces-#5da5da"
                             ("year" (2020 "[.C6]") (2021 "[.D6]") (2022 "[.E6]") (2023 "[.F6]")))
                            ("last-parameter-NoDecimalPlaces-#5da5da"
                             ("value" (-100 "[.C7]") (-50 "[.D7]") (150 "[.E7]") (500 "[.F7]")))
                            ("return-NoDecimalPlaces-#5da5da"
                             ("discount"
                              (-100 "([.C9]/((1+[.C5])^([.C8]-[.C2])))")
                              (-45.45454545454545 "([.D9]/((1+[.C5])^([.D8]-[.C2])))")
                              (123.96694214876031 "([.E9]/((1+[.C5])^([.E8]-[.C2])))")
                              (375.65740045078877 "([.F9]/((1+[.C5])^([.F8]-[.C2])))")))
                            ("return-NoDecimalPlaces-#4d4d4d"
                             (""
                              (-100 "[.C10]")
                              (-45.45454545454545 "[.D10]")
                              (123.96694214876031 "[.E10]")
                              (375.65740045078877 "[.F10]")))
                            ("last-parameter-NoDecimalPlaces-#4d4d4d"
                             ("discounted-cashflows"
                              (-100 "[.C11]")
                              (-45.45454545454545 "[.D11]")
                              (123.96694214876031 "[.E11]")
                              (375.65740045078877 "[.F11]")))
                            ("parameter-NoDecimalPlaces-#faa43a"
                             ("discounted-cashflows"
                              (-100 "[.C12]")
                              (-45.45454545454545 "[.D12]")
                              (123.96694214876031 "[.E12]")
                              (375.65740045078877 "[.F12]")))
                            ("last-parameter-NoDecimalPlaces-#5da5da" ("year" (2023 "[.F2]")))
                            ("return-TwoDecimalPlaces-#60bd68"
                             ("terminal-value" (7357.142857142857 "(([.F3]*(1+[.C4]))/([.C5]-[.C4]))")))
                            ("last-parameter-TwoDecimalPlaces-#5da5da"
                             ("value" (7357.142857142857 "[.C15]")))
                            ("return-TwoDecimalPlaces-#5da5da"
                             ("discount" (5527.530320918749 "([.C16]/((1+[.C5])^([.C14]-[.C2])))")))
                            ("last-parameter-TwoDecimalPlaces-#faa43a"
                             ("discounted-terminal-value" (5527.530320918749 "[.C17]")))
                            ("parameter-NoDecimalPlaces-#faa43a"
                             ("t"
                              (-100 "[.C13]")
                              (-45.45454545454545 "[.D13]")
                              (123.96694214876031 "[.E13]")
                              (375.65740045078877 "[.F13]")))
                            ("last-parameter-NoDecimalPlaces-#faa43a"
                             ("c"
                              0
                              (-100 "[.C21]")
                              (-145.45454545454544 "[.D21]")
                              (-21.48760330578513 "[.E21]")))
                            ("return-NoDecimalPlaces-#faa43a"
                             (""
                              (-100 "([.C19]+[.C20])")
                              (-145.45454545454544 "([.D19]+[.D20])")
                              (-21.48760330578513 "([.E19]+[.E20])")
                              (354.16979714500366 "([.F19]+[.F20])")))
                            ("return-TwoDecimalPlaces-#faa43a"
                             ("total" (5881.700118063753 "([.F21]+[.C18])")))
                            ("return-TwoDecimalPlaces-#4d4d4d"
                             ("dcf-value" (5881.700118063753 "[.C22]")))
                            ("output-TwoDecimalPlaces-#000000" ("result" (5881.700118063753 "[.C23]"))))
                           (("output-TwoDecimalPlaces-#000000"
                             "TwoDecimalPlaces"
                             ((background-color "#b3df8d"))
                             ((font-color "#000000")))
                            ("return-NoDecimalPlaces-#4d4d4d"
                             "NoDecimalPlaces"
                             ((border-bottom "medium solid #4d4d4d"))
                             ((font-weight "bold") (font-color "#4d4d4d")))
                            ("parameter-TwoDecimalPlaces-#4d4d4d"
                             "TwoDecimalPlaces"
                             ()
                             ((font-style "italic") (font-color "#4d4d4d")))
                            ("parameter-NoDecimalPlaces-#faa43a"
                             "NoDecimalPlaces"
                             ()
                             ((font-style "italic") (font-color "#faa43a")))
                            ("return-TwoDecimalPlaces-#4d4d4d"
                             "TwoDecimalPlaces"
                             ((border-bottom "medium solid #4d4d4d"))
                             ((font-weight "bold") (font-color "#4d4d4d")))
                            ("return-NoDecimalPlaces-#faa43a"
                             "NoDecimalPlaces"
                             ((border-bottom "medium solid #faa43a"))
                             ((font-weight "bold") (font-color "#faa43a")))
                            ("return-TwoDecimalPlaces-#60bd68"
                             "TwoDecimalPlaces"
                             ((border-bottom "medium solid #60bd68"))
                             ((font-weight "bold") (font-color "#60bd68")))
                            ("last-parameter-NoDecimalPlaces-#faa43a"
                             "NoDecimalPlaces"
                             ((border-bottom "thin solid #faa43a"))
                             ((font-style "italic") (font-color "#faa43a")))
                            ("parameter-NoDecimalPlaces-#5da5da"
                             "NoDecimalPlaces"
                             ()
                             ((font-style "italic") (font-color "#5da5da")))
                            ("parameter-NoDecimalPlaces-#4d4d4d"
                             "NoDecimalPlaces"
                             ()
                             ((font-style "italic") (font-color "#4d4d4d")))
                            ("return-TwoDecimalPlaces-#faa43a"
                             "TwoDecimalPlaces"
                             ((border-bottom "medium solid #faa43a"))
                             ((font-weight "bold") (font-color "#faa43a")))
                            ("last-parameter-TwoDecimalPlaces-#4d4d4d"
                             "TwoDecimalPlaces"
                             ((border-bottom "thin solid #4d4d4d"))
                             ((font-style "italic") (font-color "#4d4d4d")))
                            ("last-parameter-TwoDecimalPlaces-#5da5da"
                             "TwoDecimalPlaces"
                             ((border-bottom "thin solid #5da5da"))
                             ((font-style "italic") (font-color "#5da5da")))
                            ("return-TwoDecimalPlaces-#5da5da"
                             "TwoDecimalPlaces"
                             ((border-bottom "medium solid #5da5da"))
                             ((font-weight "bold") (font-color "#5da5da")))
                            ("last-parameter-TwoDecimalPlaces-#faa43a"
                             "TwoDecimalPlaces"
                             ((border-bottom "thin solid #faa43a"))
                             ((font-style "italic") (font-color "#faa43a")))
                            ("last-parameter-NoDecimalPlaces-#5da5da"
                             "NoDecimalPlaces"
                             ((border-bottom "thin solid #5da5da"))
                             ((font-style "italic") (font-color "#5da5da")))
                            ("return-NoDecimalPlaces-#5da5da"
                             "NoDecimalPlaces"
                             ((border-bottom "medium solid #5da5da"))
                             ((font-weight "bold") (font-color "#5da5da")))
                            ("last-parameter-NoDecimalPlaces-#4d4d4d"
                             "NoDecimalPlaces"
                             ((border-bottom "thin solid #4d4d4d"))
                             ((font-style "italic") (font-color "#4d4d4d")))))
                         ))
  )
