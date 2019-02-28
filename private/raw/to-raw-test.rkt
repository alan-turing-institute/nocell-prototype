#lang racket


(require racket/runtime-path)
(define-runtime-path test-files "test-files")

(module+ test
  (require "to-raw.rkt")
  (require rackunit)
  (require "to-zip.rkt")
  (require file/unzip)

  (define (check-zips-have-equal-content? actual-zip-path expected-zip-path)
    (let ([unziped-actual (make-temporary-file "diffziptmp-actual-~a" 'directory)]
          [unziped-expected (make-temporary-file "diffziptmp-expected-~a" 'directory)])
      (unzip actual-zip-path (make-filesystem-entry-reader #:dest unziped-actual #:exists 'replace))
      (unzip expected-zip-path (make-filesystem-entry-reader #:dest unziped-expected #:exists 'replace))
      (let ([difference (with-output-to-string (lambda () (system (string-append "diff" " " "--recursive" " " (path->string unziped-actual) " " (path->string unziped-expected)) #:set-pwd? #t)))])
        (delete-directory/files unziped-actual)
        (delete-directory/files unziped-expected)
        (check-equal? difference "")
        )
      )
    )

  (define (check raw name) (let* ([expected (build-path test-files (string-join (list "expected-" name  ".ods") ""))]
                                  [actual (build-path test-files (string-join (list "actual-" name ".ods") ""))]
                                  )
                             (raw->zip raw actual)
                             (check-zips-have-equal-content? actual expected)
                             ))

  (test-case "Test create the smallest empty ods"
             ; This is the minimum required for Excel to open - a single worksheet
             (check (workbook->raw (list (worksheet->raw "Sheet1" '()))) "empty")
             )

  (test-case "Test create a one cell ods"
             ; A single cell ods
             (check (workbook->raw (list (worksheet->raw "One Cell Sheet" (list (row->raw (list (string->raw "One Cell" '()))) )))) "one-cell")
             )

  (test-case "Test create a one row ods"
             ; A one row ods
             (check (workbook->raw (list (worksheet->raw "One Row Sheet" (list (list->raw-row (list "One" "Two" "Three"))) ))) "one-row")
             )

  (test-case "Test create a simple grid ods"
             (check 
               (workbook->raw (list
                 (grid->raw-worksheet "One Grid Sheet" (list 
                                                         (list "One" "Two" "Three")
                                                         (list "Four" "Five" "Six")
                                                         )
                                      )) 
                 ) "one-grid")
             )

  (test-case "Test create a two worksheet ods"
             (check 
               (grids->raw-workbook (list 
                                      '("Sheet One" (
                                                     ("One" "Two" "Three")
                                                     ("Four" "Five" "Six")
                                                     )
                                        )
                                      '("Sheet Two" (
                                                     ("A" "B" "C")
                                                     ("One" "Two" "Three")
                                                     )
                                        )
                                      )
                                    ) "two-grid")
             )
    
  (test-case "Test full range of types"
             (check 
               (workbook->raw (list
                 (grid->raw-worksheet "All types demo" '( 
                                                         ("string" 1 #t 2 #f 4 (2 . "1+1"))
                                                         )
                                      )) 
                 ) "all-types")

   (test-case "Test dcf model"
             (check 
               (workbook->raw (list
                 (grid->raw-worksheet "dcf" '(("years" 2020 2021 2022 2023)
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
                                      )) 
                 ) "dcf")
               )            )
  )
