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

  (test-case "Test create an empty row"
             ; This is the minimum required for Excel to open - a single worksheet
             (check (workbook->raw (list (worksheet->raw "Sheet1" (list (row->raw '() '()))))) "empty-row")
             )

  (test-case "Test create a one cell ods"
             ; A single cell ods
             (check (workbook->raw (list (worksheet->raw "One Cell Sheet" (list (row->raw (list (cell->raw "One Cell" '() '())) '()) )))) "one-cell")
             )

  (test-case "Test create a one row ods"
             ; A one row ods
             (check (workbook->raw (list (worksheet->raw "One Row Sheet" (list (list->raw-row (list "One" "Two" "Three"))) ))) "one-row")
             )

  (test-case "Test create a one row ods like nocell"
             (check (workbook->raw (list (worksheet->raw "One Row Sheet" (list (list->raw-row (list "One" (list 2 "1+1")))) ))) "one-row-nocell-like")
             )

  (test-case "Test create a simple grid ods"
             (check 
               (workbook->raw (list
                                (grid->raw-worksheet "One Grid Sheet" '(
                                                                        ("NoCellCellDefault" ("One" "Two" "Three"))
                                                                        ("NoCellCellDefault" ("Four" "Five" "Six"))
                                                                        )
                                                     )) 
                              ) "one-grid")
             )

  (test-case "Test create a two worksheet ods"
             (check 
               (grids->raw-workbook (list 
                                      '("Sheet One" (
                                                     ("NoCellCellDefault" ("One" "Two" "Three"))
                                                     ("NoCellCellDefault" ("Four" "Five" "Six"))
                                                     )
                                        )
                                      '("Sheet Two" (
                                                     ("NoCellCellDefault" ("A" "B" "C"))
                                                     ("NoCellCellDefault" ("One" "Two" "Three"))
                                                     )
                                        )
                                      )
                                    ) "two-grid")
             )

  (test-case "Test full range of types"
             (check 
               (workbook->raw (list
                                (grid->raw-worksheet "All types demo" '( 
                                                                         ("NoCellCellDefault" ("string" 1 #t 2 #f 4 (2 "1+1")))
                                                                         )
                                                     )) 
                              ) "all-types")
             )


  (test-case "Test text styles"
             (check 
               (workbook->raw (list
                                (grid->raw-worksheet "Text styles" '( 
                                                                      ("bold" ("bold"))
                                                                      ("italic" ("italic"))
                                                                      )
                                                     )) 
                              '(
                               ("bold" () () ((font-weight "bold")))
                               ("italic" () () ((font-style "italic")))
                               )
                              ) "text-styles"))
  (test-case "Test dcf model"
             (check 
               (workbook->raw (list
                                (grid->raw-worksheet "dcf" 
                                                     '(("result" ("years" 2020 2021 2022 2023))
                                                       ("result" ("cashflows" -100 -50 150 500))
                                                       ("result" ("terminal-growth" 0.03))
                                                       ("result" ("discount-rate" 0.1))
                                                       ("result"
                                                        ("year"
                                                         (2020 "[.C2]")
                                                         (2021 "[.D2]")
                                                         (2022 "[.E2]")
                                                         (2023 "[.F2]")))
                                                       ("result"
                                                        ("cashflow"
                                                         (-100 "[.C3]")
                                                         (-50 "[.D3]")
                                                         (150 "[.E3]")
                                                         (500 "[.F3]")))
                                                       ("result"
                                                        ("year"
                                                         (2020 "[.C6]")
                                                         (2021 "[.D6]")
                                                         (2022 "[.E6]")
                                                         (2023 "[.F6]")))
                                                       ("result"
                                                        ("value" (-100 "[.C7]") (-50 "[.D7]") (150 "[.E7]") (500 "[.F7]")))
                                                       ("result"
                                                        ("discount"
                                                         (-100 "([.C9]/((1+[.C5])^([.C8]-[.C2])))")
                                                         (-45.45454545454545 "([.D9]/((1+[.C5])^([.D8]-[.C2])))")
                                                         (123.96694214876031 "([.E9]/((1+[.C5])^([.E8]-[.C2])))")
                                                         (375.65740045078877 "([.F9]/((1+[.C5])^([.F8]-[.C2])))")))
                                                       ("result"
                                                        (""
                                                         (-100 "[.C10]")
                                                         (-45.45454545454545 "[.D10]")
                                                         (123.96694214876031 "[.E10]")
                                                         (375.65740045078877 "[.F10]")))
                                                       ("result"
                                                        ("discounted-cashflows"
                                                         (-100 "[.C11]")
                                                         (-45.45454545454545 "[.D11]")
                                                         (123.96694214876031 "[.E11]")
                                                         (375.65740045078877 "[.F11]")))
                                                       ("result"
                                                        ("discounted-cashflows"
                                                         (-100 "[.C12]")
                                                         (-45.45454545454545 "[.D12]")
                                                         (123.96694214876031 "[.E12]")
                                                         (375.65740045078877 "[.F12]")))
                                                       ("result" ("year" (2023 "[.F2]")))
                                                       ("result"
                                                        ("terminal-value"
                                                         (7357.142857142857 "(([.F3]*(1+[.C4]))/([.C5]-[.C4]))")))
                                                       ("result" ("value" (7357.142857142857 "[.C15]")))
                                                       ("result"
                                                        ("discount" (5527.530320918749 "([.C16]/((1+[.C5])^([.C14]-[.C2])))")))
                                                       ("result" ("discounted-terminal-value" (5527.530320918749 "[.C17]")))
                                                       ("result"
                                                        ("t"
                                                         (-100 "[.C13]")
                                                         (-45.45454545454545 "[.D13]")
                                                         (123.96694214876031 "[.E13]")
                                                         (375.65740045078877 "[.F13]")))
                                                       ("result"
                                                        ("c"
                                                         0
                                                         (-100 "[.C21]")
                                                         (-145.45454545454544 "[.D21]")
                                                         (-21.48760330578513 "[.E21]")))
                                                       ("result"
                                                        (""
                                                         (-100 "([.C19]+[.C20])")
                                                         (-145.45454545454544 "([.D19]+[.D20])")
                                                         (-21.48760330578513 "([.E19]+[.E20])")
                                                         (354.16979714500366 "([.F19]+[.F20])")))
                                                       ("result" ("total" (5881.700118063753 "([.F21]+[.C18])")))
                                                       ("result" ("dcf-value" (5881.700118063753 "[.C22]")))
                                                       ("result" ("result" (5881.700118063753 "[.C23]")))) 
                                                     )) 
                              '(("result" "TwoDecimalPlaces" ((background-color "#b3df8d")) ()))
                              ) "dcf")
             )            )
