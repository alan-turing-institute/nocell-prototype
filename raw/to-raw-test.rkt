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
             (check (workbook->raw (worksheet->raw "Sheet1" '())) "empty")
             )

  (test-case "Test create a one cell ods"
             ; A single ODS cell
             (check (workbook->raw (worksheet->raw "One Cell Sheet" (row->raw (string->raw "One Cell")) )) "one-cell")
             )
  )

