#lang racket/base

(require racket/runtime-path)
(define-runtime-path test-files "test-files")

(module+ test
  (require rackunit "save.rkt")
  (require racket/list)
  (require racket/file)
  (require file/unzip)
  (require racket/path)
  (require racket/port)
  (require racket/system)
  (require rackunit)

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

  (test-case "Test conversion of raw back to expected-test.ods"
             (let* ([expected-test-ods-raw-path (build-path test-files "expected-test.ods.raw")]
                    [expected-test-ods-path (build-path test-files "expected-test.ods")]
                    [actual-test-ods-path (build-path test-files "actual-test.ods")]
                    [actual-raw (last (file->value expected-test-ods-raw-path))])
               (raw->zip actual-raw actual-test-ods-path)
               (check-zips-have-equal-content? actual-test-ods-path expected-test-ods-path)
               )
             )

  (test-case "Test conversion of raw back to expected-test.xlsx"
             (let* ([expected-test-xlsx-raw-path (build-path test-files "expected-test.xlsx.raw")]
                    [expected-test-xlsx-path (build-path test-files "expected-test.xlsx")]
                    [actual-test-xlsx-path (build-path test-files "actual-test.xlsx")]
                    [actual-raw (last (file->value expected-test-xlsx-raw-path))])
               (raw->zip actual-raw actual-test-xlsx-path)
               (check-zips-have-equal-content? actual-test-xlsx-path expected-test-xlsx-path)
               )
             )
  )
