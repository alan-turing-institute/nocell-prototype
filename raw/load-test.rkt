#lang racket/base

(require racket/runtime-path)
(require racket/list)
(require racket/file)

(define-runtime-path test-files "test-files")
(module+ test
  (require rackunit "load.rkt")
  (test-case "Test conversion of expected-test.ods to raw"
             (let* ([expected-test-ods-raw-path (build-path test-files "expected-test.ods.raw")]
                    [expected-test-ods-path (build-path test-files "expected-test.ods")]
                    [actual-raw-file (build-path test-files "actual-test.ods.raw")]
                    [actual-raw (zip->raw expected-test-ods-path)]
                    [expected-raw (last (file->value expected-test-ods-raw-path))])
               (raw->file actual-raw actual-raw-file)
               (check-equal? actual-raw expected-raw)
               )
             )
  (test-case "Test conversion of expected-test.xlsx to raw"
             (let* ([expected-test-xlsx-raw-path (build-path test-files "expected-test.xlsx.raw")]
                    [expected-test-xlsx-path (build-path test-files "expected-test.xlsx")]
                    [actual-raw-file (build-path test-files "actual-test.xlsx.raw")]
                    [actual-raw (zip->raw expected-test-xlsx-path)]
                    [expected-raw (last (file->value expected-test-xlsx-raw-path))])
               (raw->file actual-raw actual-raw-file)
               (check-equal? actual-raw expected-raw)
               )
             )
  )
