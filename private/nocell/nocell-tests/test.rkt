#lang racket

(require racket/runtime-path)

(define-runtime-path original-nocells "nocell/")
(define-runtime-path actual-stack-langs "actual-stack-lang/")
(define-runtime-path expected-stack-langs "expected-stack-lang/")

(module+ test-ignore
  (require "../main.rkt")
  (require rackunit)

  (define (test file) 
    (let* ([original-nocell-path (build-path original-nocells file)]
           [actual-stack-lang (load-nocell original-nocell-path)]
           [actual-stack-lang-path (build-path actual-stack-langs file)]
           [expected-stack-lang-path (build-path expected-stack-langs file)]
           [expected-stack-lang (load-stack-lang expected-stack-lang-path)]) 
      (write-stack-lang actual-stack-lang-path actual-stack-lang)
      ; We reload so that the actual and expected have identical formats.
      ; Feels like there should be a better way to do this...
      (let* ([actual-stack-lang-reloaded (load-stack-lang actual-stack-lang-path)])
        (check-equal? expected-stack-lang actual-stack-lang-reloaded)
        )))

  (define (load-nocell path) (dynamic-require path 'result))

  (define (load-stack-lang path) 
    (if (file-exists? path) 
      (call-with-input-file path (lambda (port) (read port))) 
      '()))

  (define (write-stack-lang path stack-lang) 
    (call-with-output-file path (lambda (port) (pretty-write stack-lang port)) #:exists 'replace))

  (for-each test (filter (lambda (p) (string-suffix? (path->string p) ".nocell")) (directory-list original-nocells #:build? #f))))
