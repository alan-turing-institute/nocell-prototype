#lang racket
(require sxml
         "../stack-lang/main.rkt"
         "../stack-lang/cell.rkt"
         "../grid/ods.rkt"
         syntax/to-string)

(define-namespace-anchor top)

(define cell-template
  "; -*- racket -*-
#lang racket
(require \"../../../stack-lang/main.rkt\")
(provide result)
(define result
~a)")

(define grid-template
  "; -*- racket -*-
#lang racket
(require math/array
         \"../../../grid/sheet.rkt\")
(provide result)
(define result
~a)")

;; generate regression test examples, starting from each ".nocell"
;; file in the child directories of test-examples, and producing a
;; corresponding ".cell", ".grid" and ".sxml" file
;;
(module+ main
  (for* ([test-dir (directory-list "test-examples" #:build? #t)]
         [suffix '(".cell")])
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-attach-module (namespace-anchor->namespace top)
                               "../stack-lang/cell.rkt")
      (namespace-attach-module (namespace-anchor->namespace top)
                               "../grid/ods.rkt")
      (define nocell-paths (filter (curryr path-has-extension? ".nocell")
                                   (directory-list test-dir #:build? #t)))
      (unless (null? nocell-paths)
        (define nocell-path (car nocell-paths))
        (define cell-path (path-replace-extension nocell-path ".cell"))
        (define grid-path (path-replace-extension nocell-path ".grid"))
        (define fods-path (path-replace-extension nocell-path ".fods"))
        (define result (dynamic-require nocell-path 'result))
        (with-output-to-file cell-path #:exists 'replace
          (λ () (printf cell-template
                        (pretty-format
                         (map (λ (a) (struct-copy assignment a [sampler #f]))
                              result)))))

        (with-output-to-file grid-path #:exists 'replace
          (λ () (printf grid-template
                        (pretty-format (stack->sheet result)))))

        (with-output-to-file fods-path #:exists 'replace
          (λ () (display (srl:sxml->xml (ods (stack->sheet result))))))
        ))))
