#lang racket
(require rackunit
         sxml
         "../private/cell/main.rkt"
         "../private/grid/ods.rkt")

(define-namespace-anchor top)

;; "maybe" implemented with lists
;; nothing = null
;; just a  = (list a)
(define ((lift/maybe fn) . args)
  (if (ormap null? args)
      null
      (list (apply fn (map car args)))))

(define attempt-load-result
  (lift/maybe (λ (path) (dynamic-require path 'result))))

;; Check that conv correctly converts from the data read from
;; maybe-input-path (if not missing) to the data read from
;; maybe-expected-path.  If the expected path is missing, attempt the
;; conversion anyway, even though the check cannot be performed.
(define ((check-convert conv) maybe-input-path maybe-expect-path)
  (define maybe-input  (attempt-load-result maybe-input-path))
  (define maybe-expect (attempt-load-result maybe-expect-path))
  (define maybe-actual ((lift/maybe conv) maybe-input))
  ((lift/maybe (λ (v1 v2) (check-within v1 v2 1e-8))) maybe-actual maybe-expect))

;; Check conversions nocell -> cell -> grid -> sxml, grid -> fods
;;
;; nocell->cell just a case of evaluating `result`, done when we require it
(define check:nocell->cell (check-convert identity))
(define check:cell->grid   (check-convert stack->sheet))
(define check:grid->sxml   (check-convert ods))
(define check:grid->fods   (check-convert (compose1 srl:sxml->xml ods)))

(module+ test
  (for ([test-dir (directory-list "test-examples" #:build? #t)])
    (define (paths/suffix suffix)
      (filter (curryr path-has-extension? suffix)
              (directory-list test-dir #:build? #t)))
    (match-let
        ([(list nocell-path cell-path grid-path sxml-path ods-path)
          (map paths/suffix '(".nocell" ".cell" ".grid" ".sxml" ".fods"))])
      ;; make a new namespace so that the name counters in nocell
      ;; begin at zero for each test example
      (parameterize ([current-namespace (make-base-namespace)])
        (namespace-attach-module (namespace-anchor->namespace top)
                                 "../private/cell/main.rkt")
        (namespace-attach-module (namespace-anchor->namespace top)
                                 "../private/grid/ods.rkt")
        (check:nocell->cell nocell-path cell-path)
        (check:cell->grid   cell-path   grid-path)
        (check:grid->sxml   grid-path   null)
        (check:grid->fods   grid-path   null)
        ))))
