#lang racket
(require rackunit
         sxml
         "../stack-lang/main.rkt"
         "../stack-lang/cell.rkt"
         "../grid/ods.rkt")

;; "maybe" implemented with lists
;; nothing = null
;; just a  = (list a)
(define ((lift/maybe fn) . args)
  (if (ormap null? args)
      null
      (list (apply fn (map car args)))))

;; Check that conv correctly converts from the data read from
;; maybe-input-path (if not missing) to the data read from
;; maybe-expected-path.  If the expected path is missing, attempt the
;; conversion anyway, even though the check cannot be performed.
(define ((check-convert conv) maybe-input-path maybe-expect-path)
  (define maybe-input  (map (curryr dynamic-require 'result) maybe-input-path))
  (define maybe-expect (map (curryr dynamic-require 'result) maybe-expect-path))
  (define maybe-actual ((lift/maybe conv) maybe-input))
  ((lift/maybe check-equal?) maybe-actual maybe-expect))

;; Check conversions nocell -> cell -> grid -> sxml, grid -> fods
;;
;; nocell->cell just a case of evaluating `result`, done when we require it
(define check:nocell->cell (check-convert identity))
(define check:cell->grid   (check-convert stack->sheet))
(define check:grid->sxml   (check-convert ods))
(define check:grid->fods   (check-convert (curryr sheet-write-ods "out.fods")))

(module+ test
  (for ([test-dir (directory-list "test-examples" #:build? #t)])
    (define (paths/suffix suffix) (filter (curryr path-has-extension? suffix)
                                          (directory-list test-dir #:build? #t)))
    (match-let ([(list nocell-path cell-path grid-path sxml-path)
                 (map paths/suffix '(".nocell" ".cell" ".grid" ".sxml"))])
      (check:nocell->cell nocell-path cell-path)
      (check:cell->grid   cell-path   grid-path)
      (check:grid->sxml   grid-path   sxml-path)
      ;; check that the conversion succeeds, but not the resulting file
      (check:grid->fods   grid-path   null))))
