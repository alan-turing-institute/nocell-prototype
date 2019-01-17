#lang racket

;; Parse and generate (a small subset of) OpenFormula format formulae
;; from cell-values.  OpenFormula is the formula language used within
;; OpenDocument spreadsheets.
;;
;; Based on [1], which is what we mean when referring to the "the
;; standard" mean [1].
;;
;; [1] "Open Document Format for Office Applications (OpenDocument)
;;      Version 1.2, part 2: Recalculated Formula (OpenFormula)
;;      Format", OASIS, September 2011

(require math/array
         "../sheet.rkt")

(define (transpose arr) (array-axis-swap arr 0 1))

;; e.g.
;; (format-array-openformula (array #[#[1 2 3] #[4 5 6]]))
;;      ==> "{1;4|2;5|3;6}"

(define (format-element-openformula elt)
  (cond
    [(number? elt) (~a elt)]
    [(string? elt) (format "&quot;~a&quot;" elt)]
    [(boolean? elt) (if elt "TRUE" "FALSE")]
    [(is-empty? elt) ""]
    [(is-error? elt)
     (case elt
       [(NA)    "#NA"]
       [(VALUE) "#VALUE!"]
       [(DIV/0) "#DIV/0!"]
       [(NUM)   "#NUM!"]
       [else (raise-arguments-error 'format-element-openformula
                                    "invalid error case encountered"
                                    "got" elt)])]
    [else (raise-argument-error 'format-element-openformula
                                "is-atomic?"
                                elt)]))

(define (format-array-openformula arr)
  (let ((rows (array->list* (transpose arr))))
    (string-join
     (map (lambda (elts)
            (string-join
             (map (lambda (elt) (format-element-openformula elt))
                  elts)
             ";"))
          rows)
     "|"
     #:before-first "{"
     #:after-last   "}"
     )))

;; cell-expr->openformula : cell-expr? -> string?
(define (cell-expr->openformula expr acc)
  (match expr
    [(cell-value elements)
     (cell-expr->openformula
      '()
      (if (cell-simple-value? expr)
          (format-element-openformula (array-ref elements #(0 0)))
          (format-array-openformula elements)))]
      
    ;; [(cell-ref col row col-rel? row-rel?) ...]

    ;; [(cell-range tl br) ...]

    ;; [(cell-name id) ...]

    ;; [(cell-app builtin args) ...]

    [else acc]
    ))

