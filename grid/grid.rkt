#lang racket/base

#|
A little language for creating sheets. (Uses functions, not syntax.)

Syntax:

  (sheet #:name "mysheet"
    (row 10 20 (cell))  ;; the last entry is a blank cell
    (row (cell 40) (cell 50 "a-cell") (cell `(+ 1 ,(ref "a-cell")))))

TODO
 - All references are converted to relative cell references and names are removed
 - Names aren't very consistent. We're using "id" in the context of a reference to mean a reference
   that should be turned into an A1-style reference, rather than a named reference. 
 - How to name: constructors? converters? parsers? 
  - Doesn't cope with rows of unequal length

|#

(require
 racket/contract
 math/array
 (only-in racket/list check-duplicates)
 (prefix-in s: "sheet.rkt"))

(provide
 (contract-out
  ;; Create a cell. Use within (row ...)
  [cell  (case-> (-> cell-spec/c)
                 (cell-expr? . -> . cell-spec/c)
                 (cell-expr? string? . -> . cell-spec/c))]
  
  ;; Create a row of cells. Use within (sheet ..)
  [row   (cell-spec/c ... . -> . row-spec?)]
  
  ;; Create a sheet with an optional name
  [sheet ((row-spec?) (#:name string?) #:rest (listof row-spec?) . ->* . s:sheet?)]
  
  ;; Specify a reference to a nanemd cell
  [ref   (string? . -> . id-ref?)]))

;; ---------------------------------------------------------------------------------------------------

(struct named-cell-spec (contents id) #:transparent)  ; what is produced by (cell v #:id name)
(struct anon-cell-spec  (contents) #:transparent)     ; what is produced by (cell v)
(struct row-spec (cells ids) #:transparent)           ; what is produced by (row ...)
(struct id-ref (id) #:transparent)

(define cell-spec/c
  (or/c s:atomic-value? named-cell-spec? anon-cell-spec? 'nothing))

(define (cell-expr? x)
  (cond
    [(s:atomic-value? x) #t]
    [(id-ref? x)         #t]
    [(pair? x)           (and (symbol? (car x))
                              (andmap cell-expr? (cdr x))) ]
    [else                #f]))

(define cell
  (case-lambda
    [()     'nothing]
    [(v)    (anon-cell-spec v)]
    [(v id) (named-cell-spec v id)]))

;; ref : string? -> id-ref?
(define (ref id)
  (id-ref id))

;; row : (List-of <cell-spec>) -> <row-spec>
;; Traverse cells, producing a list of unparsed-cells and a list of extracted names with their indexes
(define (row . cells)
  (let-values ([(cols cells ids) (row-iter cells)])
    (row-spec cells ids)))

;; row-iter : List-of cell -> [values max-index cells names]
;; Turn named cells into unnamed cells and extract the names
(define (row-iter cell-specs)
  (for/fold ([col-index 0]
             [cells     null]
             [ids       null]
             #:result (values col-index (reverse cells) ids))
            ([c cell-specs])
    (let-values ([(anon-cell maybe-id) (parse-cell-spec c)])
      (values (+ col-index 1)
              (cons anon-cell cells)
              (if maybe-id
                  (cons (cons maybe-id col-index) ids)
                  ids)))))
  
;; parse-cell-spec : <cell-spec> -> values anon-cell-spec? (or/c string? #f)
;; Parse an entry in a row, pulling out the name if there is one
(define (parse-cell-spec v)
  (cond
    [(s:atomic-value? v)
     (values (anon-cell-spec v) #f)]
    [(anon-cell-spec? v)
     (values v #f)]
    [(named-cell-spec? v)
     (values (anon-cell-spec (named-cell-spec-contents v))
             (named-cell-spec-id v))]))

;; Two-pass sheet parser. The first pass extracts the names; the second pass makes
;; a sheet? and fills in the cell references
;; There must be at least one row in the call to sheet. 
(define (sheet #:name [name "unnamed-sheet"] first-row-spec . rest-row-specs)
  (let-values ([(n-rows rows ids) (sheet-iter (cons first-row-spec rest-row-specs))])
    (let ([maybe-dup (duplicated-id ids)])
      (if maybe-dup
          (raise-argument-error 'sheet "Duplicate named reference" maybe-dup)
          (s:sheet (make-dereferenced-array rows ids) null name)))))

;; duplicated-id : (list-of (name ref)) -> name
(define (duplicated-id ids)
  (check-duplicates ids #:key car #:default #f))

;; make-dereferenced-array : (list-of <rowspec>) (list-of (name ref)) -> array-of cell?
(define (make-dereferenced-array rows ids)
  ;; convert all the anon-cells to cells
  (let ([cells (map (λ (r) (map (anon-cell/ids->cell ids) (row-spec-cells r))) rows)])
    (list*->array cells s:cell?)))

;; anon-cell/ids->cell : (list-of (name ref)) -> anon-cell -> cell?
;; Turn something made by (cell ...) into an actual cell, replacing any named references with a
;; reference
(define ((anon-cell/ids->cell ids) ac)
  (s:cell ((expr/ids->cell-expr ids) (anon-cell-spec-contents ac))))

;; epxr/ids->cell-expr : -> cell-expr?
(define ((expr/ids->cell-expr ids) expr)
  (cond
    [(s:atomic-value? expr) (s:cell-value-return expr)]
    [(id-ref? expr)         (reference-to (id-ref-id expr) ids)]
    [(pair? expr)           (s:cell-app (car expr) (map (expr/ids->cell-expr ids) (cdr expr)))]))

;; reference-to : string? (listof string? int int) -> cell-addr? 
;; Look up the reference in ids, replace with address found therein
(define (reference-to name ids)
  (let ([index (cdr (assoc name ids))])
    (s:cell-addr (car index) (cadr index) #t #t)))

;; sheet-iter : list-of row-spec -> values n-rows (list-of (list-of cell)) ids
;; Collate the names from the rows, adding a row index
(define (sheet-iter row-specs)
  (for/fold ([row-index 0]
             [rows      null]
             [ids       null]
             #:result (values row-index (reverse rows) ids))
            ([r row-specs])
    (let ([refs (add-row-index row-index (row-spec-ids r))])
      (values (+ row-index 1) (cons r rows) (append refs ids)))))

;; add-row-index : number? (list-of (name . col-index))
(define (add-row-index index ids)
  (map
   (λ (col-ref) (cons (car col-ref) (list index (cdr col-ref))))
   ids))
