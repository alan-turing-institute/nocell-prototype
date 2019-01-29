#lang racket/base

#|
A little language for creating sheets

TODO
 - All references are converted to relative cell references and names are removed

<sheet-spec> ::=
  (sheet <row-spec> ... #:name string?)

<row-spec> ::=
   (row <cell-spec> ...)  

<cell-spec> ::=
  atomic-value?
| (cell <expression> [#:name name])
| (empty)

<expression> ::=
    atomic-value?
  | (ref string?) 
  | (list symbol? <expression> ...)
|#

(require
 racket/contract
 math/array
 (only-in racket/list check-duplicates)
 (rename-in "sheet.rkt"
            [sheet sheet-type] ; because we're exporting functions `sheet` and `cell`
            [cell cell-type]))

;; (provide sheet row cell)

;; ---------------------------------------------------------------------------------------------------

(struct named-cell-spec (contents id))
(struct anon-cell-spec  (contents))
(struct row-spec (cells ids))


(define (empty)
  'nothing)

;; cell : (or/c atomic-value? <expression>) -> cell-spec
(define (cell v #:id [id #f])
  (if id
      (named-cell-spec v id)
      (anon-cell-spec v)))

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
             [ids       null])
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
    [(atomic-value? v)
     (values (anon-cell-spec v) #f)]
    [(anon-cell-spec? v)
     (values  v #f)]
    [(named-cell-spec? v)
     (values (anon-cell-spec (named-cell-spec-contents v))
             (named-cell-spec-id v))]))


;; sheet : -> sheet?
;; Two-pass sheet parser. First pass extracts the names; second pass makes a sheet? and
;; fills in the cell references
(define (sheet #:name [name "unnamed-sheet"] . row-specs)
  (let-values ([(n-rows rows ids) (sheet-iter row-specs)])
    (let ([maybe-dup (duplicated-id ids)])
      (if maybe-dup
          (raise-argument-error 'sheet "Duplicate named reference" maybe-dup)
          (sheet-type (make-dereferenced-array rows ids) null name)))))

(define (duplicated-id ids)
  (check-duplicates ids #:key car #:default #f))

(define (make-dereferenced-array rows ids)
  (list*->array (map (to-cell-expr rows ids))))

;; sheet-iter : list-of row-spec -> values n-rows (list-of (list-of cell)) ids
;; Collate the names from the rows, adding a row index
(define (sheet-iter row-specs)
  (for/fold ([row-index 0]
             [rows      null]
             [ids       null])
            ([r row-specs])
    (let ([refs (add-row-index row-index (row-spec-ids r))])
      (values (+ row-index 1) (cons r rows) (append refs ids)))))

;; add-row-index : number? (list-of (name . col-index))
(define (add-row-index index ids)
  (map
   (λ (col-ref) (cons (car col-ref) (list index (cdr col-ref))))
   ids))
