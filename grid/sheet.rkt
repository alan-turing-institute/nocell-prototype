#lang racket/base

#| 
TODO
 - lists->sheet and sheet->lists do not handle names. Probably we should get rid of these.
 - possibly this is an internal module, and we can get rid of all the contracts ...
 - at the moment, there are no constructors of the basic structs. We can let grid handle that?
|#

;; ---------------------------------------------------------------------------------------------------

(require math/array
         racket/contract
         (only-in racket/math nan? infinite?)
         (only-in racket/vector vector-map))

(provide
 (contract-out
  
  ;; Types
  [struct sheet                  ((cells array2d?)
                                  (refs (listof pair?))
                                  (meta (listof any/c))
                                  (name (or/c string? #f)))]
  [struct cell                   ((content cell-expr?))]
  [struct cell-expr              ()]
  [struct (cell-name  cell-expr) ((id string?))]
  [struct (cell-app   cell-expr) ((builtin symbol?) (args (listof cell-expr?)))]
  [struct (cell-value cell-expr) ((elements array2d?))]
  [struct (cell-ref   cell-expr) ()]
  [struct (cell-addr cell-ref)
    ((row exact-nonnegative-integer?)
     (col exact-nonnegative-integer?)
     (row-is-rel boolean?)
     (col-is-rel boolean?))]
  [struct (cell-range cell-ref) ((tl cell-addr?) (br cell-addr?))]

  ;; Predicates
  [nothing?           (any/c . -> . boolean?)]
  [atomic-value?      (any/c . -> . boolean?)]
  [simple-cell-value? (any/c . -> . boolean?)]

  ;; Constructors and conversions 
  [cell-value-return (atomic-value? . -> . simple-cell-value?)]
  [atomise (cell-value? . -> . atomic-value?)] 
  
  [lists->sheet ((listof (listof (or/c atomic-value? cell-expr?))) . -> . sheet?)]
  [sheet->lists (sheet? . -> . (listof (listof (or/c atomic-value? cell-expr?))))]

  ;; Indexing and reference
  [range-extent (cell-range? . -> . range-extent/c)]
  [sheet-resolve-name (sheet? string? . -> . (or/c cell-ref? #f))]
  [addr-index (cell-addr? . -> . sheet-index/c)]
  [addr-offset (cell-addr? sheet-index/c . -> . sheet-index/c)]
  [sheet-ref (sheet? sheet-index/c . -> . cell-expr?)]
  ))


;; A 2-dimensional, non-empty, array
(define (array2d? v)
  (and
   (array? v)
   (> (array-size v) 0)
   (= (array-dims v) 2)))

;; An index into a sheet
(define sheet-index/c
  (vector/c exact-nonnegative-integer? exact-nonnegative-integer?))

;; The possible size of a range
(define range-extent/c
  (vector/c exact-positive-integer? exact-positive-integer?))

#| 

An abstract representation of a single sheet in a spreadsheet.

A "sheet" is:
 - a two-dimensional array? of cells; 
 - a list of names of ranges (not implemented); and
 - some metadata (not implemented)

A "cell" is:
 - an expression; and
 - some other things (not implemented)

An "expression" is either:
 - a value
 - an application of a builtin
 - a reference; or
 - a name

A "reference" is either:
 - a single cell address; or
 - a rectangular range of cells

A "value" is an array of atomic-value

An "atomic value" is either:
 - a real number
 - a string
 - a boolean; or
 - nothing

Note that a cell with a single number in it is represented as a cell containing
a 1x1 array of numbers.

|#

#|
Notes on design (JG)

I don't really know whether this module should just be the definition of sheet
types as well as some utilities; or whether it should be the interface to the
sheet type and hide all the interals of the structs. For now, it's the former.
|#

(struct sheet (cells refs meta name) #:transparent)
(struct cell (content) #:transparent)
(struct cell-expr () #:transparent)
(struct cell-app cell-expr (builtin args) #:transparent)
(struct cell-value cell-expr (elements) #:transparent)
(struct cell-name cell-expr (id) #:transparent)
(struct cell-ref cell-expr () #:transparent)

;; `col` and `row` are always absolute indices of the referent; the flags
;; `col-is-rel` and `row-is-rel` affect only the output 
(struct cell-addr cell-ref (row col row-is-rel col-is-rel) #:transparent)

;; `tl` and `br` are the top-left and bottom-right of the range
(struct cell-range cell-ref (tl br) #:transparent)


(define (atomic-value? v)
  (or
   (real? v)
   (string? v)
   (boolean? v)
   (nothing? v)))

(define (nothing? v)
  (eq? v 'nothing))


;;; --------------------------------------------------------------------------------
;;; Conversions

;; A simple value is a cell value whose array contains a single
;; (atomic) value.
(define (simple-cell-value? ce)
  (and (cell-value? ce)
       (= (array-size (cell-value-elements ce)) 1)))

;; Make a cell value (a 1x1 array) from an atomic value 
(define (cell-value-return v)
  (cell-value (list->array #(1 1) (list v))))

;; Pick out the top-left element of a cell-value
(define (atomise cv)
  (array-ref (cell-value-elements cv) #(0 0)))

;; Atomise the cell value if it's simple, leave it alone if not
(define (maybe-atomise cv)
  (if (simple-cell-value? cv)
      (atomise cv)
      cv))

;; Construct a sheet from a list of lists having the same shape as the intended
;; sheet. Innermost lists represent rows, and vary along the columns of the
;; sheet. If a value in the list is given as an atomic, it is wrapped as a
;; cell-value, otherwise it is assumed to be a valid cell-expr.
(define (lists->sheet ls)
  (sheet
   ;; sheet-cells
   (list*->array
    (map (lambda (row)
           (map (lambda (elt)
                  (if (atomic-value? elt)
                      (cell-value-return elt)
                      elt))
                row))
         ls)
    cell-expr?)
   ;; sheet-refs
   '()
   ;; sheet-meta
   '()
   ;; sheet-name
   #f
   ))

;; From a sheet, produce a list of lists, dropping the names. If a cell contains
;; a simple value it is atomised. (This function is the inverse of lists->sheet.)
(define (sheet->lists sht)
  (array->list* (array-map maybe-atomise (sheet-cells sht))))

;;; Indexing
;;; --------------------------------------------------------------------------------

;; Resolves a name to a cell-ref expression within the sheet. If name is
;; undefined, returns #f
(define (sheet-resolve-name sheet name)
  (let ([ref (assoc name (sheet-refs sheet))])
    (and ref (cdr ref))))

;; Returns the coordinates in a cell-addr as a 2-element vector 
(define (addr-index addr)
  (vector-immutable (cell-addr-row addr) (cell-addr-col addr)))

;; Return the cell-expr at a specific index in the sheet
(define (sheet-ref sheet idx)
  (cell-content (array-ref (sheet-cells sheet) idx)))

;; Compute an address offset from an address by a vector
(define (addr-offset addr idx)
  (vector-map + (addr-index addr) idx))

;; Return the size of a cell-range (number of rows and columns)
(define (range-extent rng)
  (vector-map +
              #(1 1)
              (vector-map -
                          (addr-index (cell-range-br rng))
                          (addr-index (cell-range-tl rng)))))

 


