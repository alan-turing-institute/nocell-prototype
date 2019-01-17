#lang racket

(require math/array)

(provide (all-defined-out))
#| 

An abstract representation of spreadsheets

|#

;; A sheet is:
;; - a two-dimensional array? of cells; 
;; - a list of names (of ranges &c); and
;; - some metadata (including a human-readable name) 

;; cells : Array? (specifically, a non-empty array one shape has length 2)
;; names : (assoc? name? cell-expr?) 
(struct sheet (cells names meta) #:transparent)

(define (is-sheet-cells? cs)
  (and (array? cs)
       (equal? (array-dims cs) 2)
       (> (array-size cs) 0)))

;; Construct a sheet from a (once) nested list with the same shape as
;; the intended sheet. The innermost lists varying along the columns
;; of the sheet.  If a value in the list is given as an atomic, it is
;; wrapped as a cell-value.
(define (list->sheet ls)
  (sheet
   (list*->array
    (map (lambda (col)
           (map (lambda (elt)
                  (if (is-atomic? elt)
                      (cell-value-return elt)
                      elt))
                col))
         ls)
    cell-expr?)
   '() '()))

;; A cell is:
;; - A cell value; and 
;; - some metadata (including, perhaps, whatever is used to decide the
;;   formatting of the cell)

;; content : cell-expr?
;; meta    : cell-meta?
(struct cell (content meta) #:transparent)

(struct cell-expr () #:transparent)

;; A cell expression is either:
;; - a value (arrays, perhaps with a single element, of atomic values);
;; - a reference;
;; - a name; or
;; - a function application

;; elements : (Array is-atomic?)
(struct cell-value cell-expr (elements) #:transparent)

;; A cell-ref is a pair of integers (i, j) and for each a boolean
;; which is true if the corresponding reference is relative
(struct cell-ref cell-expr (col row col-rel? row-rel?) #:transparent)

;; A cell-range is a pair of cell-ref (representing the top-left and
;; bottom-right of the range)
(struct cell-range cell-expr (tl br) #:transparent)

;; id : string?
(struct cell-name cell-expr (id) #:transparent)

;; builtin : builtin?
;; args    : List-of cell-expr?
(struct cell-app cell-expr (builtin args) #:transparent)

;; An atomic value is either
;;  - a number;
;;  - a boolean;
;;  - a string; or
;;  - an error
(define (is-atomic? v)
  (or (number? v) (string? v) (boolean? v) (is-error? v) (is-empty? v)))

(define (is-error? err)
  (memq err '(NA VALUE DIV/0 NUM)))

(define (is-empty? v)
  (eq? v 'empty))


;;; Cell Utilities
;;; --------------------------------------------------------------------------------

;; non-finite floating point numbers map to cell errors: identify these
(define (is-nonfinite? x)
  (and (number? x)
       (or (nan? (real-part x))
           (infinite? (real-part x))
           (nan? (imag-part x))
           (infinite? (imag-part x)))))

;; take a value and coerce it to an atomic value, with 'empty becoming
;; the value specified by empty (in some contexts this will remain
;; 'empty, but in arithmetic contexts, this will usually be 0.0
(define (coerce-atomic a [empty 0.0])
  (cond
    [(is-empty? a) empty]
    [(number? a) (exact->inexact a)]
    [(is-nonfinite? a) 'NUM]
    [else a]))

;; a simple value is a cell value whose array contains a single
;; atomic-value.
(define (cell-simple-value? ce)
  (and (cell-value? ce)
       (equal? (array-shape (cell-value-elements ce))
               #(1 1))))

;; take an atomic value v and wrap it in a cell-value containing a 1x1
;; array
(define (cell-value-return v)
  (cell-value (list->array #(1 1) (list (coerce-atomic v 'empty)))))

;; cell-value? -> is-atomic?
(define (cell-value->atomic cv)
  (array-ref (cell-value-elements cv) #(0 0)))

;; cell-ref->vector : cell-ref? (Vector fixnum) -> (Vector fixnum)
(define (cell-ref->vector target [offset #(0 0)])
  (let ((col (if (cell-ref-col-rel? target)
                 (+ (vector-ref offset 0) (cell-ref-col target))
                 (cell-ref-col target)))
        (row (if (cell-ref-row-rel? target)
                 (+ (vector-ref offset 1) (cell-ref-row target))
                 (cell-ref-row target))))
    (vector col row)))

;; cell-ref? vector? -> cell-ref?
(define (cell-ref-vector/+ r v)
  (struct-copy cell-ref r
               [col (+ (cell-ref-col r) (vector-ref v 0))]
               [row (+ (cell-ref-row r) (vector-ref v 1))]))

;; take two vectors comprising a range (top left and bottom right),
;; and return the size of the range (as a vector).  The origin is used
;; to resolve relative references.
;;
;; cell-ref? cell-ref? vector? -> vector?
(define (cell-range-extent tl br origin)
  (vector-map - (cell-ref->vector br origin) (cell-ref->vector tl origin)))


;;; Indexing
;;; --------------------------------------------------------------------------------

;; sheet-shape : sheet? -> vector?
(define (sheet-shape sh)
  (array-shape (sheet-cells sh)))

;; sheet-ref : sheet? non-negative-integer? non-negative-integer? -> cell-value?
(define (sheet-ref sh i j)
  (array-ref (sheet-cells sh) (list i j)))

;; sheet-get-cell : sheet? cell-ref? [vector?] -> cell-expr?
(define (sheet-get-cell sheet target [offset #(0 0)])
  (array-ref (sheet-cells sheet) (cell-ref->vector target offset)))

;; resolve-name : sheet? string? -> cell-expr?
;;
;; takes a name and tries to resolve it to its expression within the
;; sheet.  If name is undefined, returns #f
(define (sheet-resolve-name sheet name)
  (cdr (assoc name (sheet-names sheet))))


;;; TODO
;;; --------------------------------------------------------------------------------

;; The list of errors should also have utility functions for using/testing the
;; error values. Don't know what this means.

