#lang racket

(require math/array)

#| 

An abstract representation of spreadsheets

|#

;; A sheet is:
;; - a two-dimensional array? of cells; 
;; - a list of names (of ranges); and
;; - some metadata (including a human-readable name) 

;; cells : Array? (specifically, a non-empty array one shape has length 2)
;; names : List-of range-name? 
(struct sheet (cells names meta))

;; A cell is:
;; - A cell value; and 
;; - some metadata (including, perhaps, whatever is used to decide the
;;   formatting of the cell)

;; value : cell-value?
;; meta : cell-meta?
(struct cell (value meta))

;; A cell value is either:
;; - empty (represented by #f); or
;; - a cell expression

;; A cell expression is either:
;; - a literal; 
;; - a range;
;; - a reference;
;; - an error; or
;; - a function application

(struct cell-expression ())

(struct cell-literal   cell-expression (value))   ; value : cell-literal-value?
(struct cell-range     cell-expression (address)) ; address : (or/c? cell-address? range-address?)
(struct cell-ref       cell-expression (name))    ; name : string?
(struct cell-error     cell-expression (type))    ; type : cell-error-type? 
(struct cell-app       cell-expression (builtin args)) ; builtin : builtin?
                                                       ; args    : List-of cell-expression?

(define (cell-value? v)
  (or (not v) (cell-expression? v)))

(define (cell-literal-value? v)
  (or (number? v) (string? v) (boolean? v)))

(define (cell-error-type? err)
  (memq err '(NA DIV/0)))

;; A cell-address is a pair of integers (i, j) and for each a boolean which is
;; true if the corresponding reference is relative
(struct cell-address (col row col-rel? row-rel?))

;; A range-address is a pair of cell-address (representing the top-left and
;; bottom-right of the range)
(struct range-address (tl br))


;;; Indexing
;;; --------------------------------------------------------------------------------

;; sheet-ref : sheet? non-negative-integer? non-negative-integer? -> cell-value? 
(define (sheet-ref sh i j)
  (array-ref (sheet-cells sh) (list i j)))


;;; Builtins
;;; --------------------------------------------------------------------------------





;;; TODO
;;; --------------------------------------------------------------------------------

;; The list of errors should also have utility functions for using/testing the
;; error values. Don't know what this means.

