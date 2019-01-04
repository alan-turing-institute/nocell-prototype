#lang racket

(require math/array)

#| 

An abstract representation of spreadsheets

|#

;; A sheet is:
;; - a two-dimensional array? of cells; 
;; - a list of names (of ranges &c); and
;; - some metadata (including a human-readable name) 

;; cells : Array? (specifically, a non-empty array one shape has length 2)
;; names : (assoc? name? cell-expr?) 
(struct sheet (cells names meta))

;; A cell is:
;; - A cell value; and 
;; - some metadata (including, perhaps, whatever is used to decide the
;;   formatting of the cell)

;; content : cell-content?
;; meta : cell-meta?
(struct cell (content meta))

;; The cell content is either:
;; - empty (represented by #f); or
;; - a cell expression

(define (is-content? v)
  (or (not v) (cell-expr? v)))

(struct cell-expr ())

;; A cell expression is either:
;; - a value; 
;; - a name; or
;; - a function application

(struct cell-value   cell-expr ())
(struct cell-name    cell-expr (id))           ; id      : string?
(struct cell-app     cell-expr (builtin args)) ; builtin : builtin?
                                               ; args    : List-of cell-expr?

;; A cell value is either
;; - an atomic value;
;; - an array of atomic values;
;; - a reference

(struct cell-atomic  cell-value (value))       ; value   : is-atomic?
(struct cell-ref     cell-value (address))     ; address : or/c? cell-address? range-address?   
(struct cell-array   cell-value (cells))       ; cells   : (Array is-atomic?) 

;; An atomic value is either
;; - a number;
;; - a boolean;
;; - a string; or
;; - an error

(define (is-atomic? v)
  (or (number? v) (string? v) (boolean? v) (is-error? v)))

(define (is-error? err)
  (memq err '(NA DIV/0)))

;; A cell reference is an address, which is either a cell or a range

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

(define (is-builtin? id)
  (assq id builtins))

;; -> sequence-of symbol?
(define (builtins)
  (in-dict-keys builtins0))

(define (builtin-min-args id)
  (let ([nargs (cdr (assq id builtins))])
    (if (cons? nargs)
        (car nargs)
        nargs)))

;; Returns null if max args is unbounded
(define (builtin-max-args id)
  (let ([nargs (cdr (assq id builtins))])
    (if (cons? nargs)
        (cdr nargs)
        nargs)))

(define (unary-builtin id)
  (cons id 1))

(define (binary-builtin id)
  (cons id 2))

(define number-builtins
  (append
   '([+ . (1)]
     [- . (1)]
     [log . (1 . 2)])
   (map binary-builtin
        '(* /
          quotient remainder modulo
          expt))
   (map unary-builtin
        '(abs max min floor ceiling truncate 
          sin cos tan
          asin acos atan
          random
          sqrt exp log10))))

(define builtins0
  (append
   number-builtins
   ;; more go here ...
   ))

;;; TODO
;;; --------------------------------------------------------------------------------

;; The list of errors should also have utility functions for using/testing the
;; error values. Don't know what this means.

