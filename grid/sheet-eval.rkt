#lang racket
(require math/array)

(require "sheet.rkt")

(provide sheet-eval
         list->sheet
         cell-value-return)

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

(define (sheet-get-cell sheet target [offset #(0 0)])
  (array-ref (sheet-cells sheet) (cell-ref->vector target offset)))

;; resolve-name : sheet? string? -> cell-expr?
;; takes a name and resolves it to its expression within the sheet
;; undefined returns #f
(define (sheet-resolve-name sheet name)
  (cdr (assoc name (sheet-names sheet))))

;; ;; an example builtin function
;; (define (builtin-foo sheet vals origin arg)
;;   (match arg
;;     [(cell-atomic v) v]  ;; atomics/arrays evaluate to themselves 
;;     [(cell-array v) v]
;;     ;; references are followed in the value map (error if not present)
;;     [(cell-ref addr) (array-ref vals (address->vector addr origin))]))

;; take a simple value v and wrap it in a 1x1 array with a single cell
(define (cell-value-return v)
  (cell-value (list->array #(1 1) (list v))))

(define (builtin-foo a b)
  (cell-value-return (+ (array-all-sum (cell-value-elements a))
                        (array-all-sum (cell-value-elements b)))))

;; cell-value? -> is-atomic?
(define (cell-value->atomic cv)
  (array-ref (cell-value-elements cv) #(0 0)))

;; take two vectors comprising a range (top left and bottom right),
;; and return the size of the range (as a vector).  The origin is used
;; to resolve relative references.
;;
;; cell-ref? cell-ref? vector? -> vector?
(define (cell-range-extent tl br origin)
  (vector-map - (cell-ref->vector br origin) (cell-ref->vector tl origin)))

;; sheet-eval : sheet? -> (Mutable-Array is-atomic?)
(define (sheet-eval sheet)
  (let* ([shape (array-shape (sheet-cells sheet))]
         [atomic-vals (array->mutable-array (make-array shape 'undefined))])

    ;; force-one-cell : sheet? address? -> cell-value?
    ;;
    ;; helper function to start the evaluation at one cell, following
    ;; references and making applications until a value can be
    ;; determined for that cell.  The "atomic value" of a cell is
    ;; memoized using the 'atomic-vals' mutable array.
    (define (force-one-cell sheet idx content)
      (match content        
        ;; values (arrays) evaluate to themselves
        [(cell-value elts)
         (begin
           (array-set! atomic-vals idx (cell-value->atomic content))
           content)]

        ;; reference return the atomic value obtained after following the ref)
        [(struct cell-ref _)
         (let* ((target-idx (cell-ref->vector content idx))
                (memo-atomic-val (array-ref atomic-vals target-idx))
                (target-atomic-val
                 (if (eq? memo-atomic-val 'undefined)
                     (cell-value->atomic
                      (force-one-cell sheet
                                      (cell-ref->vector content idx)
                                      (sheet-ref sheet target-idx)))
                     memo-atomic-val)))
           (begin
             (array-set! atomic-vals idx target-atomic-val)
             (cell-value-return target-atomic-val)))]
        
        ;; ranges: return the array of atomic values obtained after
        ;; following each of the refs in the range.  The
        ;; "atomic-value" of a range is a VALUE error.
        [(cell-range tl br)
         (let* ((extent (cell-range-extent tl br idx))
                (val
                 (cell-value
                  (build-array extent
                               (lambda (j)
                                 (cell-value->atomic
                                  (force-one-cell
                                   sheet idx
                                   (cell-ref-vector/+ tl j)))))))
                (atomic-val (if (equal? extent #(1 1))
                                val
                                'VALUE)))
           (array-set! atomic-vals idx atomic-val)
           val)]
        
        ;; resolve names
        [(cell-name name)
         (force-one-cell sheet idx (sheet-resolve-name sheet name))]
        
        ;; builtins
        [(cell-app builtin args)
         (let* ((eval-args (map (lambda (x) (force-one-cell sheet idx x)) args))
                (result (apply builtin-foo eval-args)))
           ;;(apply (get-racket-builtin builtin eval-args)
           (array-set! atomic-vals idx (cell-value->atomic result))
           result)]))

    ;; for each location in the sheet (idx), try an evaluation; return
    ;; the resulting values
    (for ((idx (in-array-indexes shape)))
      (when (eq? (array-ref atomic-vals idx) 'undefined)
        (force-one-cell sheet idx (array-ref (sheet-cells sheet) idx))))
    
    atomic-vals))

;; Construct a sheet from a (once) nested list with the same shape as
;; the intended sheet (the innermost lists varying along the columns
;; of the sheet).  If a value in the list is given as an atomic, it is
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
