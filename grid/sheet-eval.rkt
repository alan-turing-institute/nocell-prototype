#lang racket
(require math/array)

(require "sheet.rkt"
         "builtins.rkt")

(provide sheet-eval)

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


;;; Builtins
;;; --------------------------------------------------------------------------------

(define (builtin-foo a b)
  (cell-value-return (+ (array-all-sum (cell-value-elements a))
                        (array-all-sum (cell-value-elements b)))))
