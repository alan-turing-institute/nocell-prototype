#lang racket
(require math/array)

(require "grid.rkt")


(provide grid-eval)

;; take a sheet where cells have the most general type (cell-expr),
;; and evaluate each cell so that it has type cell-value.
;; (define (eval-sheet sheet)
;;   ;(sheet-cells )
;;   'todo)

;; take an evaluated sheet (after the application of eval, where each
;; cell has type cell-value), and "fully evaluate" it, so that each
;; cell has type "cell-atomic"
(define (cell-eval sheet)
  'todo)

(define (grid-eval sheet)
  (let ((evaluated-sheet (eval sheet)))
    (sheet-cells (cell-eval evaluated-sheet))))

(define (A1)
  (cell-address 0 0 #f #f))

;; lookup a cell by address, where origin is used for the origin for
;; relative addressing

(define (address-to-vector target [origin (A1)])
  (let ((col (if (cell-address-col-rel? target)
                 (+ (cell-address-col origin) (cell-address-col target))
                 (cell-address-col target)))
        (row (if (cell-address-row-rel? target)
                 (+ (cell-address-row origin) (cell-address-row target))
                 (cell-address-row target))))
    (vector col row)))

(define (sheet-get-cell sheet target [origin (A1)])
  (array-ref (sheet-cells sheet) (address-to-vector target origin)))

;; resolve-name : sheet? string? -> cell-expr?
;; takes a name and resolves it to its expression within the sheet
;; undefined returns #f
(define (sheet-resolve-name sheet name)
  (cdr (assoc name (sheet-names sheet))))

;; an example builtin function
(define (builtin-foo sheet vals origin arg)
  (match arg
    [(cell-atomic v) v]  ;; atomics/arrays evaluate to themselves 
    [(cell-array v) v]
    ;; references are followed in the value map (error if not present)
    [(cell-ref addr) (array-ref vals (address-to-vector addr origin))]))

;; sheet-eval : sheet? -> (Array is-atomic?)
(define (sheet-eval sheet)
  ;; atomic-vals contains cell-atomic
  (let* ([shape (array-shape (sheet-cells sheet))]
         [atomic-vals (array->mutable-array
                       ;; should really initialize to an error value
                       ;; and explicitly set to empty when referenced
                       (make-array shape #f))])
    (define (evaluate-content sheet my-addr)
      (let ((content (sheet-get-cell sheet my-addr)))
        (match content
          ;; empty -> empty
          [#f content]

          ;; atomic values evaluate to themselves
          [(cell-atomic v)
           (begin
             (array-set! atomic-vals my-addr content)
             content)]

          ;; references are followed to find their atomic value, but
          ;; return themselves
          [(cell-ref addr)
           (begin
             (array-set! atomic-vals my-addr
                        (evaluate-content
                         sheet addr
                         (sheet-get-cell sheet addr my-addr)))
             content)]

          ;; arrays evaluate to themselves and fully-evaluate to their
          ;; top-left element
          [(cell-array cells)
           (begin
             (array-set! atomic-vals my-addr (array-ref cells #(0 0)))
             content)]
          
          ;; resolve names (not refs!) then continue
          [(cell-name name)
           (evaluate-content sheet my-addr
                             (sheet-resolve-name sheet name))]

          ;; builtins
          [(cell-app builtin args)
           (let ((eval-args
                  (map (lambda (x)
                         (evaluate-content sheet my-addr x))
                       args)))
             (evaluate-content sheet my-addr
                               ;;(apply (get-racket-builtin builtin)
                               (apply builtin-foo 
                                      sheet atomic-vals my-addr
                                      eval-args)))])))

    ;; for each idx location in the sheet, try an evaluation; return
    ;; the resulting values
    (for ((addr (in-array-indexes shape)))
      (evaluate-content sheet addr (sheet-get-cell sheet addr)))
    atomic-vals))
