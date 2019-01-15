#lang racket
(require math/array)

(require "sheet.rkt"
         "builtins.rkt")

;(provide sheet-eval)
(provide (all-defined-out))

;;; Builtins
;;; --------------------------------------------------------------------------------

(define (get-racket-builtin f)
  (cdr (assoc f racket-builtins)))

;; 'empty treated as zero
(define ((builtin-unary op) x)
  (define (maybe-op x)
    (cond
      [(is-error? x) x]
      [(is-empty? x) (coerce-atomic (op 0))]
      [else (coerce-atomic (op x))]))
  (cell-value-return (maybe-op (cell-value->atomic x))))

(define ((builtin-binary op [default-y 0]) x . y)
  (define (maybe-op x [y default-y])
    (cond
      [(is-error? x) x]
      [(is-error? y) y]
      [(is-empty? x) (coerce-atomic (op 0 y))]
      [(is-empty? y) (coerce-atomic (op x 0))]
      [else (coerce-atomic (op x y))]))
  (cell-value-return
   (apply maybe-op
          (cell-value->atomic x)
          (map cell-value->atomic y))))

;; "foldl f" in Racket is like "foldl (flip f)" in Haskell
(define ((flip f) x y) (f y x))

;; 'empty ignored
(define ((builtin-fold op x0) . xs)
  (define (maybe-op x y)
    (cond
      [(is-error? y) y]
      [(is-empty? x) y]
      [else (coerce-atomic (op x y))]))
  (cell-value-return
   (coerce-atomic
    (foldl (flip maybe-op)
           x0
           (map (lambda (x)
                  (array-all-fold (cell-value-elements x) maybe-op))
                xs)))))

(define builtin-+         (builtin-fold   + 0))
(define builtin-*         (builtin-fold   * 1))
(define builtin-max       (builtin-fold   max 'empty))
(define builtin-min       (builtin-fold   min 'empty))

(define builtin--         (builtin-binary -))
(define builtin-/         (builtin-binary /))
(define builtin-quotient  (builtin-binary quotient))
(define builtin-remainder (builtin-binary remainder))
(define builtin-modulo    (builtin-binary modulo))
(define builtin-power     (builtin-binary expt))

(define builtin-log       (builtin-binary log 10))

(define builtin-abs       (builtin-unary  abs))
(define builtin-floor     (builtin-unary  floor))
(define builtin-ceiling   (builtin-unary  ceiling))
(define builtin-truncate  (builtin-unary  truncate))
(define builtin-sin       (builtin-unary  sin))
(define builtin-cos       (builtin-unary  cos))
(define builtin-tan       (builtin-unary  tan))
(define builtin-asin      (builtin-unary  asin))
(define builtin-acos      (builtin-unary  acos))
(define builtin-atan      (builtin-unary  atan))
(define builtin-sqrt      (builtin-unary  sqrt))
(define builtin-exp       (builtin-unary  exp))

;; "builtin-random" defined separately since it is the only "nullary" builtin
(define (builtin-random) (cell-value-return (random)))


(define racket-builtins
  `([+         . ,builtin-+        ]
    [*         . ,builtin-*        ]
    [max       . ,builtin-max      ]
    [min       . ,builtin-min      ]
    
    [-         . ,builtin--        ]
    [/         . ,builtin-/        ]
    [quotient  . ,builtin-quotient ]
    [remainder . ,builtin-remainder]
    [modulo    . ,builtin-modulo   ]
    [expt      . ,builtin-power    ]
    
    [log       . ,builtin-log      ]
    
    [abs       . ,builtin-abs      ]
    [floor     . ,builtin-floor    ]
    [ceiling   . ,builtin-ceiling  ]
    [truncate  . ,builtin-truncate ]
    [sin       . ,builtin-sin      ]
    [cos       . ,builtin-cos      ]
    [tan       . ,builtin-tan      ]
    [asin      . ,builtin-asin     ]
    [acos      . ,builtin-acos     ]
    [atan      . ,builtin-atan     ]
    [sqrt      . ,builtin-sqrt     ]
    [exp       . ,builtin-exp      ]
    [random    . ,builtin-random   ]))

;;; Eval
;;; --------------------------------------------------------------------------------

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
                (result (apply (get-racket-builtin builtin) eval-args)))
           (array-set! atomic-vals idx (cell-value->atomic result))
           result)]))

    ;; for each location in the sheet (idx), try an evaluation; return
    ;; the resulting values
    (for ((idx (in-array-indexes shape)))
      (when (eq? (array-ref atomic-vals idx) 'undefined)
        (force-one-cell sheet idx (array-ref (sheet-cells sheet) idx))))
    
    atomic-vals))


