#lang racket/base

#|

Evaluate a sheet as if it were a spreadsheet, returning an array of atomic values.

TODO
 - does not implement all builtins
 - in particular, "empty cells" used in calculations are replaced with 0, rather
   than something appropriate to the builtin function
 - builtins are not consistent with the module builtins.rkt 
 - currently, there are no cell values of type "error" (errors will cause “run-time” errors)
 - does not implement cell ranges
|#

(require math/array
         racket/match
         racket/contract)

(require "sheet.rkt"
         "builtins.rkt")

(provide
 (contract-out
  [sheet-eval (sheet? . -> . array?)])) 

;; ---------------------------------------------------------------------------------------------------

;; UNDEFINED is a value that is not otherwise possible as the value of a cell
(define UNDEFINED 'undefined) 

;; "Evaluate" a sheet: compute the value of each cell, and convert each cell to
;; an atomic value, just like a spreadsheet does.

;; memoise : mutable-array? vector? any/c -> void
(define (memoise! memo idx val)
  (array-set! memo idx val))

;; sheet-eval : sheet? -> [array-of atomic-value?]
(define (sheet-eval sheet)
  (let* ([shape (array-shape (sheet-cells sheet))]
         [atomic-vals (array->mutable-array (make-array shape UNDEFINED))])
    (begin
      (for ((idx (in-array-indexes shape)))
        (when (eq? (array-ref atomic-vals idx) UNDEFINED)
          (cell-eval/memoise sheet idx atomic-vals)))
      atomic-vals)))

;; cell-eval : sheet? index? mutable-array? -> atomic-value?
;; Evaluate a cell, memoising the result
(define (cell-eval/memoise sheet idx memo)
  (memoise! memo
            idx
            (atomise (cell-eval sheet idx memo))))

;; cell-eval : sheet? index? mutable-array? -> simple-cell-value?
;; Evaluate a cell, using the memoised version if available
(define (cell-eval sheet idx memo)
  (let ([maybe-memoised (array-ref memo idx)])
    (if (not (eq? maybe-memoised UNDEFINED))
        (cell-value-return maybe-memoised)
        (expr-eval sheet (sheet-ref sheet idx) memo))))

;; expr-eval : sheet? cell-expr? mutable-array? -> cell-value?
;; Evaluate an expression
(define (expr-eval sheet expr memo)
  (cond
    ;; Value: evaluates to itself
    [(cell-value? expr) expr]
    
    ;; Address: follow the reference; return the atomic value of that cell 
    [(cell-addr? expr) (cell-eval sheet (addr-index expr) memo)]

    ;; Range : Return the array of atomic values obtained after following each
    ;;         of the refs in the range. 
    [(cell-range? expr)
     (cell-value
      (build-array (range-extent expr)
                   (λ (idx)
                     (cell-eval sheet (addr-offset (cell-range-tl expr) idx) memo))))]

    ;; Name : resolve name to a cell-ref, and start again
    [(cell-name? expr)
     (expr-eval sheet (sheet-resolve-name sheet (cell-name-id expr)) memo)]

    ;; cell-app : apply built-in function to the cell-values of the arguments
    [(cell-app? expr)
     (let ([arg-vals (map (λ (arg) (expr-eval sheet arg memo)) (cell-app-args expr))])
       (apply (get-racket-builtin (cell-app-builtin expr)) arg-vals))]))


;;; Built-in function
;;; --------------------------------------------------------------------------------

;; get-racket-builtin : symbol? -> procedure?
(define (get-racket-builtin f)
  (let ((builtin (assoc f racket-builtins)))
    (if builtin
        (cdr builtin)
        (raise-argument-error 'get-racket-builtin "builtin" f))))

;; unmaybe : atomic-value? -> atomic-value?
;; Replace 'nothing with 0, otherwise return the argument
(define (unmaybe v)
  (if (nothing? v) 0 v))

;; builtin-unary : procedure? -> cell-value? -> cell-value?
;; 'nothing is treated as zero
(define ((builtin-unary op) v)
  (cell-value-return
   (op (unmaybe (atomise v)))))

;; builtin-binary : procedure? -> (cell-value? cell-value?) -> cell-value?
;; 'nothing is treated as zero
(define ((builtin-binary op) x y)
  (cell-value-return
   (op (unmaybe (atomise x))
       (unmaybe (atomise y)))))

;; builtin-fold : procedure? atomic-value? -> cell-value? -> cell-value?
;; 'nothing is replaced by 0
;; The input cell value is an array, which is /not/ atomised
(define ((builtin-fold op x0) vs)
  (cell-value-return
   (array-all-fold (array-map unmaybe (cell-value-elements vs)) op x0)))

(define (if-fn test then else)
  (if test then else))


(define builtin-sum       (builtin-fold   + 0))
(define builtin-max       (builtin-fold   max 'nothing))
(define builtin-min       (builtin-fold   min 'nothing))

(define builtin-=         (builtin-binary =))
(define builtin-<         (builtin-binary <))
(define builtin-<=        (builtin-binary <=))
(define builtin->         (builtin-binary >))
(define builtin->=        (builtin-binary >=))

(define builtin-+         (builtin-binary +))
(define builtin-*         (builtin-binary *))
(define builtin--         (builtin-binary -))
(define builtin-/         (builtin-binary /))
(define builtin-quotient  (builtin-binary quotient))
(define builtin-remainder (builtin-binary remainder))
(define builtin-modulo    (builtin-binary modulo))
(define builtin-power     (builtin-binary expt))

(define builtin-log10     (builtin-unary (λ (x) (log x 10))))

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

(define (builtin-if test then else)
  (cell-value-return (if-fn (unmaybe (atomise test))
                            (unmaybe (atomise then))
                            (unmaybe (atomise else)))))

(define (builtin-halt)
  (cell-value-return +nan.0))

(define racket-builtins
  `([+         . ,builtin-+        ]
    [*         . ,builtin-*        ]
    [max       . ,builtin-max      ]
    [min       . ,builtin-min      ]

    [=         . ,builtin-=        ]
    [<         . ,builtin-<        ]
    [<=        . ,builtin-<=       ]
    [>         . ,builtin->        ]
    [>=        . ,builtin->=       ]
    
    [-         . ,builtin--        ]
    [/         . ,builtin-/        ]
    [quotient  . ,builtin-quotient ]
    [remainder . ,builtin-remainder]
    [modulo    . ,builtin-modulo   ]
    [expt      . ,builtin-power    ]
    
    [log10     . ,builtin-log10    ]
    
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
    [random    . ,builtin-random   ]

    [if        . ,builtin-if       ]
    
    [halt      . ,builtin-halt     ]))
