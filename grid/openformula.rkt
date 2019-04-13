#lang racket/base

#|

Generate (a small subset of) OpenFormula format formulae from cell-values.  OpenFormula is the formula
language used within OpenDocument spreadsheets [1].

[1] "Open Document Format for Office Applications (OpenDocument)
    Version 1.2, part 2: Recalculated Formula (OpenFormula)
    Format", OASIS, September 2011

TODO
- Only a limited set of functions are supported
- ROUND has subtly different semantics to Racket
- Implement test to ensure all builtins are implemented
|#

(require (only-in racket/string string-join)
         (only-in racket/list
                  flatten
                  range)
         racket/contract
         racket/format
         racket/match
         math/array
         "builtins.rkt"
         "sheet.rkt")

(require rackunit)

(provide
 (contract-out (cell-expr->openformula (cell-expr? . -> . string?))))

;; ---------------------------------------------------------------------------------------------------

(define (transpose arr) (array-axis-swap arr 0 1))

;; cell-expr->openformula : cell-expr? -> string?
(define (cell-expr->openformula expr)
  (match expr
    [(cell-value elements)
     (if (simple-cell-value? expr)
         (format-element (array-ref elements #(0 0)))
         (format-array elements))]
    
    [(struct cell-addr _) (string-append "[" (format-ref expr) "]")]
    
    [(cell-range tl br)
     (string-append "[" (format-ref tl) ":" (format-ref br) "]")]
    
    [(cell-name id)
     (~a id)]
    
    [(cell-app builtin args)
     (let ((fmt-args (map cell-expr->openformula args)))
       (format-app builtin fmt-args))]))


;; format-element : atomic-value? -> string
(define (format-element elt)
  (cond
    [(real? elt) (~a elt)]
    [(string? elt) (format "&quot;~a&quot;" elt)]
    [(boolean? elt) (if elt "TRUE" "FALSE")]
    [(nothing? elt) ""]
    [else (raise-argument-error 'format-element
                                "atomic-value?"
                                elt)]))

(define (format-array arr)
  (let ((rows (array->list* (transpose arr))))
    (string-join
     (map (lambda (elts)
            (string-join
             (map (lambda (elt) (format-element elt))
                  elts)
             ";"))
          rows)
     "|"
     #:before-first "{"
     #:after-last   "}"
     )))


(define (fmt-binary-op op a1 a2)
  (format "(~a~a~a)" a1 op a2))

(define (fmt-unary-op op a)
  (format "(~a~a)" op a))

(define (fmt-fn fn-name as)
  (string-join as ";"
               #:before-first (string-append fn-name "(")
               #:after-last ")"))

(define (fmt-unary-or-binary-op op args)
  (if (= (length args) 1)
      (fmt-unary-op op (car args))
      (fmt-binary-op op (car args) (cadr args))))

;; format-app : symbol? (List-of cell-expr?) -> string?
(define (format-app builtin fmt-args)
  (match builtin
    ;; Lookup and reference
    ['ref               (error "Ref not yet defined")]
    ;; Arithmetic
    ['+                 (fmt-unary-or-binary-op "+" fmt-args)]
    ['-                 (fmt-unary-or-binary-op "-" fmt-args)]
    ['*                 (fmt-binary-op "*" (car fmt-args) (cadr fmt-args))]
    ['/                 (fmt-binary-op "/" (car fmt-args) (cadr fmt-args))]
    ['abs               (fmt-fn "ABS" fmt-args)]
    ['round             (fmt-fn "ROUND" fmt-args)]  ;; note slightly different semantics to Racket when
                                                    ;; resolving ties
    ['truncate          (fmt-fn "TRUNC" fmt-args)]
    ['floor             (fmt-fn "FLOOR" fmt-args)]
    ['ceiling           (fmt-fn "CEILING" fmt-args)]
    ;; Multiple arity
    ['sum               (error "sum not yet defined")]
    ['min               (error "min not yet defined")]
    ['max               (error "max not yet defined")]
    ;; Functions of ranges
    ['range-sum         (error "range-sum not yet defined")]
    ['range-min         (error "range-min not yet defined")]
    ['range-max         (error "range-max not yet defined")]
    ;; Integer artithmetic
    ['quotient          (fmt-fn "QUOTIENT" fmt-args)]
    ['remainder         (format "MODULO(~a;SIGN(~a)*ABS(~a))"
                                (car fmt-args)
                                (car fmt-args)
                                (cadr fmt-args))]
    ['modulo (fmt-fn "MOD" fmt-args)]
    ;; Powers and roots
    ['expt (fmt-binary-op "^" (car fmt-args) (cadr fmt-args))]
    ['exp  (fmt-fn "EXP" fmt-args)]
    ['log (if (= (length (fmt-args) 1))
              (fmt-fn "LN" fmt-args) ; Natural log
              (if (string=? (cadr fmt-args) "10")
                  (fmt-fn "LOG10" (car fmt-args)) ; Log to base 10
                  (fmt-fn "LOG" fmt-args)))] ; Log with arbitrary base
    ['sqrt (fmt-fn "SQRT" fmt-args)]
    ;; Trigonometic functions
    ['sin (fmt-fn "SIN" fmt-args)]
    ['cos (fmt-fn "COS" fmt-args)]
    ['tan (fmt-fn "TAN" fmt-args)]
    ['asin (fmt-fn "ASIN" fmt-args)]
    ['acos (fmt-fn "ACOS" fmt-args)]
    ['atan (fmt-fn "ATAN" fmt-args)]
    ;; Other
    ['random
     (cond
       [(= (length fmt-args) 0) (fmt-fn "RAND" fmt-args)]
       [(= (length fmt-args) 1) (format "RANDBETWEEN(0,(~a-1))" (car fmt-args))]
       [else (format "RANDBETWEEN(~a,(~a-1))" (car fmt-args) (cadr fmt-args))])]
    ))

;; --------------------------------------------------------------------------------
;;
;; convert a numeric (zero-indexed) column reference to and from a
;; column letter (A,...,Z,AA,...,AZ,...,ZZ,AAA,...) 

(define codepoint-A 65)

(define (integer->column-letter n)
  (define (recur rems b)
    (if (< (car rems) b)
        rems
        (let-values ([(q r) (quotient/remainder (car rems) b)])
          (recur (list* (- q 1) r (cdr rems)) b))))
  (list->string
   (map (lambda (d) (integer->char (+ d codepoint-A)))
        (recur (list n) 26))))

(define (column-letter->integer cs)
  (let ((ds (map (lambda (c) (- (char->integer c) codepoint-A))
                 (string->list cs))))
    (- (foldl (lambda (a b) (+ (* 26 b) (+ a 1))) 0 ds) 1)))

;; cell-addr? -> string?
(define (format-ref ref)
  (let ((col-$ (if (cell-addr-col-is-rel ref) "" "$"))
        (row-$ (if (cell-addr-row-is-rel ref) "" "$"))
        (col-A (integer->column-letter (cell-addr-col ref)))
        (row-1 (add1 (cell-addr-row ref))))
    (format ".~a~a~a~a" col-$ col-A row-$ row-1)))



;; ---------------------------------------------------------------------------------------------------
;; Tests

;; Check we can write column names correctly
(module+ test
  (test-case "Column letter"
    (check-equal? (integer->column-letter 0)   "A")
    (check-equal? (integer->column-letter 1)   "B")
    (check-equal? (integer->column-letter 25)  "Z")
    (check-equal? (integer->column-letter 26)  "AA")
    (check-equal? (integer->column-letter 701) "ZZ")
    (check-equal? (integer->column-letter 702) "AAA")
    (check-equal? (integer->column-letter 728) "ABA")
    (for-each
     (lambda (n)
       (with-check-info
         (('current-element n))
         (check-equal? n
                       (column-letter->integer
                        (integer->column-letter n)))))
     (range 0 10000))))

;; Test cell-value 
(module+ test
  (test-case "Openformula: Values"
    ;; real?
    (check-equal?
     (cell-expr->openformula
      (cell-value-return 42))
     "42"))
  ;; string? 
  (check-equal?
   (cell-expr->openformula
    (cell-value-return "Forty-two"))
   "&quot;Forty-two&quot;")
  ;; boolean?
  (check-equal?
   (cell-expr->openformula
    (cell-value-return #t))
   "TRUE")
  (check-equal?
   (cell-expr->openformula
    (cell-value-return #f))
   "FALSE")
  ;; nothing?
  (check-equal?
   (cell-expr->openformula
    (cell-value-return 'nothing))
   "")
  ;; not simple-cell-value?
  (check-equal?
   (cell-expr->openformula
    (cell-value (array #[ #[1 2 3] #[4 5 6]])))
   "{1;4|2;5|3;6}"))

;; Test cell-ref
(module+ test
  (test-case "Openformula: References"
    ;; cell-addr?
    (check-equal?
    (cell-expr->openformula
     (cell-addr 0 0 #t #t))
    "[.A1]")
   (check-equal?
    (cell-expr->openformula
     (cell-addr 1 26 #t #f))
    "[.$AA2]")
   (check-equal?
    (cell-expr->openformula
     (cell-addr 2 (* 26 27) #f #f))
    "[.$AAA$3]"))
  ;; cell-range?
  (check-equal?
   (cell-expr->openformula
    (cell-range (cell-addr 0 0 #t #t)
                (cell-addr 1 1 #f #f)))
   "[.A1:.$B$2]"))

;; Test cell-name
;; TODO

;; Test cell-app
(module+ test
  (test-case "Openformula: Applications"
    (check-equal?
     (cell-expr->openformula
      (cell-app
       'sqrt (list (cell-app
                    '+ (list (cell-value-return 2)
                             (cell-value-return 2))))))
     "SQRT((2+2))")))

