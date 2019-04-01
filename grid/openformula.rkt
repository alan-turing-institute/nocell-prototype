#lang racket/base

#|

Generate (a small subset of) OpenFormula format formulae from cell-values.  OpenFormula is the formula
language used within OpenDocument spreadsheets [1].

[1] "Open Document Format for Office Applications (OpenDocument)
    Version 1.2, part 2: Recalculated Formula (OpenFormula)
    Format", OASIS, September 2011

TODO
- Only a limited set of functions are supported

|#

(require (only-in racket/string string-join)
         (only-in racket/list range)
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


(define (fmt-infix op a1 a2)
  (format "(~a~a~a)" a1 op a2))

(define (fmt-prefix fn-name as)
  (string-join as ";"
               #:before-first (string-append fn-name "(")
               #:after-last ")"))

;; symbol? (List-of cell-expr?) -> string?
(define (format-app builtin fmt-args)
  (match `(,builtin . ,fmt-args)
    [(list op x y)      #:when (memq op '(+ - * /))
                        (fmt-infix op x y)]
    [(list 'expt x y)   (fmt-infix "POWER" x y)]
    [(list 'modulo x y) (fmt-infix "MOD" x y)]
    [(list 'truncate x) (fmt-infix "TRUNC" x)]
    [(list '+ xs ...)   (fmt-prefix "SUM" xs)]
    [(list '* xs ...)   (fmt-prefix "PRODUCT" xs)]
    [(list fn xs ...)   (fmt-prefix (string-upcase (symbol->string fn)) xs)]
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
;; Functions

;; 


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
      (cell-app 'sqrt )))))
