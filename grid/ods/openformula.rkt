#lang racket

;; Parse and generate (a small subset of) OpenFormula format formulae
;; from cell-values.  OpenFormula is the formula language used within
;; OpenDocument spreadsheets [1].
;;
;; [1] "Open Document Format for Office Applications (OpenDocument)
;;      Version 1.2, part 2: Recalculated Formula (OpenFormula)
;;      Format", OASIS, September 2011

(require math/array
         "../sheet.rkt")

(provide cell-expr->openformula)

(define (transpose arr) (array-axis-swap arr 0 1))

;; format-element : is-atomic? -> string
(define (format-element elt)
  (cond
    [(number? elt) (~a elt)]
    [(string? elt) (format "&quot;~a&quot;" elt)]
    [(boolean? elt) (if elt "TRUE" "FALSE")]
    [(is-empty? elt) ""]
    [(is-error? elt)
     (case elt
       [(NA)    "#NA"]
       [(VALUE) "#VALUE!"]
       [(DIV/0) "#DIV/0!"]
       [(NUM)   "#NUM!"]
       [else (raise-arguments-error 'format-element
                                    "invalid error case encountered"
                                    "got" elt)])]
    [else (raise-argument-error 'format-element
                                "is-atomic?"
                                elt)]))

;; e.g.
;; (format-array-openformula (array #[#[1 2 3] #[4 5 6]]))
;;      ==> "{1;4|2;5|3;6}"
;;
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

;; symbol? (List cell-expr?) -> string?
(define (format-app builtin fmt-args)
  (match `(,builtin . ,fmt-args)
    [(list op x y) #:when (memq op '(+ - * /))
                   (fmt-infix op x y)]
    [(list 'expt x y) (fmt-infix "POWER" x y)]
    [(list 'modulo x y) (fmt-infix "MOD" x y)]
    [(list 'truncate x) (fmt-infix "TRUNC" x)]
    [(list '+ xs ...) (fmt-prefix "SUM" xs)]
    [(list '* xs ...) (fmt-prefix "PRODUCT" xs)]
    [(list fn xs ...) (fmt-prefix (string-upcase (symbol->string fn)) xs)]
    ))

;; convert a numeric (zero-indexed) column reference to a column
;; letter (A,...,Z,AA,...,AZ,...,ZZ,AAA,...)
;; 0 => "A", 25 => "Z", 26 => "AA", 27 => "AB", ...
(define (integer->column-letter n)
  (define (recur rems b)
    (if (< (car rems) b)
        rems
        (let-values ([(q r) (quotient/remainder (car rems) b)])
          (recur (list* (- q 1) r (cdr rems)) b))))
  (let ((codepoint-A 65))
    (list->string
     (map (lambda (d) (integer->char (+ d codepoint-A)))
          (recur (list n) 26)))))

;; cell-ref? -> string?
(define (format-ref ref)
  (let ((col-$ (if (cell-ref-col-rel? ref) "$" ""))
        (row-$ (if (cell-ref-row-rel? ref) "$" ""))
        (col-A (integer->column-letter (cell-ref-col ref)))
        (row-1 (add1 (cell-ref-row ref))))
    (format ".~a~a~a~a" col-$ col-A row-$ row-1)))

;; cell-expr->openformula : cell-expr? -> string?
(define (cell-expr->openformula expr)
  (match expr
    [(cell-value elements)
     (if (cell-simple-value? expr)
         (format-element (array-ref elements #(0 0)))
         (format-array elements))]
    
    [(struct cell-ref _) (string-append "[" (format-ref expr) "]")]
    
    [(cell-range tl br)
     (string-append "[" (format-ref tl) ":" (format-ref br) "]")]
    
    [(cell-name id) (~a id)]
    
    [(cell-app builtin args)
     (let ((fmt-args (map cell-expr->openformula args)))
       (format-app builtin fmt-args))]
    ))
