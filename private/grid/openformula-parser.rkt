#lang racket/base

#|

Parse (a small subset of) OpenFormula format formulae as cell-values.
OpenFormula is the formula language used within OpenDocument spreadsheets [1].

[1] "Open Document Format for Office Applications (OpenDocument)
    Version 1.2, part 2: Recalculated Formula (OpenFormula)
    Format", OASIS, September 2011

TODO
- Only a limited set of functions are supported
- Doesn't work with ranges

|#

(require parsack
         math/array
         (only-in racket/list flatten)
         rackunit
         "sheet.rkt"
         "column-letters.rkt")

(provide openformula->cell-expr)

(define (transpose arr) (array-axis-swap arr 0 1))

(define (maybe s)
  (<or> s (return null)))

(define (withSpaces s)
  (parser-one
   $spaces
   (~> s)
   $spaces))

(define (char->symbol c) (string->symbol (list->string (list c))))

;; parse infix operator parser by their precedence
(define ($infix-op prec)
  (case prec
    ((0) (oneOf "+-"))
    ((1) (oneOf "/*"))
    ((2) (oneOf "^"))
    (else #f)))

(define (($openformula-expr/prec prec) s)
  (let (($op ($infix-op prec)))
    ((parser-compose
      (left <- (withSpaces (if (not $op)
                               $l-expr
                               ($openformula-expr/prec (+ prec 1)))))
      (<or> (try (parser-compose
                  (op <- (or $op $err)) ; $op is #f after last
                                        ; precedence level - call
                                        ; error parser to skip to the
                                        ; other <or> case
                  (op-sym <- (return (char->symbol op)))
                  (right <- (withSpaces ($openformula-expr/prec prec)))
                  (return (cell-app op-sym (list left right)))))
            (return left)))
     s)))

(define $openformula-expr ($openformula-expr/prec 0))

(define $parenthesized-expr
  (between (string "(") (string ")") $openformula-expr))

(define $function-name
  (parser-compose
   (fn <- $identifier)
   (fn-str <- (return (string-downcase (list->string fn))))
   (return
    (case fn-str
      (("sum")     '+)
      (("power")   'expr)
      (("product") '*)
      (("mod")     'modulo)
      (("trunc")   'truncate)
      (else        (string->symbol fn-str))))))

(define $prefix-app
  (parser-compose
   (fn <- $function-name)
   $spaces
   (args <- (between (char #\() (char #\))
                     (sepBy (parser-one $spaces
                                        (~> $openformula-expr)
                                        $spaces)
                            (char #\;))))
   $spaces
   (return (cell-app fn args))
   ))

(define $cell-name
  (parser-compose
   (id <- $identifier)
   (return (cell-name (list->string id)))))

(define $cell-string
  (between (string "&quot;") (string "&quot;")
           (many1 (<!> (string "&quot;")))))

(define $decimal
  (>>= (parser-seq
        (maybe (oneOf "+-"))
        (<or>
         (parser-seq (char #\.) (many1 $digit))
         (try (parser-seq (many1 $digit) (char #\.)))
         (many1 $digit))
        (many $digit))
       (lambda (t) (return (flatten t)))))

(define $cell-number
  (>>= (parser-seq
        $decimal
        (maybe (parser-seq (oneOf "eE")
                           (maybe (oneOf "+-"))
                           (many1 $digit))))
       (lambda (t) (return (string->number
                            (list->string (flatten t)))))))

(define $cell-bool
  (<or> (>> (string "TRUE") (return #t))
        (>> (string "FALSE") (return #f))))

(define $cell-atomic
  (<or> $cell-number
        $cell-bool))

(define $cell-simple-value
  (>>= $cell-atomic
       (lambda (val)
         (return (cell-value-return val)))))

(module+ test
  (check-equal? (parse-result $cell-atomic "1")   1)
  (check-equal? (parse-result $cell-atomic "1.")  1.0)
  (check-equal? (parse-result $cell-atomic ".0")  0.0)
  (check-equal? (parse-result $cell-atomic "2.4") 2.4)
  (check-equal? (parse-result $cell-atomic "1.1E+021") 1.1e21)

  (check-equal? (parse-result $cell-simple-value "0.0")
                (cell-value (array #[#[0.0]]))))


(define $cell-row
  (sepBy (withSpaces (<or> $cell-atomic (return 'nothing))) (char #\;)))

(define $cell-array
  (>>= (between (char #\{) (char #\})
                (sepBy (withSpaces $cell-row) (char #\|)))
       (lambda (elts)
         (return (cell-value (transpose (list*->array elts atomic-value?)))))))
(module+ test
  (check-equal?
   (parse-result $cell-array "{1;4|2;5|3;6}")
   (cell-value (array #[#[1 2 3] #[4 5 6]]))))

(define $ref-addr
  (parser-compose
   (char #\.)
   (col-rel-str <- (maybe (char #\$)))
   (col-rel?    <- (return (not (null? col-rel-str))))

   (col-letter  <- (many1 $letter))
   (col         <- (return (column-letter->integer
                            (list->string col-letter))))
   
   (row-rel-str <- (maybe (char #\$)))
   (row-rel?    <- (return (not (null? row-rel-str))))

   (row-str     <- (many1 $digit))
   (row         <- (return (sub1 (string->number
                                  (list->string row-str)))))
   (return (cell-addr col row col-rel? row-rel?))))

(define $cell-ref
  (between (char #\[) (char #\]) $ref-addr))
(module+ test
  (check-equal?
   (parse-result $cell-ref "[.D5]")
   (cell-addr 3 4 #f #f)))

(define $cell-range
  (between (char #\[) (char #\])
           (parser-compose
            (tl <- $ref-addr)
            (char #\:)
            (br <- $ref-addr)
            (return (cell-range tl br)))))
(module+ test
  (check-equal?
   (parse-result $cell-range "[.A$2:.$C4]")
   (cell-range (cell-addr 0 1 #f #t) (cell-addr 2 3 #t #f))))

(define $l-expr
  (<or> (try $parenthesized-expr)
        (try $prefix-app)
        (try $cell-simple-value)
        (try $cell-array)
        (try $cell-ref)
        (try $cell-range)
        $cell-name))

(define $openformula
  (parser-one
   ;; ignore any starting "=" or "of:="
   (maybe (string "of:"))
   (maybe (string "="))
   ;; allow for "recalculated" formulae (with second "=")
   (maybe (string "="))
   (~> $openformula-expr)
   $eof))

(define (openformula->cell-expr x)
  (parse-result $openformula x))


;; (module+ test
;;   (require "../eval.rkt")
;;   (test-case "openformula->cell-expr tests"
;;     (check-equal?
;;      (openformula->cell-expr "SUM({ 1;2  ; 3;4};[.A1])+ 5 *MOD( 7;   6)")
;;      (cell-app
;;       '+
;;       (list
;;        (cell-app
;;         '+
;;         (list
;;          (cell-value (array #[#[1] #[2] #[3] #[4]]))
;;          (cell-addr 0 0 #f #f)))
;;        (cell-app
;;         '*
;;         (list
;;          (cell-value (array #[#[5]]))
;;          (cell-app
;;           'modulo
;;           (list (cell-value (array #[#[7]]))
;;                 (cell-value (array #[#[6]])))))))))

;;     (check-equal?
;;      (sheet-eval (sheet (array #[#[(openformula->cell-expr
;;                                     ; note the empty value in the array
;;                                     "SUM({0;1|2; |4;5})")]])
;;                         null))
;;      (mutable-array #[#[12]]))))
