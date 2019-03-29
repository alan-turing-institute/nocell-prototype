#lang racket/base

#|
List of nocell built-in functions, together with details of their arity. 
Actual implementations must be provided by each backend

|#

(require racket/dict)

(provide is-builtin?        
         builtins           
         builtin-min-args
         builtin-max-args)

;; is-builtin? : any/c -> boolean?
(define (is-builtin? id)
  (assq id *builtins*))

;; builtins : -> [sequence-of symbol?]
(define (builtins)
  (in-dict-keys *builtins*))

;; builtin-min-args : symbol? -> number?
;; Returns the minimum arity of the builtin, possibly zero
(define (builtin-min-args id)
  (let ([nargs (cdr (assq id *builtins*))])
    (if (pair? nargs)
        (car nargs)
        nargs)))

;; builtin-max-args : symbol? -> number?
;; Returns the maximum arity, or #f if unbounded
(define (builtin-max-args id)
  (let ([nargs (cdr (assq id *builtins*))])
    (if (pair? nargs)
        (cdr nargs)
        nargs)))

(define (unary-builtin id)
  (cons id 1))

(define (binary-builtin id)
  (cons id 2))

;; Internal list of builtings
;; Each entry is a pair, (symbol? . arity?),
;; where arity? is either
;; - a number?, indicating a fixed arity
;; - a pair, indicating a multiple arity function

(define *builtins*
  (list
   ;; Arithmetic
   '[+ . (1 .  2)]  ; + and - can also be unary prefix operators
   '[- . (1 .  2)]
   '[* . 2]
   '[/ . 2]

   ;; Functions of ranges
   '[sum . (1 . #f)]
   '[min . (1 . #f)] 
   '[max . (1 . #f)]

   ;; Integer arithmetic
   '[quotient  . 2]
   '[remainder . 2]
   '[modulo    . 2]

   ;; Maths -- Powers and roots
   '[expt      . 2]
   
   '[log    . (1 . 2)]
   '[random . 0]
   ))

(define number-builtins
  (append
  
   (map binary-builtin
        '(* /
          quotient remainder modulo
          expt))
   (map unary-builtin
        '(abs floor ceiling truncate 
          sin cos tan
          asin acos atan
          sqrt exp))))
