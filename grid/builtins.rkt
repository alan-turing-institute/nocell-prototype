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

;; Internal list of builtings
;; Each entry is a pair, (symbol? . arity?),
;; where arity? is either
;; - a number?, indicating a fixed arity
;; - a pair, indicating a multiple arity function

(define *builtins*
  (list
   ;; -- Lookup and reference --

   ;; (ref range idx) for one-dimensional ranges
   ;; (ref range col-idx row-idx) for two-dimenisional ranges
   '[ref . (2 . 3)]
   
   ;; -- Maths --

   ;; Arithmetic
   '[+ . (1 .  2)]  ; + and - can also be unary prefix operators
   '[- . (1 .  2)]
   '[* . 2]
   '[/ . 2]
   '[abs      . 1]
   '[round    . 1]
   '[truncate . 1]
   '[floor    . 1]
   '[ceiling  . 1]

   ;; Multiple arity
   '[sum . (1 . #f)]
   '[min . (1 . #f)] 
   '[max . (1 . #f)]
   
   ;; Functions of ranges
   '[range-sum . 1]
   '[range-min . 1] 
   '[range-max . 1]

   ;; Integer arithmetic
   '[quotient  . 2]
   '[remainder . 2]
   '[modulo    . 2]

   ;; Powers and roots
   '[expt . 2]
   '[log  . (1 . 2)] ; log of one argument is natural log

   ;; Trigonometric functions
   '[sin  . 1]
   '[cos  . 1]
   '[tan  . 1]
   '[asin . 1]
   '[acos . 1]
   '[atan . 1]
   
   ;; Maths -- Other
   ;; (rand) is a uniform random variate in the interval [0, 1)
   ;; (rand k) is a random integer 0 .. (k - 1)
   ;; (rand min max) is a random integer min ... (max - 1)
   '[random . (0 . 2)] 
   
   ))


