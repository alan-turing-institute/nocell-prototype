#lang racket

(provide is-builtin?
         builtins
         builtin-min-args
         builtin-max-args)

(define (is-builtin? id)
  (assq id builtins))

;; -> sequence-of symbol?
(define (builtins)
  (in-dict-keys builtins0))

(define (builtin-min-args id)
  (let ([nargs (cdr (assq id builtins))])
    (if (cons? nargs)
        (car nargs)
        nargs)))

;; Returns null if max args is unbounded
(define (builtin-max-args id)
  (let ([nargs (cdr (assq id builtins))])
    (if (cons? nargs)
        (cdr nargs)
        nargs)))

(define (unary-builtin id)
  (cons id 1))

(define (binary-builtin id)
  (cons id 2))

(define number-builtins
  (append
   '([random . 0]
     [+ . (1)]
     [- . (1)]
     [min . (1)]
     [max . (2)]
     [log . (1 . 2)])
   (map binary-builtin
        '(* /
          quotient remainder modulo
          expt))
   (map unary-builtin
        '(abs floor ceiling truncate 
          sin cos tan
          asin acos atan
          sqrt exp))))

(define builtins0
  (append
   number-builtins
   ;; more go here ...
   ))
