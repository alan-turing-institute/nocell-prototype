#lang gamble

(require (except-in racket #%module-begin #%top-interaction)
         (only-in "../cell/assignment.rkt"
                  stack-print assignment? stack-top-val)
         "main.rkt")

(provide @
         ±
         +/-
         ~normal
         ~uniform
         stack-print
         sum
         product
         len
         nth
         halt
         (rename-out (=& =))
         (rename-out (<&  <))
         (rename-out (<=& <=))
         (rename-out (>&  >))
         (rename-out (>=& >=))
         (rename-out (+& +))
         (rename-out (-& -))
         (rename-out (*& *))
         (rename-out (/& /))
         (rename-out (expt& expt))
         (rename-out (define& define))
         (rename-out (define-values& define-values))
         (rename-out (if& if))

         (rename-out (datum #%datum))
         #%app
         #%module-begin
         #%top
         #%top-interaction

         require
         provide)

(let ([printer (current-print)])
  (current-print
   (lambda (a) (printer
                (if (and (list? a) (andmap assignment? a))
                    (stack-top-val a)
                    a)))))
