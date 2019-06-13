#lang gamble

(require (except-in racket #%module-begin #%top-interaction)
         (only-in "../cell/assignment.rkt" stack-print)
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
