#lang racket/base

(require rackunit
         math/array
         "grid.rkt"
         "eval.rkt"
         (prefix-in sheet: "sheet.rkt"))


;; ---------------------------------------------------------------------------------------------------

(check-equal?
 (sheet (row (cell)) #:name "geranium")
 (sheet:sheet (array #[ #[ (sheet:unstyled-cell (sheet:cell-value-return 'nothing))]])
              null ;; refs
              null ;; style-definitions
              null ;; column-definitions
              null ;; row-definitions
              '(()) ;; meta
              "geranium")) ;; name
