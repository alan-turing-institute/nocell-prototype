#lang racket/base

(require rackunit
         math/array
         "grid.rkt"
         "eval.rkt"
         (prefix-in sheet: "sheet.rkt"))


;; ---------------------------------------------------------------------------------------------------

(check-equal?
 (sheet (row (blank)) #:name "geranium")
 (sheet:sheet (array #[ #[ (sheet:cell (sheet:cell-value-return 'nothing))]]) null "geranium"))
