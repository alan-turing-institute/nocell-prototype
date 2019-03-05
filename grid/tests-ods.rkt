#lang racket/base

(require rackunit
         "ods.rkt"
         "grid.rkt"
         (prefix-in s: "sheet.rkt")
         "eval.rkt")

(define asheet
  (sheet
   (row 0 1)
   (row 2 3)
   (row 3 4)))
