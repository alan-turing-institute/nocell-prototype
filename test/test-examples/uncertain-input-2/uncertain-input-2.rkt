; -*- racket -*-
#lang s-exp "../../../stack-lang/nocell.rkt"

(provide result)

(require "../../test-lib/timesheet-data.nocell")

(define mean  (~uniform 3.0 9.0))
(define stdev (~uniform 0.2 2.0))

(define result (~normal mean stdev timesheet-3))
