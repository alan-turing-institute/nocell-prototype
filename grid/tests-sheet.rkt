#lang racket/base

(require rackunit
         math/array
         "sheet.rkt"
         )

;; ---------------------------------------------------------------------------------------------------

(test-case "Predicates"
  (check-pred nothing? 'nothing "'nothing should be of type nothing?")

  (check-pred atomic-value? 1        "Exact numbers should be atomic values.")
  (check-pred atomic-value? 1.0      "Real numbers should be atomic values.")
  (check-pred atomic-value? "text"   "Strings should be atomic values.")
  (check-pred atomic-value? #t       "Booleans should be atomic values.")
  (check-pred atomic-value? 'nothing "'nothing should be an atomic value.")

  (check-pred simple-cell-value? (cell-value-return 3))
  (check-pred (compose not simple-cell-value?)
              (cell-value (array #[#[0 1] #[2 3]]))))

;; In the future, we may wish to remove these
(test-case "cell-expressions"
  (check-pred cell-expr? (cell-name "text"))
  (check-pred cell-expr? (cell-ref))
  (check-pred cell-ref? (cell-addr 0 0 #t #t))
  (check-pred cell-ref? (cell-range (cell-addr 0 0 #t #t) (cell-addr 1 1 #f #f)))
  (check-pred cell-expr? (cell-app 'fn (list (cell-expr))))
  (check-pred cell-expr? (cell-value (array #[#[0]]))))

(check-equal? ((compose atomise cell-value-return) 42) 42
              "(compose atomise cell-value-return) not identity.")

;;; sheet.rkt no longer exports lists->sheet and sheet->lists
;;;
;; Note that this checks equality of all the elements of the list and we may run
;; into trouble with inexact numbers
;; (let ([ls '((0 1) (2 3))])
;;   (check-equal? ((compose sheet->lists lists->sheet) ls) ls
;;                 "(compose sheet->lists lists->sheet) not identity."))

(define sht
  (let ([contents (array #[#[(cell (cell-value-return 0)) (cell (cell-value-return 1))]     ; row 0
                           #[(cell (cell-value-return 2)) (cell (cell-value-return 3))]])]  ; row 1
        [refs     `(("id" . ,(cell-addr 1 0 #t #f)))])
    (sheet contents refs null "a-sheet")))

(test-case "Referencing in sheets"
  (check-equal?
   (sheet-resolve-name sht "id")
   (cell-addr 1 0 #t #f))
  (check-equal?
   (addr-index (sheet-resolve-name sht "id"))
   #(1 0))
  (check-equal?
   (sheet-ref sht (addr-index (sheet-resolve-name sht "id")))
   (cell-value-return 2))
  (check-false (sheet-resolve-name sht "not-there")))

(check-equal?
 (range-extent (cell-range (cell-addr 5 6 #t #t) (cell-addr 10 12 #t #t)))
 #(6 7))

(check-equal?
 (addr-offset (cell-addr 5 6 #t #f) #(1 2))
 #(6 8))

