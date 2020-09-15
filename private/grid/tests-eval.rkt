#lang racket/base

(require rackunit
         math/array
         "grid.rkt"
         "eval.rkt"
         )

;; ---------------------------------------------------------------------------------------------------

(check-equal?
 (sheet-eval (sheet (row (cell))))
 (array #[#['nothing]]))

(check-equal?
 (sheet-eval
  (sheet
   (row 0 1)
   (row 2 3)))
 (array
  #[#[0 1]
    #[2 3]])
 )

(check-equal?
 (sheet-eval
  (sheet
   (row (cell '(+ 1 2)))))
 (array #[#[3]]))

(check-equal?
 (sheet-eval
  (sheet
   (row (cell 21 "a-cell") (cell (ref "a-cell")))))
 (array #[#[21 21]]))

(check-equal?
 (sheet-eval
  (sheet
   (row (cell 21 "a-cell") (cell `(* 2 ,(ref "a-cell"))))))
 (array #[#[21 42]]))

;; --------------------------------------------------------------------------------

;; Old test -- should be tried again!


;; (test-case "Tests of sheet-eval"
;;   (let ((example-sheet
;;          (list->sheet
;;           `((1 ; column "A"
;;              ,(cell-app '+
;;                         (list (cell-ref 0 -1 #t #t)
;;                               (cell-value-return 100)))
;;              3)
;;             (4 ; column "B"
;;              5
;;              6)))))
;;     (check-equal?
;;      (sheet-eval example-sheet)
;;      (mutable-array #[#[1.0 101.0 3.0] #[4.0 5.0 6.0]])))

;;   (let ((example-sheet
;;          (list->sheet
;;           `((1
;;              2
;;              3
;;              ,(cell-app '+
;;                         (list (cell-range (cell-ref 0 -2 #f #t)
;;                                           (cell-ref 0 2 #f #f))
;;                               (cell-value-return -5)))
;;              ,(cell-range (cell-ref 0 0 #f #f)
;;                           (cell-ref 0 1 #f #f)))))))
    
;;     (check-equal?
;;      (sheet-eval example-sheet)
;;      (mutable-array #[#[1.0 2.0 3.0 0.0 'VALUE]])))

;;   (let ((example-sheet
;;          (list->sheet
;;           `((1 2 3 empty 4
;;                ,(cell-app '+
;;                           (list (cell-range (cell-ref 0 0 #f #f)
;;                                             (cell-ref 0 4 #f #f)))))))))
;;     (check-equal?
;;      (sheet-eval example-sheet)
;;      (mutable-array #[#[1.0 2.0 3.0 'empty 4.0 10.0]])))

;;   (let ((eps 1e-12)
;;         (example-sheet
;;          (list->sheet
;;           `((1
;;              2
;;              3
;;              2
;;              1
;;              ,(cell-app
;;                'sqrt
;;                (list (cell-app
;;                       '+
;;                       (list (cell-range
;;                              (cell-ref 0 0 #f #f)
;;                              (cell-ref 0 4 #f #f)))))))))))
;;     (check-=
;;      (array-ref (sheet-eval example-sheet) #(0 5))
;;      3.0
;;      eps
;;      ))

;;   (let ((example-sheet
;;          (list->sheet
;;           `((,(cell-app 'random '()))))))
;;     (check-=
;;      (array-ref (sheet-eval example-sheet) #(0 0))
;;      0.5 0.5))


;;   (let ((eps 1e-12)
;;         (example-sheet
;;          (list->sheet
;;           `((,(cell-app 'log (list (cell-value-return 1000))))))))
;;     (check-=
;;      (array-ref (sheet-eval example-sheet) #(0 0))
;;      3.0
;;      eps))

;;   ) 

;; (test-case "OpenFormula"
;;   (let ((expr
;;          (cell-app
;;           'log
;;           (list (cell-app
;;                  '+
;;                  (list (cell-value-return 2)
;;                        (cell-app
;;                         '+
;;                         (list (cell-value-return 0.5)
;;                               (cell-ref 0 0 #t #f)
;;                               (cell-ref 0 1 #t #t)
;;                               (cell-range (cell-ref 2 2 #f #f)
;;                                           (cell-ref 4 4 #f #f)))))))))
;;         (ofstring "LOG((2.0+SUM(0.5;[.$A1];[.$A$2];[.C3:.E5])))"))
;;     (check-equal?
;;      (cell-expr->openformula expr)
;;      ofstring)

;;     (check-equal?
;;      (openformula->cell-expr ofstring)
;;      expr))) 
