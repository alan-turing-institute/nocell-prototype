#lang racket/base

(require math/array)

(module+ test
         (require rackunit
                  "sheet.rkt"
                  "sheet-eval.rkt")
         
         (test-case "Tests of sheet-eval"
           (let ((example-sheet
                  (list->sheet
                   `((1 ; column "A"
                      ,(cell-app '+
                                   (list (cell-ref 0 -1 #t #t)
                                         (cell-value-return 100)))
                      3)
                     (4 ; column "B"
                      5
                      6)))))
             (check-equal?
              (sheet-eval example-sheet)
              (mutable-array #[#[1.0 101.0 3.0] #[4.0 5.0 6.0]])))

           (let ((example-sheet
                  (list->sheet
                   `((1
                      2
                      3
                      ,(cell-app '+
                                 (list (cell-range (cell-ref 0 -2 #f #t)
                                                   (cell-ref 1 3 #f #f))
                                       (cell-value-return -5)))
                      ,(cell-range (cell-ref 0 0 #f #f)
                                   (cell-ref 1 2 #f #f)))))))
             
             (check-equal?
              (sheet-eval example-sheet)
              (mutable-array #[#[1.0 2.0 3.0 0.0 'VALUE]])))

           (let ((example-sheet
                  (list->sheet
                   `((1 2 3 empty 4
                        ,(cell-app '+
                                   (list (cell-range (cell-ref 0 0 #f #f)
                                                     (cell-ref 1 5 #f #f)))))))))
             (check-equal?
              (sheet-eval example-sheet)
              (mutable-array #[#[1.0 2.0 3.0 'empty 4.0 10.0]])))

           (let ((eps 1e-12)
                 (example-sheet
                  (list->sheet
                   `((1
                      2
                      3
                      2
                      1
                      ,(cell-app
                        'sqrt
                        (list (cell-app
                               '+
                               (list (cell-range
                                      (cell-ref 0 0 #f #f)
                                      (cell-ref 1 5 #f #f)))))))))))
             (check-=
              (array-ref (sheet-eval example-sheet) #(0 5))
              3.0
              eps
              ))

           (let ((example-sheet
                  (list->sheet
                   `((,(cell-app 'random '()))))))
             (check-=
              (array-ref (sheet-eval example-sheet) #(0 0))
              0.5 0.5))


           (let ((eps 1e-12)
                 (example-sheet
                  (list->sheet
                   `((,(cell-app 'log (list (cell-value-return 1000))))))))
             (check-=
              (array-ref (sheet-eval example-sheet) #(0 0))
              3.0
              eps))

           ) ;; test-case

         ;;
         )
