#lang racket/base

(require math/array)

(module+ test
         (require rackunit "grid.rkt")

         (test-case "This is where tests will go"
                    (check-equal? (+ 1 1) 2 "1+1=2")
                    (check-equal? (+ 1 2) 2 "1+2=2")
                    )

         (require rackunit "sheet-eval.rkt")
         
         (test-case "Tests of sheet-eval"
           (let ((example-sheet
                  (list->sheet
                   `((1 ; column "A"
                      ,(cell-app 'builtin-foo
                                   (list (cell-ref 0 -1 #t #t)
                                         (cell-value-return 100)))
                      3)
                     (4 ; column "B"
                      5
                      6)))))
             (check-equal?
              (sheet-eval example-sheet)
              (mutable-array #[#[1 101 3] #[4 5 6]])))

           (let ((example-sheet
                  (list->sheet
                   `((1
                      2
                      3
                      ,(cell-app 'builtin-foo
                                 (list (cell-range (cell-ref 0 -2 #f #t)
                                                   (cell-ref 1 3 #f #f))
                                       (cell-value-return -5)))
                      ,(cell-range (cell-ref 0 0 #f #f)
                                   (cell-ref 1 2 #f #f)))))))
             
             (check-equal?
              (sheet-eval example-sheet)
              (mutable-array #[#[1 2 3 0 'VALUE]]))))

         ;;
         
         )



