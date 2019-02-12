#lang racket

(require "main.rkt"
         "../grid/grid.rkt"
         math/array)

(module+ test (require rackunit))

(define (make-name base offset)
  (let ((base-str (symbol->string base)))
    (if (null? offset)
        base-str
        (string-append base-str "[" (~a (car offset)) "]"))))

(module+ test
  (check-equal? (make-name 'a '()) "a")
  (check-equal? (make-name 'b '(2)) "b[2]"))

(define (unwrap-binary-op-expr expr idx)
  (let ((op         (caar expr))
        (arg-dims   (cdar expr))
        (arg-names  (cdr  expr)))
    (list* op
           (map (lambda (dim name)
                  (ref (make-name name (if (null? dim) null idx))))
                arg-dims arg-names))))

(module+ test
  (check-equal? (unwrap-binary-op-expr '([+ () (3)] %e1 %e2) '(2))
                (list '+ (ref "%e1") (ref "%e2[2]"))))

(define (expr-op expr)
  (match expr
    [(cons (cons op _) _) op]
    [_ #f]))

(define (unwrap arg dim)
  (if (null? dim)
      (list (make-name arg dim))
      (map (lambda (i) (make-name arg (list i))) (range (car dim)))))

(define (assignment->row a)
  (let* ((vals (assignment-val a))
         (expr (assignment-expr a))
         (vector-vals (if (vector? vals)
                          vals
                          (vector vals))))
    (cond [(eq? (expr-op expr) 'nth) 'nth]
          [(eq? (expr-op expr) 'len) 'len]

          [(eq? (expr-op expr) 'sum) ;; folds (just sum for now)
           (let* ((args           (cdr expr))
                  (arg-dims       (cdar expr))
                  (args-unwrapped (car (map unwrap args arg-dims))))
             (row
              (cell
               (foldl
                (lambda (x acc)
                  (list '+ acc (ref x)))
                (ref (car args-unwrapped))
                (cdr args-unwrapped)))))]

          [else ;; values, refs or vectorized builtin eval
           (apply row
                  (for/list ([v (in-vector vector-vals)]
                             [col (in-naturals)])
                    (let* ((idx (if (null? (shape vals))
                                    null
                                    (list col)))
                           (cell-expr
                            (cond
                              [(equal? expr vals) ;; expr is a value
                               (vector-ref vector-vals col)]
                              [(symbol? expr)
                               (ref (make-name expr idx))]
                              [else
                               (unwrap-binary-op-expr expr idx)])))
                      (cell cell-expr
                            #:id (make-name (assignment-id a) idx)))))]
          )))

(module+ test
  (let ((expected (row (cell 1 #:id "%e1")))
        (actual   (assignment->row
                   (assignment '%e1 '() '() 1 1))))
    (check-equal? actual expected))

  (let ((expected (row (cell 3 #:id "%e1[0]")
                       (cell 1 #:id "%e1[1]")
                       (cell 4 #:id "%e1[2]")))
        (actual   (assignment->row
                   (assignment '%e1 '() '() #(3 1 4) #(3 1 4)))))
    (check-equal? actual expected))

  (let ((expected (row (cell (list '+ (ref "%e1[0]") (ref "%e2"))
                             #:id "%sum1[0]")
                       (cell (list '+ (ref "%e1[1]") (ref "%e2"))
                             #:id "%sum1[1]")
                       (cell (list '+ (ref "%e1[2]") (ref "%e2"))
                             #:id "%sum1[2]")))
        (actual (assignment->row
                 (assignment '%sum1 '() '() '([+ (3) ()] %e1 %e2) #(0 1 2)))))
    (check-equal? actual expected))

  (let ((expected (row (cell (ref "target") #:id "a")))
        (actual   (assignment->row
                   (assignment 'a '() '() 'target 0))))
    (check-equal? actual expected))

  ;; "sum" results in a fold
  (let ((expected (row (cell (list '+ (list '+ (ref "a[0]") (ref "a[1]"))
                                   (ref "a[2]")))))
        (actual   (assignment->row
                   (assignment 'result '() '() '([sum (3)] a) 0))))
    (check-equal? actual expected)))
  

;; assignments->sheet :: (List assignment?) -> sheet?
(define (assignments->sheet a)
  (apply sheet (map assignment->row (reverse a))))

(module+ test
  (let ((expected (sheet (row (cell  0 #:id "a[0]")
                              (cell -1 #:id "a[1]")
                              (cell -2 #:id "a[2]"))
                         (row (cell  0 #:id "b[0]")
                              (cell  2 #:id "b[1]")
                              (cell  4 #:id "b[2]"))
                         (row (cell (list '+ (ref "a[0]") (ref "b[0]")))
                              (cell (list '+ (ref "a[1]") (ref "b[1]")))
                              (cell (list '+ (ref "a[2]") (ref "b[2]"))))))
        (actual   (assignments->sheet
                   (list
                    (assignment 'result '() '() '([+ (3) (3)] a b) #(0 1 2))
                    (assignment 'b      '() '() #(0  2  4) #(0  2  4))
                    (assignment 'a      '() '() #(0 -1 -2) #(0 -1 -2))))))
    (check-equal? actual expected)))

