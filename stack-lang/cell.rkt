#lang racket

(require "main.rkt"
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
    (list* 'cell-expr op
           (map (lambda (dim name)
                  (list 'cell-ref
                        (make-name name (if (null? dim) null idx))))
                arg-dims arg-names))))

(module+ test
  (check-equal? (unwrap-binary-op-expr '([+ () (3)] %e1 %e2) '(2))
                '(cell-expr + (cell-ref "%e1") (cell-ref "%e2[2]"))))

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
             (list* 'row
                    (list 'cell
                          (foldl
                           (lambda (x acc)
                             (list 'cell-expr '+ acc (list 'cell-ref x)))
                           (list 'cell-ref (car args-unwrapped))
                           (cdr args-unwrapped)))))]

          [else ;; values, refs or vectorized builtin eval
           (list* 'row
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
                               (list 'cell-ref (make-name expr idx))]
                              [else
                               (unwrap-binary-op-expr expr idx)])))
                      (list 'cell cell-expr
                            '#:id (make-name (assignment-id a) idx)))))]
          )))

(module+ test
  (let ((expected '(row (cell 1 #:id "%e1")))
        (actual   (assignment->row
                   (assignment '%e1 '() '() 1 1))))
    (check-equal? actual expected))

  (let ((expected '(row (cell 3 #:id "%e1[0]")
                        (cell 1 #:id "%e1[1]")
                        (cell 4 #:id "%e1[2]")))
        (actual   (assignment->row
                   (assignment '%e1 '() '() #(3 1 4) #(3 1 4)))))
    (check-equal? actual expected))

  (let ((expected '(row (cell (cell-expr + (cell-ref "%e1[0]") (cell-ref "%e2"))
                                         #:id "%sum1[0]")
                        (cell (cell-expr + (cell-ref "%e1[1]") (cell-ref "%e2"))
                                         #:id "%sum1[1]")
                        (cell (cell-expr + (cell-ref "%e1[2]") (cell-ref "%e2"))
                                         #:id "%sum1[2]")))
        (actual (assignment->row
                 (assignment '%sum1 '() '() '([+ (3) ()] %e1 %e2) #(0 1 2)))))
    (check-equal? actual expected))

  (let ((expected '(row (cell (cell-ref "target") #:id "a")))
        (actual   (assignment->row
                   (assignment 'a '() '() 'target 0))))
    (check-equal? actual expected))

  ;; "sum" results in a fold
  (let ((expected '(row
                    cell
                    (cell-expr
                     +
                     (cell-expr + (cell-ref "a[0]") (cell-ref "a[1]"))
                     (cell-ref "a[2]"))))
        
        (actual   (assignment->row
                   (assignment 'result '() '() '([sum (3)] a) 0))))
    (check-equal? actual expected)))

;; ;; assignment-list->sheet :: (List assignment?) -> sheet?
;; (define (assignments->sheet a)
;;   (let loop ((a a)
;;              (rows (list)))
;;     (if (null? a)
;;         rows
;;         (loop (cdr a)
;;               (cons (assignment->row (car a)) rows)))
;;     ))

