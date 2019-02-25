#lang racket

(require "main.rkt"
         "../grid/grid.rkt"
         "../grid/eval.rkt"
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

(define (pad-to-width w lst)
  (take (append lst (make-list w (cell))) w))

(define (->vector x)
  (if (vector? x) x (vector x)))

(define (assignment->row pad-width a)
  (let* ((val  (assignment-val a))
         (expr (assignment-expr a))
         (vector-vals (->vector val)))
    (cond [(eq? (expr-op expr) 'nth)
           (apply row (pad-to-width pad-width
                 (list (cell (ref (make-name (caddr expr) (cadr expr))
                                  (assignment-id a))))))]
                                     
          [(eq? (expr-op expr) 'len)
           (apply row (pad-to-width pad-width
                 (list (cell val (make-name (assignment-id a) null)))))]

          [(eq? (expr-op expr) 'sum) ;; folds (just sum for now)
           (let* ((args           (cdr expr))
                  (arg-dims       (cdar expr))
                  (args-unwrapped (car (map unwrap args arg-dims))))
             (apply row (pad-to-width pad-width
                   (list (cell (foldl (lambda (x acc)
                                        (list '+ acc (ref x)))
                                      (ref (car args-unwrapped))
                                      (cdr args-unwrapped))
                               (make-name (assignment-id a) null))))))]

          [else ;; values, refs or (vectorized) builtins
           (apply row
                  (pad-to-width pad-width
                   (for/list ([v (in-vector vector-vals)]
                              [col (in-naturals)])
                     (let* ((idx (if (null? (shape val))
                                     null
                                     (list col)))
                            (cell-expr
                             (cond
                               [(equal? expr val) ;; expr is a value
                                (vector-ref vector-vals col)]
                               [(symbol? expr)
                                (ref (make-name expr idx))]
                               [else
                                (unwrap-binary-op-expr expr idx)])))
                       (cell cell-expr
                             (make-name (assignment-id a) idx))))))]
           )))

(module+ test
  (let ((expected (row (cell 1 "%e1") (cell) (cell)))
        (actual   (assignment->row 3
                   (assignment '%e1 '() '() 1 1))))
    (check-equal? actual expected))

  (let ((expected (row (cell 3 "%e1[0]")
                       (cell 1 "%e1[1]")
                       (cell 4 "%e1[2]")))
        (actual   (assignment->row 3
                   (assignment '%e1 '() '() #(3 1 4) #(3 1 4)))))
    (check-equal? actual expected))

  (let ((expected (row (cell (list '+ (ref "%e1[0]") (ref "%e2"))
                             "%sum1[0]")
                       (cell (list '+ (ref "%e1[1]") (ref "%e2"))
                             "%sum1[1]")
                       (cell (list '+ (ref "%e1[2]") (ref "%e2"))
                             "%sum1[2]")))
        (actual (assignment->row 3
                 (assignment '%sum1 '() '() '([+ (3) ()] %e1 %e2) #(0 1 2)))))
    (check-equal? actual expected))

  (let ((expected (row (cell (ref "target") "a")))
        (actual   (assignment->row 1
                   (assignment 'a '() '() 'target 0))))
    (check-equal? actual expected))

  ;; "sum" results in a fold
  (let ((expected (row (cell (list '+ (list '+ (ref "a[0]") (ref "a[1]"))
                                   (ref "a[2]")) "result")))
        (actual   (assignment->row 1
                   (assignment 'result '() '() '([sum (3)] a) 0))))
    (check-equal? actual expected)))

;; assignments->sheet :: (List assignment?) -> sheet?
 (define (stack->sheet stack)
  (define widths (map (lambda (a) (vector-length (->vector (val a)))) stack))
  (define max-width (apply max widths))
  (apply sheet (map (curry assignment->row max-width) (reverse stack))))

(module+ test
  (let ((expected (sheet (row (cell  0 "a[0]")
                              (cell -1 "a[1]")
                              (cell -2 "a[2]"))
                         (row (cell  0 "b[0]")
                              (cell  2 "b[1]")
                              (cell  4 "b[2]"))
                         (row (cell (list '+ (ref "a[0]") (ref "b[0]")))
                              (cell (list '+ (ref "a[1]") (ref "b[1]")))
                              (cell (list '+ (ref "a[2]") (ref "b[2]"))))))
        (actual   (stack->sheet
                   (list
                    (assignment 'result '() '() '([+ (3) (3)] a b) #(0 1 2))
                    (assignment 'b      '() '() #(0  2  4) #(0  2  4))
                    (assignment 'a      '() '() #(0 -1 -2) #(0 -1 -2))))))
    (check-equal? actual expected)))

(module+ test
  (require (submod "example-2.rkt" example))
  (let ((epsilon  1e-7)
        (actual   (sheet-eval (stack->sheet result)))
        (expected (mutable-array #[#[2010 2011 2012 2013]
                                   #[-100 -50 150 500]
                                   #[0.03 'nothing 'nothing 'nothing] 
                                   #[0.1 'nothing 'nothing 'nothing] 
                                   #[1 'nothing 'nothing 'nothing] 
                                   #[1.1 'nothing 'nothing 'nothing] 
                                   #[1 0 0 0] 
                                   #[2010 0 0 0] 
                                   #[2010 'nothing 'nothing 'nothing] 
                                   #[0 1 2 3] 
                                   #[1 1.1 1.2100000000000002 1.3310000000000004] 
                                   #[-100 -45.45454545454545 123.96694214876031 375.65740045078877] 
                                   #[354.16979714500366 'nothing 'nothing 'nothing] 
                                   #[0 0 0 1] 
                                   #[0 0 0 1] 
                                   #[0 0 0 500] 
                                   #[500 'nothing 'nothing 'nothing] 
                                   #[1 'nothing 'nothing 'nothing] 
                                   #[1.03 'nothing 'nothing 'nothing] 
                                   #[515.0 'nothing 'nothing 'nothing] 
                                   #[0.07 'nothing 'nothing 'nothing] 
                                   #[7357.142857142857 'nothing 'nothing 'nothing] 
                                   #[1 'nothing 'nothing 'nothing] 
                                   #[1.1 'nothing 'nothing 'nothing] 
                                   #[1 0 0 0] 
                                   #[2010 0 0 0] 
                                   #[2010 'nothing 'nothing 'nothing] 
                                   #[0 1 2 3] 
                                   #[1 1.1 1.2100000000000002 1.3310000000000004] 
                                   #[7357.142857142857 6688.311688311687 6080.2833530106245 5527.530320918749] 
                                   #[0 0 0 5527.530320918749] 
                                   #[5527.530320918749 'nothing 'nothing 'nothing] 
                                   #[5881.700118063753 'nothing 'nothing 'nothing] 
                                   #[5881.700118063753 'nothing 'nothing 'nothing]])))
    (check-within actual expected epsilon)))
