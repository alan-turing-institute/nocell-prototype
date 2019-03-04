#lang racket/base

(provide (struct-out assignment)
         make-stack
         stack-push
         stack-top)

;; Stacks
;; -----------------------------------------------------------------------
;; A stack is a list of lists, of the form "(name scope expr val)"
;; where name (: symbol?) is the name of the entry on the stack, scope
;; (: list?) is a list of any enclosing definitions, expr (: list) is
;; the expression used to compute it (which can refer to names deeper
;; on the stack) and val is the value of the result.
;;
(define (make-stack)        (list))
(define (stack-push v s)    (cons v s))
(define (stack-top s)       (car s))

;; Our stack is a list with elements that are the following struct,
;; each representing a named assignment, with the following elements:
;;
;; - id (: symbol?) is the unique name of the entry on the stack,
;;
;; - name (: (List symbol?)) is a (possibly empty) list of
;; user-defined names given to a stack entry
;;
;; - calls (: (List pair?)) are the current calls to user defined
;; functions for a stack entry (the call stack), where each entry is a
;; pair of the function name and the result id
;; 
;; - expr (: list?) is the expression used to compute it (which can
;; refer to names deeper on the stack).  The function name is
;; annotated with the shape of its arguments, for example, an entry of
;; ([+ () (3)] %e1 %e2) means the sum of the scalar %e1 and the vector
;; of three elements %e2 (with broadcasting).
;;
;; - val is the value of the result of evaluating expr
;;
(struct assignment (id name calls expr val) #:transparent)
