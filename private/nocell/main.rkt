#lang gamble

(require (except-in racket #%module-begin #%top-interaction)
         (only-in "../grid/sheet.rkt" atomic-value?)
         "../cell/assignment.rkt"
         "util.rkt"
         "parameters.rkt"
         racket/syntax
         syntax/id-table
         (prefix-in m: math/array)
         (for-syntax racket/syntax
                     (only-in racket make-list)))

(provide @
         ±
         +/-
         ~normal
         ~uniform
         stack-print
         sum
         product
         len
         nth
         halt
         (rename-out (=& =))
         (rename-out (<&  <))
         (rename-out (<=& <=))
         (rename-out (>&  >))
         (rename-out (>=& >=))
         (rename-out (+& +))
         (rename-out (-& -))
         (rename-out (*& *))
         (rename-out (/& /))
         (rename-out (expt& expt))
         (rename-out (define& define))
         (rename-out (define-values& define-values))
         (rename-out (if& if))

         (rename-out (datum #%datum))
         #%app
         #%module-begin
         #%top
         #%top-interaction

         require
         provide)

;; Datum
;;----------------------------------------------------------------------

(define (name-generator name [name-counter 0])
  (lambda () (make-name name (post-inc! name-counter))))

(define next-datum-name (name-generator "e"))

;;   #%datum
;; A datum expands to a stack with a single element
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     #'(list (make-assignment #:id    (next-datum-name)
                              #:expr  (m:vector*->array (#%datum . d) atomic-value?)
                              #:val   (m:vector*->array (#%datum . d) atomic-value?)))]))


;; Defining stack functions
;;----------------------------------------------------------------------

;; (define-primitive-stack-fn (f args ...) (f-str f-symb) body ...)
;;
;; Helper macro for defining the stack functions provided by this
;; module.  It expands to a definition of a function named "f", whose
;; implementation is given by the racket forms "body", and whose
;; results are labelled on the stack by symbols constructed from
;; "f-str" and a number (with a counter kept internally).  These names
;; are interned, so the name provided must be unique amoung all such
;; stack function definitions.
;;
(define-syntax (define-primitive-stack-fn stx)
  (syntax-case stx ()
    ;; f        : id?
    ;; args ... : stack? ...
    ;; f-str    : string?
    ;; f-name   : symbol?
    ;; body ... : expression? ...
    [(_ (f args ...) (f-str f-symb) body ...)
     (with-syntax ([(arg-vals ...) #'((val (stack-top args)) ...)]
                   [(arg-samples ...) #'((sampler (stack-top args)) ...)]
                   [(arg-ids ...) #'((id (stack-top args)) ...)]
                   [(rargs ...) (syntax-reverse #'(args ...))])
       #'(define f
           (let ((name-counter 0))
             (lambda (args ...)
               (let* ((result
                       ;; shadow the actual args (which are stacks)
                       ;; when evaluating the body (which is a racket
                       ;; expression expecting the value contents of
                       ;; the stacks)
                       (let ((args arg-vals) ...) body ...))
                      (result-sampler
                       (λ ()
                         (let ((args (arg-samples)) ...)
                           body ...)))
                      (args-stack (remove-duplicates-before
                       ;; args put on the stack in reverse order
                       (append rargs ...)))
                      (res-name (make-name f-str (post-inc! name-counter))))
                 (stack-push
                  (make-assignment
                   #:id      res-name
                   #:expr    `([f-symb ,(shape arg-vals) ...] ,arg-ids ...)
                   #:val     result
                   #:sampler result-sampler)
                  args-stack))))))]))

(define next-argument-name (name-generator "arg"))

(define (make-arg name s is-last)
  (stack-push
   (make-assignment
    #:id      (next-argument-name)
    #:name    (list name)
    #:expr    (id (stack-top s))
    #:val     (val (stack-top s))
    #:sampler (sampler (stack-top s))
    #:context (if is-last 'last-arg 'arg))
   s))

;; Given a number of stacks, combine them by putting the top of each
;; of the stacks first (in order), followed by the rest of each stack.
;; This is useful for combining stacks of arguments, where the value
;; of all of the arguments is followed by the values from which they
;; are computed (rather then interspersing the results with these
;; intermediate values, as a simple "append" would produce).
;;
(define (splice-argument-stacks . stacks)
  (apply append
         (map (lambda (x) (car x)) stacks)
         (map cdr stacks)))

;; Work-around for failing gamble contract
;;
(define (statistics-get-mean stats)
  (vector-ref (struct->vector stats) 3))
(define (statistics-get-cov stats)
  (vector-ref (struct->vector stats) 4))

;; helpers for define-stack-fn
;;
(define stack-fn-counter 1)
(define stack-fn-ids (make-bound-id-table))
(define (fn-name f)
  (let ((r (bound-id-table-ref stack-fn-ids f #f)))
    (if r
        r
        (begin (bound-id-table-set! stack-fn-ids f (post-inc! stack-fn-counter))
               (bound-id-table-ref stack-fn-ids f)))))

(define-for-syntax (is-last stx)
  (define stxe (syntax-e stx))
  (define len (length stxe))
  (datum->syntax
   stx
   (if (= len 0)
       null
       (append (make-list (sub1 len) #f) '(#t)))))

;; is-last: syntax -> syntax
;;
;; When given a syntax list, returns a syntax list of the same length,
;; where each element is #f, apart from the last, which is #t.
(define (is-last stx)
  (define stxe (syntax-e stx))
  (define len (length stxe))
  (datum->syntax
   stx
   (if (= len 0)
       null
       (append (make-list (sub1 len) #f) '(#t)))))


;; Like define-primitive-stack-fn, but the function body is assumed to
;; already be in terms of stack functions (that is, accepting stacks
;; as arguments and returning a stack).  Names of these functions do
;; not appear on the expression stack (although their primitive
;; subexpressions do).  This is a helper for implementing "define" in
;; the language.
;;
(define-syntax (define-stack-fn stx)
  (syntax-case stx ()
    ;; f        : id?
    ;; args ... : stack? ...
    ;; body ... : expression? ...
    [(_ (f args ...) body ...)
     (with-syntax* ([args-no-kw*
                     (filter (λ (x) (not (keyword? (syntax-e x))))
                             (syntax-e #'(args ...)))]
                    [(args-no-kw ...) (syntax->list #'args-no-kw*)]
                    [(rargs ...) (syntax-reverse #'(args-no-kw ...))]
                    [(is-last-arg ...) (is-last #'(args-no-kw ...))]
                    [f-str #'(symbol->string 'f)])
       #'(define f
           (let ((name-counter 0))
             (lambda (args ...)
               (if (> (length (current-calls)) (current-max-branching-depth))
                   (halt)
                   (let*
                       ((args-no-kw    (make-arg 'args-no-kw args-no-kw is-last-arg)) ...
                        (res-name      (make-name f-str (post-inc! name-counter)))
                        (res-name-mean (make-name f-str (post-inc! name-counter)))
                        (res-name-var  (make-name f-str (post-inc! name-counter)))
                        (result-stack
                         (parameterize ((current-calls
                                         (cons (cons
                                                (fn-name (datum->syntax #'stx 'f))
                                                res-name)
                                               (current-calls))))
                           body ...))
                        (top           (stack-top result-stack))
                        (result-val    (val top))
                        (args-stack    (splice-argument-stacks rargs ...)))
                     (let-values ([(result-mean result-var)
                                   (parameterize ([default-proposal (proposal:resample)])
                                     (when (deterministic-sampler?) (random-seed 0))
                                     (let ([s (mh-sampler (m:array->vector ((sampler top))))]
                                           [result-shape (m:array-shape ((sampler top)))])
                                       (for ([i (current-mh-burn-steps)])
                                         (s))
                                       (let ([stats (sampler->statistics s (current-mh-sample-steps))])
                                         (values 
                                          (m:vector->array result-shape (m:array->vector (Array-contents (statistics-get-mean stats))))
                                          (m:array-reshape (array-diag (Array-contents (statistics-get-cov stats))) result-shape)))))])
                       (remove-duplicates-before
                        (stack-push
                         (make-assignment #:id      res-name
                                          #:expr    (id top)
                                          #:val     (val top)
                                          #:sampler (sampler top)
                                          #:context 'result
                                          #:note    (note top))
                         
                         (make-assignment #:id      res-name-mean
                                          #:expr    result-mean
                                          #:val     result-mean
                                          #:context 'result-mean)
                         
                         (make-assignment #:id      res-name-var
                                          #:expr    result-var
                                          #:val     result-var
                                          #:context 'result-var)
                         (append
                          result-stack
                          args-stack))))))))))]))


;; Stack operator definitions
;;-----------------------------------------------------------------------

;; (define id expr)
;; (define (id args ...) body ...)
;;
;; The first form causes the result of evaluating expr to be bound by
;; id, and in addition renames the result at the top of the stack to
;; 'id.
;;
;; The second form defines a function taking the stack arguments args
;; and returning the result of evaluating body.
;;

(define (rename-define d stack)
  (define top (stack-top stack))
  (stack-push
   (struct-copy assignment top
                [name (cons d (name top))])
   (cdr stack)))

(define (copy-define-one-assignment d a)
  (struct-copy assignment a
               [id (next-datum-name)]
               [calls (current-calls)]
               [name (list d)]
               [expr (id a)]
               [context 'body]
               [note null])) ;; clear any note

(define (copy-define d stack)
  (define top (stack-top stack))
  (stack-push
   (copy-define-one-assignment d top)
   stack))

;; Grant an *unnamed* stack-top value a name, or copy a named variable
;; to one with the given name (d).
(define (rename-or-copy d stack)
  (if (null? (name (stack-top stack)))
      (rename-define d stack)
      (copy-define d stack)))

(define-syntax (define& stx)
  (syntax-case stx ()
    [(_ (id args ...) body ...)
     #'(define-stack-fn (id args ...) body ...)]

    [(_ id expr)
     #'(define id (rename-or-copy 'id expr))]))

(define-syntax (define-values& stx)
  (syntax-case stx ()
    [(_ (ids ...) expr)
     ;; copy-define (always), the first few elements of the stack to the ids
     ;;
     (with-syntax ([n (length (syntax->list #'(ids ...)))])
       #'(define-values (ids ...)
           (let ((expr* expr))
             (apply values
                    (map
                     (λ (id assn)
                       (stack-push (copy-define-one-assignment id assn) expr*))
                     '(ids ...)
                     (take expr* n))))))]))

;; halt represents termination of a calculation (called if the maximum
;; call depth is exceeded)
;;
(define-primitive-stack-fn (halt)
  ("halt" halt)
  (m:array +nan.0))

;; Label a value with a note (intended to appear as a note on the
;; cell(s), or as text somewhere adjacent to the value, as the
;; formatting rules allow).  Appears everywhere the value is
;; references.
(define (@ expr . note-strs)
  (define top (stack-top expr))
  (stack-push
   (struct-copy assignment top
                [note (cons (string-join (map stack-top-val note-strs) "\n")
                            (note top))])
   (cdr expr)))

;; if is a function (and consequently, its arguments are evaluated)
(define-primitive-stack-fn (if& test-expr then-expr else-expr)
  ("branch" if)
  (m:array-if test-expr then-expr else-expr))

(define-primitive-stack-fn (+& a b)
  ("sum" +) ;; results show up on the stack with names like "sum0"
            ;; and with expressions in terms of "+"
  (m:array+ a b))

(define-primitive-stack-fn (-& a b)
  ("diff" -)
  (m:array- a b))

(define-primitive-stack-fn (*& a b)
  ("prod" *)
  (m:array* a b))

(define-primitive-stack-fn (/& a b)
  ("quot" /)
  (m:array/ a b))

(define-primitive-stack-fn (=& a b)
  ("eq?" =)
  (m:array= a b))

(define-primitive-stack-fn (<& a b)
  ("lt?" <)
  (m:array< a b))

(define-primitive-stack-fn (<=& a b)
  ("le?" <=)
  (m:array<= a b))

(define-primitive-stack-fn (>& a b)
  ("gt?" >)
  (m:array> a b))

(define-primitive-stack-fn (>=& a b)
  ("ge?" >=)
  (m:array>= a b))

(define-primitive-stack-fn (expt& a b)
  ("expn" expt)
  (m:array-map expt a b))

(define-primitive-stack-fn (sum xs)
  ("vsum" sum)
  (m:array (m:array-all-fold xs +)))

(define-primitive-stack-fn (product xs)
  ("vprod" product)
  (m:array (m:array-all-fold xs *)))

;; emit the shape of an array (could be empty if a scalar)
(define-primitive-stack-fn (len a)
  ("l" len)
  (m:vector->array (m:array-shape a)))

;; indexing
(define nth
  (let ((name-counter 0))
    (lambda (n a)
      (let* ((n*       (m:array->vector (stack-top-val n)))
             (shape-a* (m:array-shape (stack-top-val a)))
             (select*  (m:build-array shape-a*
                                    (λ (idx) (if (equal? idx n*) 1 0))))
             (id       (make-name 'select (post-inc! name-counter)))
             (select   (list (make-assignment #:id   id
                                              #:expr select*
                                              #:val  select*))))
        (sum (*& select a))))))

;; two parameter distribution: gamble produces an error when mapping
;; over rest args here!
(define ((distribution-2-parameter distribution-kind)
         a b #:observations [observations '()] #:val v)
  (list (struct-copy assignment (stack-top v)
                     [sampler
                      (λ ()
                        (define dist
                          (distribution-kind ((sampler (stack-top a)))
                                             ((sampler (stack-top b)))))
                        (unless (null? observations)
                          (for ([obs (stack-top-val observations)])
                            (observe-sample dist obs)))
                        (sample dist))])))

(define ~normal (distribution-2-parameter normal-dist))
(define ~uniform (distribution-2-parameter uniform-dist))

;; don't permit the observations keyword argument with the +/- form
(define (+/- mean stdev) (~normal mean stdev #:val mean))

(define ± +/-)
