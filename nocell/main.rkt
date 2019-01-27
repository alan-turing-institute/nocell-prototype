#lang racket

(provide
  ; Our goal is to turn nocell (a very limited subset of Racket) into stack (an imutable stack of expressions, where each expression can only refer to rows in the stack that were defined before it)
  nocell->stack
  ; This is a useful cross check, it calculates the value of the last entry on the stack. This should be the same as the return value of the nocell.
  stack->value
  )

(require list-util rackunit)

; "Any sufficiently complicated C or Fortran program contains an ad-hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp." ... sums up the code below.

; Our goal is to turn nocell (a very limited subset of Racket) into stack (an imutable stack of expressions, where each expression can only refer to rows in the stack that were defined before it)
(define (nocell->stack nocell)
  (set! stack null)
  (stack-push "result" (evaluate-body nocell (hash)))
  stack
  )

; This is our stack
; FIXME: Currently a global, figure out a way of safely passing into the code
(define stack null)
; The stack is an ordered list of rows
(define (stack-row i) (list-ref stack i))
; A row has an index
(define (stack-row-index row) (list-ref row 0))
; A row has an optional name
(define (stack-row-name row) (list-ref row 1))
; A row has an expression
(define (stack-row-expression row) (list-ref row 2))
; An expression might refer to a (row i) which refers to the expression on row i
(define (row i) (stack-row-expression (stack-row i)))
; An expression might be a list, in which case (col i) gets the i'th element (starting at zero)
(define (col j row) (list-ref row j))
; When we push onto the stack, we get the (row i) that we pushed onto
(define (stack-push name expression)
  (let* ([i (length stack)])
    (set! stack (append stack `((,i ,name ,expression))))
    `(row ,i)
    )
  )

; TODO: Make this print with nicely aligned columns
(define (stack-print)
  (for ([line stack]) (println line))
  )

; Evaluates an expression and returns it in stack form
(define (evaluate expression definitions)
  (match expression
    [(? symbol?) (evaluate-symbol expression definitions)]
    [(? number?) expression]
    [(? string?) expression]
    [`(let ,bindings .,body) (evaluate-let bindings body definitions)]
    [`(lambda ,parameters .,body) expression] ; Not sure if we should ever really permit this, but leaving it makes debugging easier
    [`(map ,function .,lists) (evaluate-map function lists definitions)] ; We special case this in order to turn the map into columns
    [`(foldl ,function ,initial .,lists) (evaluate-foldl function initial lists definitions)] ; We special case this in order to turn the fold into columns
    [`(,name . ,arguments) (evaluate-procedure name arguments definitions)]
    [`() expression]
    )
  )

; Evaluates a list of expressions, returning the evaluated last one.
; Mainly useful because some of the expressions may define new variables or functions
; that are then visible to the final expression
(define (evaluate-body body definitions)
  (define top-level-definitions definitions)
  (if (empty? body) null
      (let-values ([(expressions last-expression) (split-at-right body 1)])
        (for-each (lambda (expression)
                    (match expression
                      [`(define (,name .,parameters) .,function-body) (set! top-level-definitions (hash-set top-level-definitions name (append `(lambda ,parameters) function-body)))]
                      [`(define ,name ,value) (set! top-level-definitions (hash-set top-level-definitions name (stack-push name (evaluate value top-level-definitions))))]
                      [_ (stack-push "" (evaluate expression top-level-definitions))]
                      )) expressions)
        (evaluate (last last-expression) top-level-definitions)
        )
      )
  )

; When we evaluate a let, if the value is not a lambda then we evaluate it and put it
; on the stack. If it is a lambda then it will be pushed onto the stack when it is called.
; In both cases, the binding will also be added to the definitions
(define (evaluate-let bindings body definitions)
  (let* ([new-bindings (map (lambda (binding) (maybe-replace-binding binding definitions)) bindings)]
         [new-definitions (hash-multi-set definitions new-bindings)]
         )
    (evaluate-body body new-definitions)
    )
  )

; If the definition is for a variable then we dump the definition to the stack and replace
; references to it with the position in the stack.
; If the defnition is for a function then we store the function for later use.
(define (maybe-replace-binding binding definitions)
  (let* ([name (first binding)]
         [value (last binding)]
         )
    (match value
      [`(lambda . ,arguments) (cons name value)]
      [_ (cons name (stack-push name (evaluate value definitions)))]
      )
    )
  )

; Evaluates procedures which may or may not be defined here, or built in
(define (evaluate-procedure name arguments definitions)
  (define (defined? name) (hash-has-key? definitions name))
  (match name
    [`(lambda ,parameters .,body) (evaluate-lambda parameters body arguments definitions)]
    [(? defined?) (evaluate-defined-procedure name arguments definitions)]
    [_ (evaluate-builtin-procedure name arguments definitions)]
    )
  )

; Evaluates a lambda
(define (evaluate-lambda parameters body arguments definitions)
  (stack-push "" (let* ([argument-bindings (map list parameters arguments)]) (evaluate-let argument-bindings body definitions)))
  )

; Evaluates built in procedures by writing out the code to the stack
; to set the parameters to the correct values, then push to the stack
; whatever is needed to carry out the procedure and then returning a
; reference to the result on the stack.
(define (evaluate-defined-procedure name arguments definitions)
  (let* ([definition (evaluate-symbol name definitions)])
    (match definition 
      [`(lambda ,parameters .,body) (stack-push name (let* ([argument-bindings (map list parameters arguments)]) (evaluate-let argument-bindings body definitions)))]
      )
    )
  )

; This is for built-in procedures 
(define (evaluate-builtin-procedure name arguments definitions)
  (cons name (map (lambda (argument) (evaluate argument definitions)) arguments))
  )

; Special case, the map function transforms itself into an unmapped list of calculations
; FIXME: Each column currently ends up being a separate row, need to fix that.
(define (evaluate-map function lists definitions)
  (map (lambda (arguments) (evaluate-procedure function arguments definitions)) (lists->map-arguments lists definitions))
  )

(define (lists->map-arguments lists definitions)
  (map (lambda (position) (map (lambda (list) `(col ,position ,list) ) lists)) (range 0 (length (interpret-stack stack (first lists) definitions)) 1))
  )

; Special case, the foldl function transforms itself into an unfolded list of calculations
; FIXME: Each column currently ends up being a separate row, need to fix that.
(define (evaluate-foldl function initial lists definitions)
  (foldl (lambda (arguments reference) (evaluate-procedure function (append arguments `(,reference)) definitions)) initial (lists->map-arguments lists definitions))
  )

; This looks up a defined symbol or, if not defined, returns the symbol
(define (evaluate-symbol symbol definitions)
  (if (hash-has-key? definitions symbol)
      (hash-ref definitions symbol)
      symbol
      )
  )

(define (hash-multi-set hash key-value-pairs)
  (foldl (lambda (pair hash) (hash-set hash (car pair) (cdr pair))) hash key-value-pairs)
  )

(define-namespace-anchor anchor)
(define namespace (namespace-anchor->namespace anchor))

; This interprets an expression in the context of the stack, returning the calculated value
(define (interpret-stack stack expression definitions)
  (match expression
    [(? null?) expression]
    [(? number?) expression]
    [(? string?) expression]
    ; The if check here is so that we don't go into an infinite loop for undefined symbols
    [(? symbol?) (if (equal? (evaluate-symbol expression definitions) expression) expression (interpret-stack stack (evaluate-symbol expression definitions) definitions))]
    [`(row ,index) (interpret-stack stack (row index) definitions)]
    [`(col ,index ,list) (interpret-stack stack (col index (interpret-stack stack list definitions)) definitions)]
    [`(first ,list) (interpret-stack stack (col 0 (interpret-stack stack list definitions)) definitions)]
    [`(last ,list) (interpret-stack stack (last (interpret-stack stack list definitions)) definitions)]
    ; FIXME: This is buggy, I can't get the quoting right
    [`(,function .,arguments) #:when (symbol? function) (let ([interpreted-expression (append `(,function) (map (lambda (argument) (interpret-stack stack argument definitions)) arguments))])
                                                          (eval interpreted-expression namespace))]
    [(? list?) (map (lambda (element) (interpret-stack stack element definitions)) expression)]
    [_ expression]
    )
  )

; Works out the value of the last expression on the stack
(define (stack->value stack)
  (let ([result (stack-row-expression (last stack))])
    (interpret-stack stack result (hash))
    )
  )
