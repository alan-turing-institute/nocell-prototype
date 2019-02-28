#lang racket

(provide
  ; Our goal is to turn nocell (a very limited subset of Racket) into stack (an imutable stack of expressions, where each expression can only refer to rows in the stack that were defined before it)
  nocell->stack
  ; This is a useful cross check, it calculates the value of the last entry on the stack. This should be the same as the return value of the nocell.
  stack->value
  ; This takes a stack and creates a grid ready for raw/to->raw.rkt
  stack->grid
  )

; "Any sufficiently complicated C or Fortran program contains an ad-hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp." ... sums up the code below.

; Our goal is to turn nocell (a very limited subset of Racket) into stack (an imutable stack of expressions, where each expression can only refer to rows in the stack that were defined before it)
(define (nocell->stack nocell)
  (stack-clear)
  (stack-push "result" (evaluate-body nocell (hash)))
  stack
  )

; This is our stack. It is a 2D stack
; FIXME: Currently a global, figure out a way of safely passing into the code
(define stack null)
; Starts as an empty list
(define (stack-clear) (set! stack null) (stack-pointer-clear!))
; The stack is an ordered list of rows
(define (stack-row i) (list-ref stack i))
; The stack pointer points to the first _empty_ position and we have a bunch of ways of manipulating it
(define stack-pointer-start (cons  0 'no-column) )
(define stack-pointer stack-pointer-start)
(define (stack-pointer-set new-pointer) (set! stack-pointer new-pointer))
(define (stack-pointer-clear!) (stack-pointer-set stack-pointer-start))
(define (stack-pointer-row-set new-row) (stack-pointer-set (cons new-row (stack-pointer-col))))
(define (stack-pointer-row) (stack-pointer-get-row stack-pointer))
(define (stack-pointer-get-row pointer) (car pointer))
(define (stack-pointer-col) (stack-pointer-get-col stack-pointer))
(define (stack-pointer-get-col pointer) (cdr pointer))
(define (stack-pointer-col-set new-col) (stack-pointer-set (cons (stack-pointer-row) new-col)))
(define (stack-pointer-increment-row!) (stack-pointer-row-set (+ 1 (stack-pointer-row))))
(define (stack-pointer-increment-column!) (stack-pointer-col-set (+ 1 (stack-pointer-col))))
(define (stack-column-mode-enter) (stack-pointer-col-set 0))
(define (stack-column-mode-exit) (stack-pointer-col-set 'no-column))
; A row has an index
(define (stack-row-index row) (list-ref row 0))
; A row has an optional name
(define (stack-row-name row) (list-ref row 1))
; A row has an expression
(define (stack-row-expression row) (list-ref row 2))
; An expression might refer to a (row i) which refers to the expression on row i
(define (row i) (stack-row-expression (stack-row i)))
; An expression might be a list, in which case (col i) gets the i'th element (starting at zero)
(define (col j row) (if (list? row) (list-ref row j) row))
; This returns an expression that points to the given stack pointer, e.g., (row i)
(define (stack-reference pointer) 
  (if (equal? 'no-column (stack-pointer-get-col pointer))
      `(row ,(stack-pointer-get-row pointer))
      `(col ,(stack-pointer-get-col pointer) (row ,(stack-pointer-get-row pointer)))
      )
  )

; This allows us to set items beyond the end of the list, with any intervening items being given nil values
(define (list-set-col lst i x) (if (>= i (length lst)) (list-set-col (append lst '(())) i x) (list-set lst i x)))
; When we push onto the stack, we get the (row i) that we pushed onto
(define (stack-push name expression)
  (let* ([pointer stack-pointer] [row (stack-pointer-get-row stack-pointer)] [col (stack-pointer-get-col stack-pointer)])
    (let ([stack-row (if (>= row (length stack)) `(,row ,name '()) (stack-row row))])
      (if (equal? col 'no-column)
        (set! stack-row (list-set-col stack-row 2 expression))
        (begin (set! stack-row (list-set-col stack-row 2 (list-set-col (stack-row-expression stack-row) col expression))))
      )
      (set! stack (list-set-col stack row stack-row))
    )   
    (stack-pointer-increment-row!)
    (stack-reference pointer)
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
    ['first (evaluate-builtin-procedure 'col (cons 0 arguments) definitions)] 
    ['last (evaluate-last arguments definitions)] 
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
  (cons name (evaluate-list arguments definitions))
  )

(define (evaluate-list lst definitions) (map (lambda (x) (evaluate x definitions)) lst))

(define (actual-length arg definitions)
  (let ([ v  (interpret-stack stack arg definitions)])
    (if (list? v) (length v) 1)
    )
  )

(define (evaluate-last arguments definitions) 
  `(col ,(- (actual-length (first arguments) definitions) 1) ,@(evaluate-list arguments definitions)) 
  )

; Special case, the map function transforms itself into an unmapped list of calculations
(define (evaluate-map function lists definitions)
  (stack-column-mode-enter)
  (let* ([start-row (stack-pointer-row)]
         [ref (map (lambda (arguments) 
                     (stack-pointer-row-set start-row)
                     (let ([r (evaluate-procedure function arguments definitions)])
                       (stack-pointer-increment-column!)
                       r)
                     ) (lists->map-arguments lists definitions )
                   )
              ])
    (stack-column-mode-exit)
    ref
    )
  )

(define (lists->map-arguments lists definitions)
  (map (lambda (position) (map (lambda (list) `(col ,position ,list) ) lists)) (range 0 (length (interpret-stack stack (first lists) definitions)) 1))
  )

; Special case, the foldl function transforms itself into an unfolded list of calculations
(define (evaluate-foldl function initial lists definitions)
  (stack-column-mode-enter)
  (let* ([start-row (stack-pointer-row)]
         [ref (foldl (lambda (arguments reference) 
                       (stack-pointer-row-set start-row)
                       (let ([r (evaluate-procedure function (append arguments `(,reference)) definitions)])
                         (stack-pointer-increment-column!)
                         r)
                       ) initial (lists->map-arguments lists definitions )
                     )
              ])
    (stack-column-mode-exit)
    ref
    )
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

(define (procedure-application? expression)
  (if (list? expression)
    (if (symbol? (first expression))
      #t
      #f
      )
    #f
    )
  )

; This interprets an expression in the context of the stack, returning the calculated value
(define (interpret-stack stack expression definitions)
  (match expression
         [(? null?) expression]
         [(? number?) expression]
         [(? string?) expression]
         ; The if check here is so that we don't go into an infinite loop for undefined symbols
         [(? symbol?) (if (equal? (evaluate-symbol expression definitions) expression) expression (interpret-stack stack (evaluate-symbol expression definitions) definitions))]
         [`(row ,index) (interpret-stack stack (row index) definitions)]
         [`(col ,index (row ,row-index)) (let ([r (row row-index)]) 
                                           (interpret-stack stack (col index (if (procedure-application? r) 
                                                                         (interpret-stack stack r definitions)
                                                                         r
                                                                         )) definitions))]
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

(define (stack->grid this-stack)

  (define (expression-value->grid-value expression)
    (match expression
           ([? procedure-application?] (procedure->grid-procedure expression))
           (_ expression)
           )
    )

  (define (procedure->grid-procedure expression)
    (define (infix-maths a operator b) (string-join (list "(" (rewritten-procedure a) (symbol->string operator) (rewritten-procedure b) ")") ""))
    (define columns '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z AA AB AC AD ))
    (define (col-ods i) (symbol->string (list-ref columns (+ i 1))))
    (define (last-col-in-row-ods j) (col-ods  (- (actual-length `(row , j) (hash)) 1)))
    (define (row-ods j) (number->string (+ j 1)))
    (define (maybe-range j) 
(let ([start (string-join (list "." (col-ods 0) (row-ods j)) "")]
			[finish (string-join (list "." (last-col-in-row-ods j) (row-ods j)) "")])
(if (equal? start finish) (string-join (list "[" start "]") "")
(string-join (list "[" start ":" finish "]") ""))))

    (define (rewritten-procedure expression)
      (match expression
             [`(row ,j) (maybe-range j)]
             [`(col ,i (row ,j)) (string-join (list "[." (col-ods i) (row-ods j) "]") "")]
             [`(+ ,a ,b) (infix-maths a '+ b)]
             [`(- ,a ,b) (infix-maths a '- b)]
             [`(/ ,a ,b) (infix-maths a '/ b)]
             [`(* ,a ,b) (infix-maths a '* b)]
             [`(expt ,a ,b) (infix-maths a '^ b)]
             [( ? procedure-application?) (string-join (list (symbol->string (first expression) ) "(" (string-join (map rewritten-procedure (rest expression) ) ", " ) ")") "")]
             [(? number?) (number->string expression)]
             [_ expression]
             )
      )
      (cons (interpret-stack this-stack expression (hash)) (rewritten-procedure expression))
    )

  (define (stack-row-expression->grid-row expression)
      (match expression
             ([? empty?] '())
             [`(row ,j) (map (lambda (i) (expression-value->grid-value `(col ,i (row ,j)) )) (sequence->list (in-range (- (actual-length `(row , j) (hash)) 0))))]
             ([? procedure-application?] (list (expression-value->grid-value expression)))
             ([? list?] (map expression-value->grid-value expression))
             (_ (list (expression-value->grid-value expression)))
             )
    )

  (define (maybe-symbol->string maybe) (if (symbol? maybe) (symbol->string maybe) maybe) )
  (define (stack-row->grid-row row)
    (let* ([name (maybe-symbol->string (stack-row-name row))]
          [expression (stack-row-expression row)]
          [grid-values (stack-row-expression->grid-row expression)]
          )
      (if (empty? grid-values) (list name) (cons name grid-values))
    )
    )
  (set! stack this-stack)
  (map stack-row->grid-row this-stack) 
  )

