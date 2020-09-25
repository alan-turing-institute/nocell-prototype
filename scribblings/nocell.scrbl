#lang scribble/manual
@require[
  scribble/example
  racket/sandbox
  @for-label[nocell]]

@(define nocell-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'nocell)))

@title{nocell}
@author{ots22, tamc, triangle-man}

@defmodule[nocell]

A language for building probabilistic spreadsheets.

Development of nocell is hosted at @url{https://github.com/alan-turing-institute/nocell}.

@section[#:tag "intro"]{Introduction}

Where to start?

@bold{A user new to nocell, who is unfamiliar with Racket.}  Start
with the @secref{lang-guide}.

@bold{Users familiar with Racket, who wish to use nocell.}  For an
in-depth reference, which assumes familiarity with the Racket language
and ecosystem on which nocell is based, see the @secref{lang-ref}.

@bold{Someone who wishes to add functionality to nocell, or to modify
it.} To help farmiliarize yourself with the codebase, consult the
@secref{dev-guide}, containing documentation for the lower-level
routines and some general pointers.  The other language resources may
be useful as well of course.  Upstream contributions are most welcome:
please see @secref{contributing}.

@section[#:tag "get-started"]{Getting Started}

@subsection{Installation}

Nocell can be installed as a Racket package, or, for macOS users, from
Homebrew.

@subsubsection{Installation with Homebrew}

TODO

@subsubsection{Installation with Raco}

If you do not have Racket installed already, you will need to
@link{install it}.

After that, the nocell package can be installed using the raco package
manager, by running

@commandline{raco pkg install git://github.com/alan-turing-institute/nocell}

in your shell.

To use the nocell runner scripts from the command line, the
@filepath{scripts} directory should be in your shell's path.

@subsection{Your first nocell program}

This section illustrates the creation of a simple 


@subsubsection{Running a nocell program}

Nocell programs can be used to generate spreadsheets via a selection
of helper scripts.  For example, in your shell, running

@commandline{nocell-to-ods foo.nocell}

@margin-note{Assumes the @filepath{scripts} directory is in your path:
see @secref{Installation}}

will produce @filepath{foo.ods}, an OpenDocument spreadsheet, assuming
that @filepath{foo.nocell} exists and is a valid nocell program.

A nocell program is really a Racket module, and may be required by
another Racket program.  When this is done, anything @racket[provide]d
by your nocell module is available as a cell data structure.  Some
advice on this mode of usage is provided in the next section.

@section[#:tag "lang-guide"]{Language Guide}

This guide is a quick overview of the nocell language, for users who
would like to produce spreadsheet models quickly.  Nocell is a small
language, and this guide introduces it fully, but it is less detailed
than the complete @secref{lang-ref}.

Some example results are given, and for these the resulting
spreadsheet is represented as a table, like the following one, where
cells A1 and B1 contain numbers, A2 contains the string "hello" and B2
contains a formula.

@tabular[#:cell-properties '((border))
         #:column-properties '(center center)
        (list (list " " @bold{A} @bold{B})
              (list @bold{1} "1" "3.5")
              (list @bold{2} "'hello" "=A1+B1"))]

First we introduce the basic elements of the language, before
introducing the probabilistic aspects.

@subsection{Basics}

@itemlist[
        @item{Your nocell program should begin with the line

              @code{#lang s-exp nocell}}
        @item{Nocell is based on Racket, which is a Lisp.}
        @item{Editor support for Racket (which nocell can use) (TODO link)}
        @item{Forms}
        ]

@subsection{Program to spreadsheet}

Nocell (and in particular, the nocell-to-ods script) produces a
spreadsheet from a program.  What does this mean?

A spreadsheet itself is a limited kind of program.  Some popular
spreadsheet applications support extensibility via a scripting
language (for example, a user of Microsoft Excel can write extensions
in VBA).  We do not use this kind of functionality, and by
@italic{spreadsheet} we mean only the contents of the cells.

A program that converts one representation of a program to another is
a compiler, and so we will refer to nocell in this way.

To the extent possible [TODO note on this], the nocell compiler
produces a spreadsheet representing the same program as the nocell
program.  This can mean modifying the structure of the program in
certain ways.  For example, since there is no concept of a loop in a
spreadsheet, any loops in the program are unrolled, up to some
pre-defined depth.

For example, consider the following short nocell program,

@codeblock{
  #lang s-exp nocell
  (define x #[1 2 3])
  (define y #[10 100 1000])
  (define result (+ x y))
}

which compiles to

@tabular[#:cell-properties '((border))
         #:column-properties '(center center)
        (list (list " " @bold{A} @bold{B} @bold{C} @bold{D})
              (list @bold{1} "'x" "1" "2" "3")
              (list @bold{2} "'y" "10" "100" "1000")
              (list @bold{3} "'result" "=B1+B2" "=C1+C2" "=D1+D2"))]

Some things to notice about this spreadsheet:
@itemlist[

@item{The calculation proceeds "downwards".}

@item{Multiple values are spread across a row.}

@item{Values and formulae are labelled in the first column with their name, as
@racket[define]d in the program.}

@item{The use of a variable in the program becomes an appropriate
references in the spreadsheet.}

@item{The spreadsheet is populated with the same values used in the program.}
]

Some of this is a result of particular layout choices.  We intend for
these choices to be customizable. TODO More on layout

@subsection{Literals}

The types of literal reflect those commonly found in spreadsheets:
@itemlist[@item{number}
          @item{string}
          @item{boolean}]

@bold{Numbers} are represented internally as double-precision
floating-point numbers (I think this is false, and generally a good
thing, but e.g. (/ 1 0) raises an exception, which is undesirable (vs
NaN)).  Some numbers:

@itemlist[@item[@racket[-1]]
          @item[@racket[0.0]]
          @item[@racket[2.4e15]]
          @item[@racket[+inf.0]]]

How these map to number representations used by a particular
spreadsheet will depend on what is supported or available in the
spreadsheet format and the corresponding nocell backend.

There are two @bold{Boolean} values, @racket[#t] (true) and
@racket[#f] (false).

@margin-note{The array functionality in nocell is built on
@racketmodname[math/array], and inherits a lot of behaviour from it,
for example, broadcasting.}
@bold{Arrays} of any rank can be specified using @code{#[ ]}
syntax, which can be nested.  In fact, @bold{every} value in nocell is
an array, perhaps of rank zero.

@examples[#:eval nocell-evaluator
          #:label "Some rank zero arrays:"
1
(+ 1 1)
"a string"
]

@examples[#:eval nocell-evaluator
#:label "Arrays of rank one or greater:"
#[1 2 3 4 5 6 7 8 9 10]
#[#[1 0 0]
  #[0 1 0]
  #[0 0 1]]
#[1 "a" #f]
]

@examples[#:eval nocell-evaluator
          #:label "Arrays must be rectangular:"
(eval:error #[#[1 2 3] #[1 2]])
]

@subsection{Defining values}

There is a special form, @racket[define], that can be used to give
names to values.  There is no mutation in the language: a name (in a
particular scope) always refers to the same value after it has been
defined.

@racketblock[(define id @#,(italic "expr"))]

evaluates the expression @italic{expr} and binds the result to the
identifier @racket[id].

Scoping rules are inherited from Racket.

There is another variant of @racket[define] for defining functions,
see below.

@subsection{Comments}

Nocell distinguishes between @bold{comments} and @bold{annotations}.

A @bold{comment}, as in ordinary Racket, follows a semicolon: the text
of the comment is intended for readers of the nocell program and is
ignored by the parser.  It will not show up in any generated
spreadsheet.

@codeblock[#:keep-lang-line? #f]{
#lang s-exp nocell                         
;; a comment
(+ 1 1)}

A value can also be @bold{annotated} with some explanatory text.
Unlike a comment, this text will show up in the spreadsheet, alongside
the value in question.  The syntax for adding an annotation is

@racketblock[(\@ expr string ...)]

The expression @italic{expr} is evaluated, and the resulting value
annotated with the subsequent strings (joined with newlines).

@racketblock[
(define result (+
                [\@ (* 2 3) "two times three"
                           "is six"]
                1))
]

@tabular[#:cell-properties '((border))
         #:column-properties '(center center)
        (list (list " " @bold{A} @bold{B} @bold{C})
              (list @bold{1} " " "2" " ")
              (list @bold{2} " " "3" " ")
              (list @bold{3} " " "=B1*B2" (list "'two times three" (linebreak) "is six"))
              (list @bold{4} " " "1" " ")
              (list @bold{5} "'result" "=B3+B4" " "))]
              


@subsection{Builtins}

There is a selection of builtin functions, designed to mirror the
built-in functions available for use in spreadsheets.  For some
spreadsheet backends, a builtin might not translate directly into a
single spreadsheet function.

@subsubsection{Binary arithmetic operations}

The following operations are supported: @racket[+], @racket[-],
@racket[*], @racket[/], @racket[expt]

@examples[#:eval nocell-evaluator
          #:label #f
(expt 2 3)
(eval:error (/ 1 0))
(+ #[1 10 100] #[0 1 0])
(expt 2 #[0 1 2 3 4 5])
]

The last example demonstrates @secref{broadcasting}.

@subsubsection{Boolean comparison functions}

@racket[=], @racket[<], @racket[<=], @racket[>], @racket[>=]

@subsubsection{Conditionals}

@racket[if] is a function and as such it fully evaluates its
arguments.  This may be surprising, but it is desirable because the
results of both branches can then be included in the spreadsheet (and
if a value on which the condition depends changes, the other branch
can be taken).

It is possible to fully explore all branches because
@itemlist[
@item{there are no side effects in the language}
@item{certain errors show up as special error values,
instead of raising exceptions}
@item{nocell programs always terminate}
]

See also @secref{recursion}.

@examples[#:eval nocell-evaluator
#:no-prompt
#:label #f
(define x 0.0)
(define result
  (if (> x 0.0)
      (/ 1.0 x)
      -1.0))
]

@tabular[#:cell-properties '((border))
         #:column-properties '(center center center)
        (list (list " " @bold{A} @bold{B} (list "value in " @bold{B}))
              (list @bold{1} "'x" "0.0" @italic{0})
              (list @bold{2} "" "0.0" @italic{0}) 
              (list @bold{3} "" "=B1>B2" @italic{FALSE})
              (list @bold{4} "" "=1.0/B1" @italic{#DIV/0!})
              (list @bold{5} "" "-1.0" @italic{-1.0})
              (list @bold{6} "'result" "=IF(B3,B4,B5)" @italic{-1.0}))]


@subsubsection{Folds}

@racket[sum], @racket[product]

@examples[#:eval nocell-evaluator
#:label #f
(sum #[1 2 3 4])
(product #[1 2 3 4])
]

@subsubsection{Shape}

The shape of an array can be obtained with @racket[len], which returns
a one-dimensional array.

@examples[#:eval nocell-evaluator
#:label #f
(len 1)
(len #[1 2 3])
(len #[#[1 2 3]])
(len #[#[1 2 3] #[1 2 3]])
]


@subsubsection{Indexing}
@racket[nth]



@subsection{Defining Functions}

May be recursive

Mention halt

Limit to depth

@subsection{Elements of Probabilistic Programming}



@subsection{Modular programming: Importing and exporting}

require, provide

@section[#:tag "lang-ref"]{Language Reference}

@subsection{Overview}

Nocell is a language for building probabilistic spreadsheets.

Nocell -> Cell -> Sheet -> .ods, .xlsx, etc.

Interacting with nocell inside Racket

Idea: start in the middle (cell), and define a stack/assignment/cell data structure.  Then nocell evaluation model. Type conventions, then nocell.

Later, sheet, and converting from cell.

@subsection{The nocell language}

Each nocell program begins with the line

@;;@racketmod{s-exp nocell}

@defform[#:kind "procedure"
         (stack-print expr)]{
Prints the result of evaluating expr in "stack" form.  Most useful
when used interactively.}

@deftogether[
  (@defproc[(+ [a (Array number?)] [b (Array number?)]) (Array number?)]
   @defproc[(- [a (Array number?)] [b (Array number?)]) (Array number?)]
   @defproc[(* [a (Array number?)] [b (Array number?)]) (Array number?)]
   @defproc[(/ [a (Array number?)] [b (Array number?)]) (Array number?)]
   @defproc[(expt [a (Array number?)] [b (Array number?)]) (Array number?)])]{
   Elementwise arithmetic operations
   }

@deftogether[
  (@defproc[(= [a (Array number?)] [b (Array number?)]) (Array boolean?)]
   @defproc[(< [a (Array number?)] [b (Array number?)]) (Array boolean?)]
   @defproc[(<= [a (Array number?)] [b (Array number?)]) (Array boolean?)]
   @defproc[(> [a (Array number?)] [b (Array number?)]) (Array boolean?)]
   @defproc[(>= [a (Array number?)] [b (Array number?)]) (Array boolean?)])]{
   Elementwise boolean operations
   }

@defproc[(sum [a (Array number?)]) number?]{
  Sum of all elements of @racket[a]
  @examples[#:eval nocell-evaluator
            #:label #f
            (stack-print (sum #[2 3 4]))]
  }

@defproc[(product [a (Array number?)]) number?]{
  Product of all elements of @racket[a].
  @examples[#:eval nocell-evaluator
            #:label #f
            (stack-print (product #[2 3 4]))]
  }

@defproc[(len [a (Array any/c)]) (Array number?)]{
  Returns the shape of @racket[a], as an array of rank one. TODO example
  }

@defproc[(nth [n (Array number?)] [a (Array number?)]) number?]{
  Return the element of array @racket[a] with index @racket[n], where
  @racket[n] is a vector with length equal to the rank of @racket[a].
  Note: at the moment, it is only possible to select from arrays of
  numbers.}

@defproc[(if [test boolean?] [then any/c] [else any/c]) any/c]{
  Conditional.  Note that @racket[if] is a function, and as such it
  evaluates its arguments.  Recursion is permitted, since the
  call depth is bounded (TODO link to relevant section).  }

@defproc[(halt) halt?]{
  Returns a special error-value to represent the termination of a calculation.
  }

@defform[(\@ expr string ...)]{
The strings are joined with newlines,
and the resulting string is added to the note [TODO note is a link to
assignment] of the value resulting from evaluating the expression
@racket[expr]
}

@defform[(define id expr)]{Associates the name @racket[id] with the
result of evaluating @racket[expr].  If the value of @racket[expr]
does not currently have a name, @racket[id] is associated with it,
without introducing another assignment.  On the other hand, if the
value of @racket[expr] already has a name (for example, being the
result of a named definition already), then a new assignment is
introduced, being a reference to @racket[expr] and named @racket[id]}

@subsection{Cell data structures}

@subsection{Sheet data structures}

@section[#:tag "dev-guide"]{Developers' guide}



@subsection{Package layout}



@section[#:tag "contributing"]{Contributing}

