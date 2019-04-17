#lang racket

#| 
Convert a single `sheet` to a bytestream representation of an OpenDocument Spreadsheet document [odf,
Part 1, 2.2.4]. 

[odf] Open Document Format for Office Applications (OpenDocument) Version 1.2 

TODO and LIMITATIONS
 - Produces fods, the single-file XML format
 - Currently only copes with cells whose values are:
    - cell-value?
    - cell-addr? (not cell-range?)
    - cell-app? 
|#


;; ---------------------------------------------------------------------------------------------------

(require racket/contract
         math/array
         sxml
         "sheet.rkt"
         "openformula.rkt")

(provide (all-defined-out))

(define *MIMETYPE* "application/vnd.oasis.opendocument.spreadsheet")

;; ---------------------------------------------------------------------------------------------------
;; Export sheet to a directory; zip the directory; and delete the original directory

;; sheet? path-string? -> void
;; `path` should not be the name of an existing directory.  
(define (sheet-write-ods sheet path)
  (call-with-output-file "content.xml" #:exists 'replace
    (λ (out) (srl:sxml->xml (ods sheet) out))))

;; ---------------------------------------------------------------------------------------------------
;; Wrap XML boilerplate for the file around the exported sheet 

;; sheet? -> sxml?
(define (ods sheet)
  `(*TOP*
    (@ (*NAMESPACES*
        [office "urn:oasis:names:tc:opendocument:xmlns:office:1.0"]
        [table  "urn:oasis:names:tc:opendocument:xmlns:table:1.0"]
        [text   "urn:oasis:names:tc:opendocument:xmlns:text:1.0"]))
    (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
    (office:document
     (@ (office:version "1.2")
        (office:mimetype ,*MIMETYPE*))
     ,(ods-body sheet))))

(define (ods-body sheet)
  `(office:body
    ,(ods-spreadsheet sheet)))

(define (ods-spreadsheet sheet)
  `(office:spreadsheet
    ,(ods-table sheet)))

;; ---------------------------------------------------------------------------------------------------
;; Array and list manipulation to turn an array into sxml
;; TODO: Information about named ranges is lost after this point

;; ods-table : sheet? -> sxml?
;; Create an office:table version of a sheet
(define (ods-table sheet)
  `(table:table
    (@ (table:name ,(or (sheet-name sheet) "Sheet 1")))
    (table:table-column) ; Why is this required?
    ,@(ods-rows (sheet-cells sheet)))) ; Splice in the rows

;; ods-rows : array2d? -> [List-of sxml?]
;; Helper function to map ods-row over the rows of an array, where the values of the array have
;; already been converted to sxml
(define (ods-rows cells)
  (let ([cells-array (ods-cells-array cells)])
    (map ods-row (array->list* cells-array))))

;; ods-cells-array : array2d? -> array2d?
;; Helper function to map ods-cell over the values of an array
(define (ods-cells-array cells)
  (let ([arr-shape (array-shape cells)])
    (array-map ods-cell
               cells
               (axis-index-array arr-shape 0)
               (axis-index-array arr-shape 1))))

;; ods-row : [List-of sxml?] -> [List-of sxml?]
;; Wrap the required ods boilerplate around each row
(define (ods-row cells)
  (cons 'table:table-row cells))


;; Helper function that returns null if the value is empty
(define (unless-empty-value key value)
  (if (null? value) null `(,key ,value)))


;; Helper function that removes nulls from a list
(define (without-nulls list) (filter-not null? list))

;; ---------------------------------------------------------------------------------------------------
;; The work of parsing cells 

;; ods-cell : cell? integer? integer? -> sxml?
;; TODO: Currently handles only:
;; - cell-value?
;; - cell-ref?
;; - cell-app?
(define (ods-cell c row col)
  (let ([expr (cell-content c)])
    (cond
      [(cell-value? expr) (ods-cell-value expr)]
      [(cell-ref? expr)   (ods-cell-ref expr)]
      [(cell-app? expr)   (ods-cell-app expr)]
      [else               #f])))


;; ---------------------------------------------------------------------------------------------------
;; Deal with values

;; ods-cell-value : cell-value? -> sxml?
;; Convert a cell-value
;; TODO: Currently handles only simple-cell-value?
(define (ods-cell-value v)
  (if (simple-cell-value? v)
    (table-table-cell (atomise v))
    #f))

(define (table-table-cell v)
  (if (nothing? v) 
    '(table:table-cell) 
    `(table:table-cell 
       (@ ,(office-value v)
          ,(office-value-type v))
       ,(office-text-p v ))))

(define (office-value v)
  (cond
    [(nothing? v) '()]
    [(string? v) `(office:string-value ,v)]
    [(number? v) `(office:value ,(number->string v))]
    [(boolean? v) `(office:boolean-value ,(if v "true" "false"))]))

(define (office-value-type v)
  (let ([t (cond
             [(nothing? v) '()]
             [(string? v) "string"]
             [(number? v) "float"]
             [(boolean? v) "boolean"])])
    (unless-empty-value 'office:value-type t)))

(define (office-text-p v)
  (let ([t (cond
              [(nothing? v) '()]
              [(string? v) v]
              [(number? v) (number->string v)]
              [(boolean? v) (if v "TRUE" "FALSE")])])
    (unless-empty-value 'text:p t)))

;; ---------------------------------------------------------------------------------------------------
;; Deal with formulae and references

;; ods-cell-app : cell-app? -> sxml?
(define (ods-cell-app x)
  `(table:table-cell
    (@ (table:formula ,(ods-cell-formula x))
       (office:value "333"))))

;; ods-cell-ref : cell-app? -> sxml?
;; Note that this is identical to ods-cell-app
(define (ods-cell-ref x)
  `(table:table-cell
    (@ (table:formula ,(ods-cell-formula x))
       (office:value "444"))))

;; ods-cell-formula : cell-expr? -> string?
(define (ods-cell-formula x)
  (string-append "of:=" (cell-expr->openformula x)))
