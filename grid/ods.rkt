#lang racket/base

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
      (let ([val (atomise v)])
        (cond
          [(nothing? val) (ods-cell-empty)]
          [(real? val)    (ods-cell-real val)]
          [(string? val)  (ods-cell-string val)]
          [(boolean? val) (ods-cell-boolean val)]))
      #f))

(define (ods-cell-empty)
  '(table:table-cell))

(define (ods-cell-real v)
  `(table:table-cell
    (@ (office:value ,(number->string v))
       (office:value-type "float"))
    (text:p ,v)))

(define (ods-cell-string v)
  `(table:table-cell
    (@  (office:value-type "string"))
    (text:p ,v)))

(define (ods-cell-boolean v)
  `(table:table-cell
    (@ (office:boolean-value ,(if v "true" "false"))
       (office:value-type "boolean"))
    (text:p ,(if v "TRUE" "FALSE"))))

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
