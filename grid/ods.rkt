#lang racket/base

#| 
Convert a single `sheet` to a bytestream representation of an OpenDocument Spreadsheet document [odf,
Part 1, 2.2.4]. 

[odf] Open Document Format for Office Applications (OpenDocument) Version 1.2 

TODO and LIMITATIONS
 - Doesn't cope with named ranges

|#



;; ---------------------------------------------------------------------------------------------------

(require racket/contract
         math/array
         sxml
         "sheet.rkt"
         "openformula.rkt")

(provide (all-defined-out))

(define PLACEHOLDER-TABLE
  '(table:table
    (@ (table:name "JG-test-sheet"))
    (table:table-column)
    (table:table-row
     (table:table-cell
      (@ (office:value "42")
         (office:value-type "float"))
      (text:p 42)))))

(define *MIMETYPE* "application/vnd.oasis.opendocument.spreadsheet")

;; ---------------------------------------------------------------------------------------------------
;; Export sheet to a directory; zip the directory; and delete the original directory

;; sheet? path-string? -> void
;; `path` should not be the name of an existing directory.  
(define (sheet-write-ods sheet path)
   (call-with-output-file "content.xml"
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
;; Emit an office:table version of a sheet

;; TODO: Information about named ranges is lost after this point
(define (ods-table sheet)
  `(office:table
    (@ (table:name ,(or (sheet-name sheet) "Sheet 1")))
    (table:table-column) ; Why is this required?
    ,@(ods-rows (sheet-cells sheet)))) ; Splice in the rows

;; array2d? -> [List-of sxml?]
(define (ods-rows cells)
  (let ([cells-array (ods-cells-array cells)])
    (map ods-row (array->list* cells-array))))

;; array2d? -> array2d? 
(define (ods-cells-array cells)
  (let ([arr-shape (array-shape cells)])
    (array-map ods-cell
               cells
               (axis-index-array arr-shape 0)
               (axis-index-array arr-shape 1))))

;; [List-of sxml?] -> [List-of sxml?]
(define (ods-row cells)
  (cons 'office:table-row cells))

;; ods-cell : cell? integer? integer? -> sxml?
;; TODO: Only cell-value? handled
(define (ods-cell c row col)
  (let ([expr (cell-content c)])
    (cond
      [(cell-value? expr) (ods-cell-value (cell-value-elements expr))]
      [else #f])))

;; TODO: What happens if elems is not a simple-cell-value? ?
(define (ods-cell-value elems)
  (if (simple-cell-value? elems)
      (let ([val (atomise elems)])
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








