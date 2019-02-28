#lang racket/base

#| 
Convert a single `sheet` to a bytestream representation of an OpenDocument Spreadsheet document [odf,
Part 1, 2.2.4]. 

[odf] Open Document Format for Office Applications (OpenDocument) Version 1.2 
|#

;; ---------------------------------------------------------------------------------------------------

(require racket/contract
         math/array
         sxml
         file/zip
         "sheet.rkt"
         "openformula.rkt")

(define PLACEHOLDER-TABLE
  '(table:table
    (@ (table:name "JG-test-sheet"))
    (table:table-column)
    (table:table-row
     (table:table-cell
      (@ (office:value= "42"))
      (text:p 42)))))

;; provides:
;; - sheet-odf : sheet -> xml?
;; - ods-sheet : sheet -> bytestream


#| 
An OpenDocument package is a zip file containing at least the files;
- mimetype
- content.xml
|#







;; ---------------------------------------------------------------------------------------------------
;; Wrap XML boilerplate for content.xml documents around the exported sheet 

;; sheet? -> sxml?
(define (content sheet)
  `(*TOP*
    (@ (*NAMESPACES*
        [office "urn:oasis:names:tc:opendocument:xmlns:office:1.0"]
        [table  "urn:oasis:names:tc:opendocument:xmlns:table:1.0"]
        [text   "urn:oasis:names:tc:opendocument:xmlns:text:1.0"]))
    (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
    ,(document-content sheet)))

(define (document-content sheet)
  `(office:document-content
    (@ (office:version "1.2"))
    ,(body sheet)))

(define (body sheet)
  `(office:body
    ,(spreadsheet sheet)))

(define (spreadsheet sheet)
  `(office:spreadsheet ,PLACEHOLDER-TABLE))



(define FILE-MIMETYPE '("mimetype" . "application/vnd.oasis.opendocument.spreadsheet"))

