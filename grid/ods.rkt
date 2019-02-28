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

;; provides:
;; - sheet-odf : sheet -> xml?
;; - ods-sheet : sheet -> bytestream


#| 
An OpenDocument package is a zip file containing at least the files;
- mimetype
- content.xml
|#

(define FILE-MIMETYPE '("mimetype" . "application/vnd.oasis.opendocument.spreadsheet"))

(define CONTENT-DECLARATION "version=\"1.0\" encoding=\"UTF-8\"")
