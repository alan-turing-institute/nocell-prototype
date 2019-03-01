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

(define MIMETYPE "application/vnd.oasis.opendocument.spreadsheet")




;; ---------------------------------------------------------------------------------------------------
;; Export sheet to a directory; zip the directory; and delete the original directory

;; sheet? path? path-string? -> Void
;; `path` should not be the name of an existing directory.  
(define (sheet-export-ods sheet path filename)
  (define package-directory (build-path path filename))
  (when (directory-exists? package-directory)
    (raise-user-error "Filename of output file (without extension) must not be the name of an existing directory"
                      (path->string path)))
  (make-directory package-directory)
  (parameterize ([current-directory package-directory])
    (write-ods-package sheet))
  (zip-ods-package path filename))

;; Write out the contents of the package to the current directory
(define (write-ods-package sheet)
  (call-with-output-file "content.xml"
    (λ (out) (srl:sxml->xml (content sheet) out)))
  (call-with-output-file "mimetype"
    (λ (out) (write-string MIMETYPE out))))

(define (zip-ods-package path filename)
  (println "Done!"))


;; ---------------------------------------------------------------------------------------------------
;; Wrap XML boilerplate for content.xml file around the exported sheet 

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



