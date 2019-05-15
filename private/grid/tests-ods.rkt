#lang racket/base

(require rackunit
         "ods.rkt"
         "grid.rkt"
         (prefix-in s: "sheet.rkt"))

(define asheet
  (sheet
   (row 10 20)
   (row 30 40)))


(check-equal?
 (ods asheet)
 '(*TOP*
   (@
    (*NAMESPACES*
     (office "urn:oasis:names:tc:opendocument:xmlns:office:1.0")
     (table "urn:oasis:names:tc:opendocument:xmlns:table:1.0")
     (text "urn:oasis:names:tc:opendocument:xmlns:text:1.0")))
   (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
   (office:document
    (@
     (office:version "1.2")
     (office:mimetype "application/vnd.oasis.opendocument.spreadsheet"))
    (office:body
     (office:spreadsheet
      (table:table
       (@ (table:name "unnamed-sheet"))
       (table:table-column)
       (table:table-row
        (table:table-cell
         (@ (office:value "10") (office:value-type "float"))
         (text:p "10"))
        (table:table-cell
         (@ (office:value "20") (office:value-type "float"))
         (text:p "20")))
       (table:table-row
        (table:table-cell
         (@ (office:value "30") (office:value-type "float"))
         (text:p "30"))
        (table:table-cell
         (@ (office:value "40") (office:value-type "float"))
         (text:p "40")))))))))

