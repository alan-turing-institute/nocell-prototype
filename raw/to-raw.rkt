#lang racket

(provide 
  workbook->raw
  worksheet->raw
  )

(require sxml)
(require "to-zip.rkt")

(define (workbook->raw raw-worksheets)
  `(
    ("content.xml"
   *TOP*
   (*PI* xml "version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"")
   (urn:oasis:names:tc:opendocument:xmlns:office:1.0:document-content
     (@ (urn:oasis:names:tc:opendocument:xmlns:office:1.0:version "1.2"))
     (urn:oasis:names:tc:opendocument:xmlns:office:1.0:body
       (urn:oasis:names:tc:opendocument:xmlns:office:1.0:spreadsheet
         ,raw-worksheets
         ))))
    ("mimetype" . "application/vnd.oasis.opendocument.spreadsheet"))
  )

(define (worksheet->raw)
  '(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table
    (@ (urn:oasis:names:tc:opendocument:xmlns:table:1.0:name "Sheet1"))

    )
)

