#lang racket

(provide 
  ; Primitives
  workbook->raw
  worksheet->raw
  row->raw
  string->raw
  ; Compounds
  list->raw-row
  grid->raw-worksheet
  grids->raw-workbook
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
        ,@raw-worksheets
         ))))
    ("mimetype" . "application/vnd.oasis.opendocument.spreadsheet"))
  )

(define (worksheet->raw name raw-rows)
  (if (empty? raw-rows)
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table
         (@ (urn:oasis:names:tc:opendocument:xmlns:table:1.0:name ,name))
         )
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table
         (@ (urn:oasis:names:tc:opendocument:xmlns:table:1.0:name ,name))
         ,@raw-rows
         )
      )
)

(define (row->raw raw-cells)
  (if (empty? raw-cells)
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-row)
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-row ,@raw-cells)
      )
)

(define (string->raw s)
`(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-cell
   (@
     (urn:oasis:names:tc:opendocument:xmlns:office:1.0:value-type "string")
     (urn:oasis:names:tc:opendocument:xmlns:office:1.0:value ,s))
   (urn:oasis:names:tc:opendocument:xmlns:text:1.0:p ,s)
   )
  ) 

; Compounds 

(define (list->raw-row lst)
  (row->raw (map string->raw lst))
  )

(define (grid->raw-worksheet name grid)
  (worksheet->raw name (map list->raw-row grid))
  )

(define (grids->raw-workbook grids)
  (workbook->raw (map (lambda (gridargs) (apply grid->raw-worksheet gridargs)) grids))
  )
