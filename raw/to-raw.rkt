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
    ("mimetype" . "application/vnd.oasis.opendocument.spreadsheet")
    ("content.xml"
   *TOP*
   (*PI* xml "version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"")
   (urn:oasis:names:tc:opendocument:xmlns:office:1.0:document-content
     (@ (urn:oasis:names:tc:opendocument:xmlns:office:1.0:version "1.2"))
     (urn:oasis:names:tc:opendocument:xmlns:office:1.0:body
       (urn:oasis:names:tc:opendocument:xmlns:office:1.0:spreadsheet
        ,@raw-worksheets
         ))))
    ("META-INF/manifest.xml"
      *TOP*
      (*PI* xml "version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"")
      (urn:oasis:names:tc:opendocument:xmlns:manifest:1.0:manifest
        (@ (urn:oasis:names:tc:opendocument:xmlns:manifest:1.0:version "1.2"))
        (urn:oasis:names:tc:opendocument:xmlns:manifest:1.0:file-entry
        (@
          (urn:oasis:names:tc:opendocument:xmlns:manifest:1.0:media-type
          "application/vnd.oasis.opendocument.spreadsheet")
          (urn:oasis:names:tc:opendocument:xmlns:manifest:1.0:full-path "/")))
        (urn:oasis:names:tc:opendocument:xmlns:manifest:1.0:file-entry
        (@
          (urn:oasis:names:tc:opendocument:xmlns:manifest:1.0:media-type
          "text/xml")
          (urn:oasis:names:tc:opendocument:xmlns:manifest:1.0:full-path
          "content.xml")))
        ))
    )
  )

(define (worksheet->raw name raw-rows)
  (if (empty? raw-rows)
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table
         (@ (urn:oasis:names:tc:opendocument:xmlns:table:1.0:name ,name))
         )
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table
         (@ (urn:oasis:names:tc:opendocument:xmlns:table:1.0:name ,name))
         (urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-column
           (@ 
             (urn:oasis:names:tc:opendocument:xmlns:table:1.0:style-name "col1")
             (urn:oasis:names:tc:opendocument:xmlns:table:1.0:number-columns-repeated "256")
             (urn:oasis:names:tc:opendocument:xmlns:table:1.0:default-cell-style-name "ce2")
              )
           )
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

(define (expression->raw e) (if (pair? e) (value->raw (car e) (cdr e)) (value->raw e '())))

(define (value->raw e f) (match e
                                ([? string?] (string->raw e f))
                                ([? number?] (number->raw e f))
                                (#t (true->raw f))
                                (#f (false->raw f))
                                )
  )

(define (string->raw s f) (raw-string-cell "string" s s f)) 
(define (number->raw n f) (raw-value-cell "float" (number->string n) (number->string n) f))
(define (true->raw f) (raw-boolean-cell "boolean" "true" "TRUE" f))
(define (false->raw f) (raw-boolean-cell "boolean" "false" "FALSE" f))
  
(define (raw-value-cell type value text formula)
  (if (empty? formula) 
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-cell
         (@
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:value-type ,type)
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:value ,value))
         (urn:oasis:names:tc:opendocument:xmlns:text:1.0:p ,text)
         )
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-cell
         (@
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:value-type ,type)
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:value ,value)
           (urn:oasis:names:tc:opendocument:xmlns:table:1.0:formula ,formula))
         (urn:oasis:names:tc:opendocument:xmlns:text:1.0:p ,text)
         )
        )
  ) 

; Argh DRY this up
(define (raw-boolean-cell type value text formula)
  (if (empty? formula) 
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-cell
         (@
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:value-type ,type)
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:boolean-value ,value))
         (urn:oasis:names:tc:opendocument:xmlns:text:1.0:p ,text)
         )
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-cell
         (@
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:value-type ,type)
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:boolean-value ,value)
           (urn:oasis:names:tc:opendocument:xmlns:table:1.0:formula ,formula))
         (urn:oasis:names:tc:opendocument:xmlns:text:1.0:p ,text)
         )
        )
  ) 

; Argh DRY this up
(define (raw-string-cell type value text formula)
  (if (empty? formula) 
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-cell
         (@
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:value-type ,type)
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:string-value ,value))
         (urn:oasis:names:tc:opendocument:xmlns:text:1.0:p ,text)
         )
      `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-cell
         (@
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:value-type ,type)
           (urn:oasis:names:tc:opendocument:xmlns:office:1.0:string-value ,value)
           (urn:oasis:names:tc:opendocument:xmlns:table:1.0:formula ,formula))
         (urn:oasis:names:tc:opendocument:xmlns:text:1.0:p ,text)
         )
        )
  ) 

; Compounds 

(define (list->raw-row lst)
  (row->raw (map expression->raw lst))
  )

(define (grid->raw-worksheet name grid)
  (worksheet->raw name (map list->raw-row grid))
  )

(define (grids->raw-workbook grids)
  (workbook->raw (map (lambda (gridargs) (apply grid->raw-worksheet gridargs)) grids))
  )
