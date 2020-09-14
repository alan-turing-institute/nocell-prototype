#lang racket

(provide 
  ; Primitives
  workbook->raw
  worksheet->raw
  row->raw
  cell->raw
  ; Compounds
  list->raw-row
  grid->raw-worksheet
  grids->raw-workbook
  )

(require sxml)
(require "to-zip.rkt")

(define (workbook->raw raw-worksheets [cell-styles '()])
  `(
    ("mimetype" . "application/vnd.oasis.opendocument.spreadsheet")
    ("content.xml"
     *TOP*
     (*PI* xml "version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"")
     (urn:oasis:names:tc:opendocument:xmlns:office:1.0:document-content
       (@ (urn:oasis:names:tc:opendocument:xmlns:office:1.0:version "1.2"))
       (urn:oasis:names:tc:opendocument:xmlns:office:1.0:automatic-styles

         (urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0:number-style
           (@ (urn:oasis:names:tc:opendocument:xmlns:style:1.0:name "NoDecimalPlaces"))
           (urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0:number 
             (@ (urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0:decimal-places "0"))))

         (urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0:number-style
           (@ (urn:oasis:names:tc:opendocument:xmlns:style:1.0:name "TwoDecimalPlaces"))
           (urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0:number 
             (@ (urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0:decimal-places "2"))))

         (urn:oasis:names:tc:opendocument:xmlns:style:1.0:style
           (@ 
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:name "NoCellRowDefault")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:family "table-row")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:parent-style-name "Default")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:data-style-name "N0")
             )
           (urn:oasis:names:tc:opendocument:xmlns:style:1.0:table-row-properties
             (@ (urn:oasis:names:tc:opendocument:xmlns:style:1.0:row-height "18pt")))
           )
         (urn:oasis:names:tc:opendocument:xmlns:style:1.0:style
           (@ 
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:name "NoCellCellDefault")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:family "table-cell")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:parent-style-name "Default")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:data-style-name "NoDecimalPlaces")
             )
           (urn:oasis:names:tc:opendocument:xmlns:style:1.0:table-cell-properties
             (@ (urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0:background-color "#FFFFFF")))
           )
         ,(make-style '("NoCellNonIntegerCellDefault" "TwoDecimalPlaces" () ()))
         ,@(make-styles cell-styles)
         (urn:oasis:names:tc:opendocument:xmlns:style:1.0:style
           (@ 
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:name "NoCellPaddingColumn")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:family "table-column")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:parent-style-name "Default")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:data-style-name "N0")
             )
           (urn:oasis:names:tc:opendocument:xmlns:style:1.0:table-column-properties
             (@ (urn:oasis:names:tc:opendocument:xmlns:style:1.0:column-width "18pt")))
           )
         (urn:oasis:names:tc:opendocument:xmlns:style:1.0:style
           (@ 
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:name "NoCellLabelColumn")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:family "table-column")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:parent-style-name "Default")
             (urn:oasis:names:tc:opendocument:xmlns:style:1.0:data-style-name "N0")
             )
           (urn:oasis:names:tc:opendocument:xmlns:style:1.0:table-column-properties
             (@ (urn:oasis:names:tc:opendocument:xmlns:style:1.0:column-width "115pt")))
           ))
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
       ))))

(define (make-styles style-definitions)
  (map (lambda (style) (make-style style)) style-definitions))

(define (make-style style-definition)
  `(urn:oasis:names:tc:opendocument:xmlns:style:1.0:style
     (@ 
       (urn:oasis:names:tc:opendocument:xmlns:style:1.0:name ,(style-name style-definition))
       (urn:oasis:names:tc:opendocument:xmlns:style:1.0:family "table-cell")
       (urn:oasis:names:tc:opendocument:xmlns:style:1.0:parent-style-name "NoCellCellDefault")
       ,(attribute-unless-empty 'urn:oasis:names:tc:opendocument:xmlns:style:1.0:data-style-name (style-data-name style-definition))
       )
     (urn:oasis:names:tc:opendocument:xmlns:style:1.0:table-cell-properties
       (@ ,@(style-properties style-definition)))
     
     (urn:oasis:names:tc:opendocument:xmlns:style:1.0:text-properties
       (@ ,@(style-text-properties style-definition)))
  ))

(define (style-name style-definition) (list-ref style-definition 0))
(define (style-data-name style-definition) (list-ref style-definition 1))
(define (style-properties style-definition) (map (lambda (property) (style-property property)) (list-ref style-definition 2)))
(define (style-text-properties style-definition) (map (lambda (property) (style-text-property property)) (list-ref style-definition 3)))
(define (style-property property-definition) 
  (match property-definition
    [(list 'background-color v) `(urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0:background-color ,v)]
    [(list 'border-bottom v) `(urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0:border-bottom ,v)]))
(define (style-text-property property-definition) 
  (match property-definition
    [(list 'font-weight v) `(urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0:font-weight ,v)]
    [(list 'font-color v) `(urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0:color ,v)]
    [(list 'font-style v) `(urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0:font-style ,v)]))

(define (worksheet->raw name raw-rows)
  (if (empty? raw-rows)
    `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table
       (@ (urn:oasis:names:tc:opendocument:xmlns:table:1.0:name ,name))
       ,(filler-row)
       )
    `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table
       (@ (urn:oasis:names:tc:opendocument:xmlns:table:1.0:name ,name))
       (urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-column
         (@ 
           (urn:oasis:names:tc:opendocument:xmlns:table:1.0:style-name "NoCellPaddingColumn")
           (urn:oasis:names:tc:opendocument:xmlns:table:1.0:default-cell-style-name "NoCellCellDefault")
           )
         )
       (urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-column
         (@ 
           (urn:oasis:names:tc:opendocument:xmlns:table:1.0:style-name "NoCellLabelColumn")
           (urn:oasis:names:tc:opendocument:xmlns:table:1.0:default-cell-style-name "NoCellCellDefault")
           )
         )
       (urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-column
         (@ 
           (urn:oasis:names:tc:opendocument:xmlns:table:1.0:style-name "NoCellColumnDefault")
           (urn:oasis:names:tc:opendocument:xmlns:table:1.0:number-columns-repeated "256")
           (urn:oasis:names:tc:opendocument:xmlns:table:1.0:default-cell-style-name "NoCellCellDefault")
           )
         )
       ,(empty-row)
       ,@raw-rows
       ,(filler-row)
       )
    ))

(define (filler-cell) `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-cell (@(urn:oasis:names:tc:opendocument:xmlns:table:1.0:number-columns-repeated "256") )))
(define (filler-row) `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-row (@ (urn:oasis:names:tc:opendocument:xmlns:table:1.0:number-rows-repeated "256")) ,(filler-cell)))
(define (empty-cell) `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-cell))
(define (empty-row) (row->raw '() "NoCellRowDefault"))

(define (row->raw raw-cells style)
  `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-row 
     (@ ,(attribute-unless-empty 'urn:oasis:names:tc:opendocument:xmlns:table:1.0:style-name style)) 
     ,(empty-cell) 
     ,@raw-cells 
     ,(filler-cell))
  )

(define (expression->raw e) (cell->raw (expression-value e) (expression-formula e) (expression-style e)))

(define (expression-value expression)
  (match expression 
    [(list value formula) value]
    [(list value formula style) value]
    [_ expression]))

(define (expression-formula expression)
  (match expression 
    [(list value formula) formula]
    [(list value formula style) formula]
    [_ '()]))

(define (expression-style expression)
  (match expression 
    [(list value formula style) style]
    [_ '()]))

(define (cell->raw value formula style)
  `(urn:oasis:names:tc:opendocument:xmlns:table:1.0:table-cell
     (@
       ,(value-type-attribute value)
       ,(value-attribute value)
       ,(style-attribute style)
       ,(formula-attribute formula))
     ,(raw-text value)
     )) 

(define (attribute-unless-empty attribute value)
  (if (empty? value)
    `()
    `(,attribute ,value)
    ))

(define (value-type-attribute original) 
  (let ((value-type (match original
                      ([? string?] "string")
                      ([? number?] "float")
                      ([? boolean?] "boolean")
                      )))
    `(urn:oasis:names:tc:opendocument:xmlns:office:1.0:value-type ,value-type)))

(define (value-attribute original)
  (match original
    ([? number?] `(urn:oasis:names:tc:opendocument:xmlns:office:1.0:value ,(number->string original)))
    ([? string?] `(urn:oasis:names:tc:opendocument:xmlns:office:1.0:string-value ,original))
    (#t `(urn:oasis:names:tc:opendocument:xmlns:office:1.0:boolean-value "true"))
    (#f `(urn:oasis:names:tc:opendocument:xmlns:office:1.0:boolean-value "false"))
    ))

(define (style-attribute style) (attribute-unless-empty 'urn:oasis:names:tc:opendocument:xmlns:table:1.0:style-name style))

(define (formula-attribute formula) (attribute-unless-empty 'urn:oasis:names:tc:opendocument:xmlns:table:1.0:formula formula))

(define (raw-text original) 
  (let ((text (match original
                ([? string?] original)
                ([? number?] (number->string original))
                (#t "TRUE")
                (#f "FALSE")
                )))
    `(urn:oasis:names:tc:opendocument:xmlns:text:1.0:p ,text)))

; Compounds 

(define (list->raw-row lst)
  (row->raw (map expression->raw lst) "NoCellRowDefault")
  )

(define (apply-row-style cells style)
  (map (lambda (cell)
         (match cell
           [(list v f) `(,v ,f ,style)]
           [_ `(,cell () ,style)])
         ) cells)
  )

(define (grid-row->raw-row lst)
  (row->raw (map expression->raw (apply-row-style (list-ref lst 1) (list-ref lst 0))) "NoCellRowDefault")
  )

(define (grid->raw-worksheet name grid)
  (worksheet->raw name (map grid-row->raw-row grid))
  )

(define (grids->raw-workbook grids)
  (workbook->raw (map (lambda (gridargs) (apply grid->raw-worksheet gridargs)) grids))
  )
