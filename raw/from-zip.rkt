#lang racket

(provide 
 ; Convert an xlsx or an ods into a the 'raw' format. The raw format is a list of pairs, where the first argument of the pair is a file name,
 ; relative to the root of the directory. The second argument of the pair is the sxml representation of the file.
 zip->raw
 ; pretty print a raw model to a file
 raw->file
 )

(require file/unzip)
(require racket/path)
(require racket/list)
(require racket/file)
(require sxml)

(define (zip->raw path) 
  (call-with-unzip path folder->raw)
  )

(define (folder->raw unzipped-root-path)
  (define (load-xml-file path)
    (define (convert-xml-file port)
      (ssax:xml->sxml port '())
      )
    (let ([relative-path (path->string (find-relative-path unzipped-root-path path))]
          [sxml (call-with-input-file path convert-xml-file)])
      (cons relative-path sxml)
      )
    )
  (map load-xml-file (fold-files list-of-xml-files (list) unzipped-root-path))

  )

(define (list-of-xml-files path file-type accumulated)
  (define (is-xml-path? path)
    ; We do this because path-get-extension returns #f unless there are digits before the extension
    (if (equal? (file-name-from-path path) (string->path ".rels")) true
        (match (path-get-extension path)
          [#".xml" true]
          [#".rdf" true]
          [#".rels" true]
          [_ false]
          )
        ))

  (define (accumulate-if-xml path accumulated)
    (if (is-xml-path? path) (cons path accumulated) accumulated)
    )
  
  (match file-type
    ['file (accumulate-if-xml path accumulated)]
    ['dir accumulated]
    ['link accumulated]
    )
  )

(define (raw->file raw path)
  (define (write-raw-to-port port)
    (pretty-print raw port)
    )
  (call-with-output-file path write-raw-to-port #:mode 'text #:exists 'replace)
  )
