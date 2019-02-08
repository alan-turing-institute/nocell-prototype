#lang racket

(provide 
 ; Convert an xlsx or an ods into a the 'raw' format. The raw format is a list of pairs, where the first argument of the pair is a file name,
 ; relative to the root of the directory. The second argument of the pair is either a list with the sxml representation of the file or a string
 ; with the content of the file if not xml.
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
  (define (relative-path-string path) (path->string (find-relative-path unzipped-root-path path)))
  (define (path->sxml path)
    (define (convert-xml-file port)
      (ssax:xml->sxml port '())
      )
    (call-with-input-file path convert-xml-file)
    )
  (define (path->pair path)
    (define (load-path path) (if (is-xml-path? path) (path->sxml path) (call-with-input-file path port->string) ))
    (cons (relative-path-string path) (load-path path))
    )
  (map path->pair (folder->file-paths unzipped-root-path))
  )

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


(define (folder->file-paths root-path) (filter-paths is-file? root-path))
(define (is-file? path file-type) (equal? file-type 'file))
(define (filter-paths predicate root-path)
  (define (accumulate-if-true path file-type accumulated)
    (if (predicate path file-type) (cons path accumulated) accumulated)
    )
  (fold-files accumulate-if-true (list) root-path)
  )

(define (raw->file raw path)
  (define (write-raw-to-port port)
    (pretty-print raw port)
    )
  (call-with-output-file path write-raw-to-port #:mode 'text #:exists 'replace)
  )
