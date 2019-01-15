#lang racket
(require file/zip)
(require file/unzip)
(require racket/path)
(require racket/pretty)
(require sxml)

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
    (match (path-get-extension path)
      [#".xml" true]
      [#".rdf" true]
      [_ false]
      )
    )

  (define (accumulate-if-xml path accumulated)
    (if (is-xml-path? path) (cons path accumulated) accumulated)
    )
  
  (match file-type
    ['file (accumulate-if-xml path accumulated)]
    ['dir accumulated]
    ['link accumulated]
    )
  )

(define (write-raw raw path)
  (define (write-raw-to-port port)
    (pretty-print raw port)
  )
  (call-with-output-file path write-raw-to-port #:mode 'text #:exists 'replace)
  )

(define (read-raw path)
  (last (file->value path))
  )

(define (relative-path raw)
  (first raw)
  )

(define (raw-sxml raw)
  (last raw)
  )

(define (make-parent-directories path)
  (let-values ([(parent a b) (split-path path)])
    (match parent
      ['relative null]
      [#f null]
      [_ (if (directory-exists? parent) '() (make-directory parent))])
    )
  )

(define (raw->folder root raw)
  (let ([path (relative-path raw)]
        [sxml (raw-sxml raw)]
        [original-current-directory (current-directory)])
    (current-directory root)
    (make-parent-directories path)
    (call-with-output-file path (lambda (port) (srl:sxml->xml sxml port)) #:mode 'text #:exists 'replace)
(current-directory original-current-directory)
    path
    )
  
  )

(write-raw
 (call-with-unzip "example.ods" folder->raw)
 "example.raw"
 )

(let ([paths (map (lambda (raw) (raw->folder "out" raw)) (read-raw "example.raw"))])
  (current-directory "out")
  (apply zip "../out.ods" paths)
  )