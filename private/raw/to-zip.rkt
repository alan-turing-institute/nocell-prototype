#lang racket

(provide 
  ; Convert a 'raw' into an ods or xlsx file. See raw/load.rkt for the definition of raw
  raw->zip
  ; Load a 'raw' file from a path
  file->raw
  )

(require file/zip)
(require racket/path)
(require sxml)

(define (file->raw path) (last (file->value path)))

(define (raw->zip raw path)
  ; The first part of a raw-entry pair is the path
  (define (raw-entry-relative-path raw-entry) (car raw-entry))

  ; The second part of a raw-entry pair is the content, which currently might be a string or a sxml
  (define (raw-entry-content raw-entry) (cdr raw-entry))

  ; We can check what type of thing it is
  (define (raw-entry-is-sxml? raw-entry) (list? (raw-entry-content raw-entry)))
  (define (raw-entry-is-string? raw-entry) (string? (raw-entry-content raw-entry)))

  ; Writes one raw entry into the folder
  (define (write-raw-entry-to-folder raw-entry folder)
    (let ([path (raw-entry-relative-path raw-entry)]
          [original-current-directory (current-directory)])
      (make-directory* folder)
      (current-directory folder)
      (make-parent-directory* path)
      (write-raw-entry-to-path raw-entry path)
      (current-directory original-current-directory)
      path
      )
    )

  (define (write-raw-entry-to-path raw-entry path) 
    (call-with-output-file path (lambda (port) (write-raw-entry-to-port raw-entry port)) #:mode 'text #:exists 'replace)
    )

  (define (write-raw-entry-to-port raw-entry port)
    (let ([content (raw-entry-content raw-entry)])
      (if (raw-entry-is-sxml? raw-entry) (srl:sxml->xml content port) (display content port)))
    )

  (define (write-zip zip-path paths-to-zip)
    ; (apply zip zip-path paths-to-zip)
    (system (string-join (flatten (list "zip -q -0 -X" (path->string zip-path) "mimetype"))))
    (system (string-join (flatten (list "zip -q -X -x 'mimetype' -r" (path->string zip-path) paths-to-zip))))
    )

  ; I get a permission failure trying to write to a temporary folder ?
  ; (let* ([temporary-folder (make-temporary-file "rawsave-~a" 'directory)]
  (let* ([temporary-folder "./out/"]
         [paths (map (lambda (raw-entry) (write-raw-entry-to-folder raw-entry temporary-folder)) raw)])
  (if (file-exists? path) (delete-file path) '())
  (current-directory temporary-folder)
  (write-zip path paths)
  (current-directory "..")
  (delete-directory/files temporary-folder)
  )
)




