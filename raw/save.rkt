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

(define (file->raw path)
  (last (file->value path))
  )

(define (raw->zip raw path)
  ; The first part of a raw-entry pair is the path
  (define (raw-entry-relative-path raw-entry)
    (first raw-entry)
    )

  ; The second part of a raw-entry pair is the sxml
  (define (raw-entry-sxml raw-entry)
    (last raw-entry)
    )

  ; Writes one raw entry into the folder
  (define (write-raw-entry-to-folder folder raw-entry)
    (let ([path (raw-entry-relative-path raw-entry)]
          [sxml (raw-entry-sxml raw-entry)]
          [original-current-directory (current-directory)])
      (make-directory* folder)
      (current-directory folder)
      (make-parent-directory* path)
      (call-with-output-file path (lambda (port) (srl:sxml->xml sxml port)) #:mode 'text #:exists 'replace)
      (current-directory original-current-directory)
      path
      )
    )
  ; I get a permission failure trying to write to a temporary folder ?
  ; (let* ([temporary-folder (make-temporary-file "rawsave-~a" 'directory)]
  (let* ([temporary-folder "./out/"]
         [paths (map (lambda (raw-entry) (write-raw-entry-to-folder temporary-folder raw-entry)) raw)])
  (if (file-exists? path) (delete-file path) '())
  (current-directory temporary-folder)
  (apply zip path paths)
  (current-directory "..")
  (delete-directory/files temporary-folder)
  )
)




