#lang racket
(require racket/path)

(provide path-extension-or-filename)

;; path-get-extension returns #f unless there are bytes before the extension or if there is no extension. This variant returns the filename if it cannot return an extension, or #f if it cannot return either a filename or extension
;; path -> (or/c bytes #f)
(define (path-extension-or-filename path)
  (let ([extension (path-get-extension path)]
        [file-name (file-name-from-path path)])
    (if extension extension (if file-name (path->bytes file-name) #f))
    ))
