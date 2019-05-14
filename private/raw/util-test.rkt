#lang racket/base

(module+ test
         (require rackunit "util.rkt")

         (test-case "Test path-extension-or-filename"
                    (check-equal? (path-extension-or-filename "world.ext") #".ext")
                    (check-equal? (path-extension-or-filename "world/.ext") #".ext")
                    (check-equal? (path-extension-or-filename "world/") #f)
                    )
         )
