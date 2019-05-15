#lang racket

(require 2htdp/image)

;; A barchart is, from left-to-right, a vertical axis, and one or more bars. Bars include their base
;; and label.

(define *BAR-EXPAND* 0.2) ;; how much space is left to each side of a bar, as a fraction of the bar width. 
(define *AXIS-EXPAND* 0.2)
(define *BAR-COLOUR* "DarkSlateGray")
(define *LINE-COLOUR* "LightSlateGray")
(define *BAR-WIDTH* 50)


(define (barchart #:label label . heights)
  (let ([max-height (foldl max 0 heights)]
        [min-height (abs (foldl min 0 heights))])
    (beside/align  "top"
                   (text label 14 *LINE-COLOUR*)
                   (rectangle 10 0 'solid "transparent")
                   (add-line 
                    (bars heights max-height min-height)
                    0 (- (* max-height *AXIS-EXPAND*))
                    0 (+ (* max-height (+ 1 *AXIS-EXPAND*)) (* min-height (+ 1 *AXIS-EXPAND*)))
                    *LINE-COLOUR*))))

(define (bar height label max-height max-depth)
  (let ([br (rectangle *BAR-WIDTH* (abs height)
                       'solid
                       *BAR-COLOUR*)]
        [ln (line (* *BAR-WIDTH* (+ 1 (* 2 *BAR-EXPAND*))) 0 *LINE-COLOUR*)]
        [max-br (rectangle *BAR-WIDTH* max-height 'solid "transparent")]
        [min-br (rectangle *BAR-WIDTH* max-depth 'solid "transparent")])
    (if (>= height 0)
        (above
         (overlay/align "middle" "bottom" br max-br)
         ln
         (underlay/align/offset "middle" "top"
                                min-br
                                0 5
                               (text label 12 *LINE-COLOUR*)))
        (above
         (underlay/align/offset "middle" "bottom"
                                max-br
                                0 -4
                                (text label 12 *LINE-COLOUR*))
         ln
         (overlay/align "middle" "top" br min-br)))))

(define (bars heights max-height min-height)
  (for/fold ([brs empty-image])
            ([br (map (λ (h i) (bar h (~a "Year " i) max-height min-height))
                      heights
                      (range (length heights)))])
    (beside brs br)))
