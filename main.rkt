#lang racket/base

(require racket/sequence
         plot)

(provide labeled-points)

;; like `points`, but all of them labeled
(define (labeled-points the-points
                        [point->label (λ (p) (sequence-ref p 0))]
                        #:anchor [anchor 'auto]
                        #:angle [angle (label-angle)])
  (for/list ([p (in-list the-points)])
    (point-label p
                 (point->label p)
                 #:anchor anchor
                 #:angle angle)))
