#lang racket/base

(require (prefix-in gregor: gregor)
         mrlib/snip-canvas
         plot
         plot/utils
         racket/class
         racket/contract
         racket/gui/base
         racket/match
         racket/stream)

(provide
 (contract-out
  [plot/timeline/interactive ({(treeof renderer2d?)}
                              {#:title string?
                               #:y-label string?
                               #:x-label string?
                               #:x-ticks ticks?
                               #:y-min real?
                               #:y-max real?}
                              . ->* .
                              void?)]
  [gregor->plot-date/time ((or/c gregor:datetime-provider?
                                 gregor:date-provider?
                                 gregor:time-provider?)
                           . -> .
                           real?)]
  [convert-dates ((listof
                   (list/c (and/c gregor:date-provider?
                                  gregor:time-provider?)
                           any/c))
                  . -> .
                  (listof (list/c real? any/c)))])
 ;; same as plot, but sets x-ticks to dates
 plot/timeline)

(define (gregor:datetime->plot:datetime dt)
  (datetime->real (date (gregor:->seconds dt)
                        (gregor:->minutes dt)
                        (gregor:->hours dt)
                        (gregor:->day dt)
                        (gregor:->month dt)
                        (gregor:->year dt)
                        0 0 #f 0)))
(define (gregor:time->plot:datetime dt)
  (datetime->real (date (gregor:->seconds dt)
                        (gregor:->minutes dt)
                        (gregor:->hours dt)
                        1 1 0
                        0 0 #f 0)))
(define (gregor:date->plot:datetime dt)
  (datetime->real (date 0 0 0
                        (gregor:->day dt)
                        (gregor:->month dt)
                        (gregor:->year dt)
                        0 0 #f 0)))

(define (gregor->plot-date/time gregor-thing)
  (define date-provider? (gregor:date-provider? gregor-thing))
  (define time-provider? (gregor:time-provider? gregor-thing))
  (cond [(and date-provider? time-provider?)
         (gregor:datetime->plot:datetime gregor-thing)]
        [date-provider?
         (gregor:date->plot:datetime gregor-thing)]
        [time-provider?
         (gregor:time->plot:datetime gregor-thing)]))

(define (plot/timeline/interactive renderer-tree
                                   #:render [renderer lines]
                                   #:title [title "dummy-title"]
                                   #:y-label [y-label "y-label"]
                                   #:x-label [x-label "x-label"]
                                   #:x-ticks [x-ticks (date-ticks)]
                                   #:y-min [y-min #f]
                                   #:y-max [y-max #f])
  (parameterize ([plot-x-ticks x-ticks])
    (define snip (plot-snip renderer-tree
                            #:title title
                            #:y-label y-label
                            #:x-label x-label
                            #:y-min y-min
                            #:y-max y-max))
    ;; lltodo: would be nice if this snapped to the splot like with the fancy
    ;; sin-wave example; this isn't easy, however, because it would have to take
    ;; in the data as well as the renderer tree and from that find the "nearest"
    ;; point to obtain a y-value.
    (define (mouse-callback snip event x y)
      (if (and x y)
          (send snip set-overlay-renderers
                (list (point-label (vector x y)
                                   #:anchor 'auto)))
          (send snip set-overlay-renderers #f)))
    (send snip set-mouse-event-callback mouse-callback)

    (define frame (new frame%
                       [label "plot"]
                       [width 100]
                       [height 100]))
    (new snip-canvas%
         [parent frame]
         [make-snip (λ x snip)])
    (send frame show #t)))

(define (convert-dates data)
  (for/list ([point (in-list data)])
    (match-define (list time v) point)
    (list (gregor:datetime->plot:datetime time) v)))

(define plot/timeline (make-keyword-procedure
                       (λ (kws kw-args . rest)
                         (parameterize ([plot-x-ticks (date-ticks)])
                           (keyword-apply plot kws kw-args rest)))
                       (λ args
                         (parameterize ([plot-x-ticks (date-ticks)])
                           (apply plot args)))))
