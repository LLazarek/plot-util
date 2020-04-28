#lang at-exp racket

(provide plot-lines/infer
         plot-bars/infer
         plot-points/infer
         plot-new-window?)

(require "inferrers.rkt"
         "../histogram.rkt"
         plot)

(define-logger plot-util-inference)

(define/contract inferrers
  (listof inferrer/c)

  (list infer-epoch-timestamps
        infer-number-pairs
        infer-name-value-pairs
        infer-single-values))

;; Takes the first line of data and tries to infer the format of the data using
;; `inferrers`, taking the first one that succeeds.
(define (infer-data-format data-lines)
  (define (try-inferrer inferrer)
    (log-plot-util-inference-info
     @~a{Trying inferrer @(object-name inferrer)})
    (inferrer data-lines))
  (define first-inferred
    (for*/first ([inferrer (in-list inferrers)]
                 [result (in-value (try-inferrer inferrer))]
                 #:when result)
      (log-plot-util-inference-info
       @~a{Selected inferrer @(object-name inferrer)})
      (log-plot-util-inference-debug
       @~a{
           Parsed data:
           @pretty-format[(inferred-data result) #:mode 'write]
           })
      result))
  (match first-inferred
    [(inferred ticks data)
     (values ticks data)]
    [#f
     (raise-user-error 'infer-data-format
                       "Unable to infer data format from input")]))

(define (read/split-data something)
  (define blank-line? (match-lambda [(regexp @pregexp{^\s*$}) #t]
                                    [else #f]))
  (filter-not blank-line?
              (match something
                [(? path-string? path)
                 #:when (file-exists? path)
                 (file->lines path)]
                [(? string? data-str)
                 (string-split data-str "\n")]
                [(? list? data)
                 data]
                [(? hash? h)
                 (hash->list h)])))

(define (do-inferred-plot do-plot some-kind-of-data)
  (define raw-data (read/split-data some-kind-of-data))
  (define-values {x-ticks data}
    (infer-data-format raw-data))
  (parameterize ([plot-x-ticks x-ticks])
    (do-plot data)))

(define (simple-inferred-plotter renderer
                                 #:plot [plot plot])
  (make-keyword-procedure
   (λ (kws kw-args . args)
     (do-inferred-plot
      (λ (data) (keyword-apply plot
                               kws
                               kw-args
                               (list (renderer data))))
      (first args)))))

(define plot-lines/infer (simple-inferred-plotter lines))
(define plot-points/infer (simple-inferred-plotter points))
(define plot-bars/infer (simple-inferred-plotter discrete-histogram
                                                 #:plot plot/labels-angled))

