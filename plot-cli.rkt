#lang at-exp rscript

(require plot-util/quick/infer)

(main
 #:arguments {[flags args]
              #:once-each
              [("-f" "--file")
               'data-file
               "Read data from the given file instead of stdin."
               #:collect ["path" take-latest #f]]
              #:args [plot-type]}
 #:check [(match (hash-ref flags 'data-file)
            [(? string? path) (path-to-existant-file? path)]
            [else #t])
          @~a{File @(hash-ref flags 'data-file) not found.}]

 (define in-port (match (hash-ref flags 'data-file)
                   [(? string? path) (open-input-file path)]
                   [else (current-input-port)]))
 (define data (port->lines in-port))
 (close-input-port in-port)

 (define plot-type-identifier
   ;; Splitting allows "line graph" -> "line", etc
   (first (string-split (first args))))
 (define plotter
   (match plot-type-identifier
     [(or "line" "lines") plot-lines/infer]
     [(or "bar" "bars" "histogram" "hist") plot-bars/infer]
     [(or "scatter" "points") plot-points/infer]))
 (parameterize ([plot-new-window? #t])
   (plotter data)))
