#lang at-exp rscript

(require racket/gui
         plot
         plot-util/quick/infer)

(define ((plot-command! period) command)
  (define wh 500)
  (define frame (new frame% [label command] [width wh] [height wh]))
  (define data '("0" "0"))
  (define canvas
    (new canvas%
         [parent frame]))
  (send frame show #t)

  (thread
   (thunk
    (let loop ()
      (sleep period)
      (send canvas
            refresh-now
            (λ (dc)
              (define now-idle-time (string-trim (system/string command)))
              (define new-data
                (cons now-idle-time data))
              (set! data
                    (drop-right new-data
                                (if (>= (length new-data) 50) 1 0)))
              (define plot-dc
                (simple-inferred-plotter lines
                                         #:plot (λ (data)
                                                  (plot/dc ))))
              ((simple-inferred-plotter
                lines
                #:plot (λ (render-tree)
                         (plot/dc render-tree
                                  dc
                                  0
                                  0
                                  wh
                                  wh)))
               (reverse data))))
      (loop))))
  (void))

(main
 #:arguments {[(hash-table ['data-file data-file-path]
                           ['data-cmd data-cmd]
                           ['data-cmd-period (app string->number data-cmd-period)])
               args]
              #:once-each
              [("-f" "--file")
               'data-file
               "Read data from the given file instead of stdin."
               #:collect ["path" take-latest #f]]

              [("-c" "--cmd-over-time")
               'data-cmd
               "Plot the output of the given shell command over time."
               #:collect ["command" take-latest #f]
               #:conflicts '(data-file)]
              [("-p" "--period")
               'data-cmd-period
               ("Set the period, in seconds, for running the command."
                "Only has an effect when `-c` is supplied."
                "Default: 1")
               #:collect {"seconds" take-latest "1"}]
              #:args [plot-type]}
 #:check [(match data-file-path
            [(? string? path) (path-to-existant-file? path)]
            [else #t])
          @~a{File @data-file-path not found.}]
 #:check [(and (real? data-cmd-period) (positive? data-cmd-period))
          @~a{Period must be a positive real number.}]

 (cond [data-cmd
        => (plot-command! data-cmd-period)]
       [else
        (define in-port (match data-file-path
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
          (plotter data))]))
