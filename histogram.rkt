#lang racket/base

(provide group-name?
         category-name?
         category-data?
         grouped-category-data?

         plain-histogram-with-labels
         grouped-category-stacked-histogram
         histogram-stack-with-labels

         make-angled-label-plotter
         plot/labels-angled
         discrete-histogram/colors
         two-sided-histogram)

(require plot
         plot/utils
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/format
         racket/list
         racket/match
         racket/math)

;; ==================== Stacked histogram illustration ====================
;; Data:
;; '((group-A ((category-a 4)
;;             (category-b 7)
;;             (category-c 3)))
;;   (group-B ((category-d 6)
;;             (category-e 3))))
;;
;; Plot:
;; |            14
;; |   +--------------------+
;; |   |                    |
;; |   |    category-c      |
;; |   |--------------------|
;; |   |                    |                 9
;; |   |                    |        +-------------------+
;; |   |    category-b      |        |                   |
;; |   |                    |        |    category-e     |
;; |   |                    |        |-------------------|
;; |   |                    |        |                   |
;; |   |--------------------|        |                   |
;; |   |                    |        |    category-d     |
;; |   |    category-a      |        |                   |
;; |   |                    |        |                   |
;; |   +--------------------+        +-------------------+
;; ----------------------------------------------------------------
;;          group-A                       group-B

(define group-name? (or/c string? symbol?))
(define category-name? group-name?)

(define category-data? (listof (list/c category-name? number?)))
(define grouped-category-data? (listof (list/c group-name? category-data?)))

(define plot-renderer-tree? any/c)

(define (label-as-point pos
                        text
                        [with-point? #f]
                        #:anchor [anchor 'center])
  (point-label pos
               text
               #:anchor anchor
               #:point-size (if with-point? (label-point-size) 0)))

(define/contract (histogram-stack-with-labels group-name
                                              points
                                              #:x-min [x-min 0]
                                              #:label-groups? [label-groups? #t]
                                              #:label-total? [label-total?/anchor #f]
                                              #:invert? [invert? #f]
                                              #:common-legend? [common-legend? #f])
  (->* {group-name? category-data?}
       {#:x-min natural?
        #:label-total? (or/c #f anchor/c 'auto)
        #:label-groups? boolean?
        #:invert? boolean?
        #:common-legend? boolean?}
       plot-renderer-tree?)

  (define histogram-stack
    (stacked-histogram (list (vector group-name
                                     (list->vector
                                      (map second points))))
                       #:x-min x-min
                       #:invert? invert?
                       #:labels (if common-legend?
                                    (map first points)
                                    '(#f))))
  (define-values (subcategory-labels total-category-amount)
    (for/fold ([labels empty]
               [last-subcategory-height 0])
              ([subcategory (in-list points)])
      (match-define (list name amount) subcategory)
      (define center-of-this-subcategory (+ last-subcategory-height
                                            (/ amount 2)))
      (define this-subcategory-height (+ last-subcategory-height amount))
      (define label (label-as-point (vector (+ 0.5 x-min)
                                            center-of-this-subcategory)
                                    (format "~a: ~a" name amount)))
      (values (cons label labels)
              this-subcategory-height)))
  (list histogram-stack
        (if label-groups?
            subcategory-labels
            empty)
        (if label-total?/anchor
            (label-as-point (vector (+ 0.5 x-min)
                                    total-category-amount)
                            #;(format "~a: ~a" group-name total-category-amount)
                            (~a total-category-amount)
                            #:anchor label-total?/anchor)
            empty)))

(define/contract (grouped-category-stacked-histogram data
                                                     #:label-groups? [label-groups? #t]
                                                     #:label-total? [label-total?/anchor #f]
                                                     #:invert? [invert? #f]
                                                     #:common-legend? [common-legend? #t])
  (->* {grouped-category-data?}
       {#:label-groups? boolean?
        #:label-total? (or/c #f anchor/c 'auto)
        #:invert? boolean?
        #:common-legend? boolean?}
       plot-renderer-tree?)

  (for/list ([{group-name category-data*} (in-dict data)]
             [i (in-naturals)])
    (define category-data (car category-data*)) ;; dict wraps 2nd in list
    (histogram-stack-with-labels group-name category-data
                                 #:x-min i
                                 #:label-groups? label-groups?
                                 #:label-total? label-total?/anchor
                                 #:invert? invert?
                                 #:common-legend? (and common-legend? (= i 0)))))

(define/contract (plain-histogram-with-labels data
                                              #:label [label #f]
                                              #:color [color 1]
                                              #:skip  [skip 1.5]
                                              #:x-min [x-min 0]
                                              #:label-anchor [label-anchor 'auto])
  (->* {category-data?}
       {#:label category-name?
        #:color color/c
        #:skip  real?
        #:x-min real?
        #:label-anchor (or/c anchor/c 'auto)}
       plot-renderer-tree?)

  (define (create-label-point value index)
    (label-as-point (vector (+ index 0.5 x-min (* (- skip 1) index))
                            (second value))
                    (number->string (second value))
                    #:anchor label-anchor))

  (list (discrete-histogram data
                            #:label label
                            #:color color
                            #:skip skip
                            #:x-min x-min)
        (map create-label-point data (range (length data)))))

(define (make-angled-label-plotter plot [angle 45])
  (make-keyword-procedure
   (λ (kws kw-args . rest)
     (parameterize ([plot-x-tick-label-anchor  'top-right]
                    [plot-x-tick-label-angle   angle])
       (keyword-apply plot kws kw-args rest)))))

(define plot/labels-angled
  (make-angled-label-plotter plot))

;; If `color-seq` has fewer colors than there are bars in the data, the colors
;; will be recycled.
;; Signature:
;; ((listof color?) . -> . 2d-renderer-function?)
;; where
;; - color? : anything plot recognizes as a color
;; - renderer2d-maker? is a function like `discrete-histogram`,
;;   which produces a renderer2d?
(define (discrete-histogram/colors color-seq)
  (make-keyword-procedure
   (λ (kws kw-args data)
     (for/list ([bar-info data]
                [color (in-cycle (in-list color-seq))]
                [i (in-naturals)])
       (keyword-apply discrete-histogram
                      kws
                      kw-args
                      ;; wrap it twice because this is `apply`
                      (list (list bar-info))
                      #:color color
                      #:x-min i)))))

;; This function generates histograms that look like this:
;;
;; |                         +-------+
;; |                         |       |
;; |             +-------+   |       |
;; |  +------+   |       |   |       |
;; |  |      |   |       |   |       |
;; |  |      |   |       |   |       |
;; |  |      |   |       |   |       |
;; |  |      |   |       |   |       |
;; |  |      |   |       |   |       |
;; +--+------+---+-------+---+-------+--------
;; |  |......|   |.......|   |.......|
;; |  |......|   |.......|   +-------+
;; |  +------+   |.......|
;; |             |.......|
;; |             +-------+
;; |
;; |
;; |
;; |
;; +-------------------------------------------
;;      A           B           C
;;
;; From data like '((A 3 2) (B 3.5 3) (C 4 1))
(define/contract (two-sided-histogram data
                                      #:top-color [top-color (rectangle-color)]
                                      #:bot-color [bot-color (rectangle-color)]
                                      #:gap [gap (discrete-histogram-gap)]
                                      #:skip [skip (discrete-histogram-skip)]
                                      #:add-ticks? [add-ticks? #t])
  ({(listof (list/c any/c real? real?))}
   {#:top-color any/c
    #:bot-color any/c
    #:gap any/c
    #:skip any/c
    #:add-ticks? boolean?}
   . ->* .
   list?)
  (list (discrete-histogram (map (match-lambda [(list name top _) (list name top)])
                                 data)
                            #:color top-color
                            #:gap gap
                            #:skip skip
                            #:add-ticks? add-ticks?)
        ;; lltodo: this is a workaround for a bug with discrete-histogram
        ;; This doesn't work:
        #;(plot (list (discrete-histogram '((a 5) (b 3)))
                         (discrete-histogram '((a -2) (b -4)))
                         (x-axis))
                   #:y-min -5)
        ;; while this does:
        #;(plot (list (discrete-histogram '((a 5) (b 3)))
                         (discrete-histogram '((a -2)) #:x-min 0)
                         (discrete-histogram '((b -4)) #:x-min 1)
                         (x-axis))
                   #:y-min -5)
        (for/list ([bar-sides (in-list data)]
                   [i (in-naturals)])
          (match-define (list name _ bottom) bar-sides)
          (discrete-histogram `((,name ,(- bottom)))
                              #:x-min i
                              #:color bot-color
                              #:gap gap
                              #:skip skip
                              #:add-ticks? add-ticks?))))
