#lang racket/base

(provide
 group-name?
 category-name?
 category-data?
 grouped-category-data?

 plain-histogram-with-labels
 grouped-category-stacked-histogram
 histogram-stack-with-labels)

(require plot
         plot/utils
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/format
         racket/list
         racket/match
         racket/math)

;; ==================== Stacked histograp illustration ====================
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

(define (plot/labels-angled data
                            #:title title
                            #:y-label y-label
                            #:x-label x-label)
  (parameterize ([plot-x-tick-label-anchor  'auto]
                 [plot-x-tick-label-angle   45])
    (plot data
          #:title title
          #:y-label y-label
          #:x-label x-label)))

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
                                              #:label? [label?/anchor #f])
  (->* {group-name? category-data?}
       {#:x-min natural?
        #:label? (or/c #f anchor/c 'auto)}
       plot-renderer-tree?)

  (define histogram-stack
    (stacked-histogram (list (vector group-name
                                     (list->vector
                                      (map second points))))
                       #:x-min x-min))
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
        subcategory-labels
        (if label?/anchor
            (label-as-point (vector (+ 0.5 x-min)
                                    total-category-amount)
                            #;(format "~a: ~a" group-name total-category-amount)
                            (~a total-category-amount)
                            #:anchor label?/anchor)
            empty)))

(define/contract (grouped-category-stacked-histogram data
                                                     #:label-groups? [label-groups?/anchor #f])
  (->* {grouped-category-data?}
       {#:label-groups? (or/c #f anchor/c 'auto)}
       plot-renderer-tree?)

  (for/list ([{group-name category-data*} (in-dict data)]
             [i (in-naturals)])
    (define category-data (car category-data*)) ;; dict wraps 2nd in list
    (histogram-stack-with-labels group-name category-data
                                 #:x-min i
                                 #:label? label-groups?/anchor)))

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
