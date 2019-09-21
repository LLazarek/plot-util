#lang racket/base

(require racket/contract/base
         racket/function
         racket/list
         racket/math)

(provide (contract-out
          [moving-average ([(listof any/c)]
                           [(any/c . -> . number?)
                            #:sides natural?]
                           . ->* .
                           (listof (cons/c any/c number?)))]))

(define (moving-average data
                        [selector identity]
                        #:sides [sides 1])
  (define sides*2+1 (add1 (* sides 2)))
  (define lists
    (for/list ([i (in-range sides*2+1)])
      (drop data i)))
  (define lists*
    (for/list ([l (in-list (reverse lists))]
               [i (in-naturals)])
      (drop-right l i)))
  (define means
    (apply map
           (λ l (/ (for/sum ([el (in-list l)])
                     (selector el))
                   sides*2+1))
           lists*))
  (map cons
       (list-ref lists* sides)
       means))

(module+ test
  (require rackunit)
  (check-equal? (moving-average '(1 2 3 4 5))
                '((2 . 2) (3 . 3) (4 . 4)))
  (check-equal? (moving-average '(1 10 5 8 2 7 3))
                '((10 . 16/3) (5 . 23/3) (8 . 5) (2 . 17/3) (7 . 4))))
