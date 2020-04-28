#lang at-exp racket

(provide add-x-indices
         (struct-out inferred)
         inferrer/c)

(require plot
         syntax/parse/define)

(define (add-x-indices data)
  (for/list ([v (in-list data)]
             [x (in-naturals)])
    (list x v)))

;; Line file->list but for strings
(define (string->list/read str)
  (with-input-from-string str port->list))

(struct inferred (ticks data)
  #:transparent)

(define inferrer/c ((non-empty-listof any/c) . -> . (or/c #f inferred?)))

(define-simple-macro (define/provide-simple-inferrer name
                       [pattern maybe-opts ...
                                #:ticks ticks:expr
                                #:convert data-converter:expr]
                       ...)
  (begin
    (define (name data)
      (match (first data)
        [pattern
         maybe-opts ...
         (inferred ticks
                   (data-converter data))]
        ...
        [else #f]))
    (provide (contract-out [name inferrer/c]))))

(define-simple-macro (map-match-lambda pattern-pairs ...)
  (λ (l) (map (match-lambda pattern-pairs ...) l)))

(define/provide-simple-inferrer infer-single-values
  [(regexp @pregexp{^\s*[-+\.\d]+\s*$})
   #:ticks (plot-x-ticks)
   #:convert convert-single-number-strings]
  [(? number?)
   #:ticks (plot-x-ticks)
   #:convert add-x-indices])
(define (convert-single-number-strings data)
  (add-x-indices (map string->number data)))

[define/provide-simple-inferrer infer-number-pairs
  [(regexp @pregexp{^\s*([-+\.\d]+)\s*[-+\.\d]+\s*$})
   #:ticks (plot-x-ticks)
   #:convert convert-number-pair-strings]
  [(list (? number?) (? number?))
   #:ticks (plot-x-ticks)
   #:convert identity]
  [(cons (? number?) (? number?))
   #:ticks (plot-x-ticks)
   #:convert (map-match-lambda [(cons a b) (list a b)])]]
(define (convert-number-pair-strings data)
  (for/list ([line (in-list data)])
    (string->list/read line)))

(define epoch-ts:1990-01-01
  631152001)
(define/provide-simple-inferrer infer-epoch-timestamps
  [(regexp @pregexp{^\s*([-+\.\d]+)\s*[-+\.\d]+\s*$}
           (list _ epoch-ts))
   #:when (>= (string->number epoch-ts) epoch-ts:1990-01-01)
   #:ticks (date-ticks)
   #:convert convert-number-pair-strings]
  [(list (? number? epoch-ts) (? number?))
   #:when (>= epoch-ts epoch-ts:1990-01-01)
   #:ticks (date-ticks)
   #:convert identity]
  [(cons (? number? epoch-ts) (? number?))
   #:when (>= epoch-ts epoch-ts:1990-01-01)
   #:ticks (date-ticks)
   #:convert (map-match-lambda [(cons a b) (list a b)])])

(define/provide-simple-inferrer infer-name-value-pairs
  [(regexp @pregexp{^\s*\S+\s+[-+\.\d]+\s*$})
   #:ticks (plot-x-ticks)
   #:convert convert-name-number-pair-strings]
  [(regexp (pregexp @~a{^\s*"[^@"\""]+"\s+[-+\.\d]+\s*$}))
   #:ticks (plot-x-ticks)
   #:convert convert-name-number-pair-strings]
  [(cons name (? number?))
   #:ticks (plot-x-ticks)
   #:convert (map-match-lambda [(cons a b) (vector a b)])])
(define (convert-name-number-pair-strings data)
  (for/list ([line (in-list data)])
    (apply vector (string->list/read line))))
