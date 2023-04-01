#lang racket/base

(module* test #f
  (require page-sequence
           racket/format
           racket/stream)

  (define it
    (make-paged-sequence (λ () 0)
                         (λ (i)
                           (displayln (~a "contents for page " i))
                           (in-range i (+ 10 i)))
                         (λ (i) (+ 10 i))
                         (λ (i) (>= i 100))))

  (for ([x it]
        #:when (odd? x)
        #:break (> x 34))
    (displayln (~a "element " x))))
