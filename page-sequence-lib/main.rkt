#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/sequence
         syntax/parse/define)

(provide (contract-out [make-paged-sequence
                        (-> (-> any)
                            (-> any/c sequence?)
                            (-> any/c any)
                            (-> any/c boolean?)
                            sequence?)]))

(struct sequence-lazy-wrapper (thunk)
  #:property prop:sequence
  (λ (this) ((sequence-lazy-wrapper-thunk this))))

(define-syntax-parse-rule (sequence-lazy e)
  #:declare e (expr/c #'sequence?)
  (sequence-lazy-wrapper (λ () e.c)))

(define (make-paged-sequence init-proc
                             get-elements-proc
                             next-proc
                             at-end-pred)
  (define (do-page at-end? get-next)
    (cond
      [at-end? empty-sequence]
      [else
       (define result (get-next))
       (sequence-append
        (get-elements-proc result)
        (sequence-lazy (do-page (at-end-pred result)
                                (λ () (next-proc result)))))]))
  (do-page #f init-proc))
