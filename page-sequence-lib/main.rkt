#lang racket/base

(require racket/contract
         racket/sequence
         racket/stream)

(provide (contract-out
          [make-paged-sequence
           (-> (-> any)
               (-> any/c sequence?)
               (-> any/c any)
               (-> any/c boolean?)
               sequence?)]))

(define (pos-page ps)   (list-ref ps 0))
(define (pos-elems ps)  (list-ref ps 1))

(define (make-paged-sequence init-page-proc
                             page->elems
                             next-page
                             at-end?)
  (make-do-sequence
   (λ ()
     (define init-page (init-page-proc))
     (define elems (sequence->stream (page->elems init-page)))
     (define init-pos (list init-page elems))
     (define (pos->element ps)
       (stream-first (pos-elems ps)))
     (define (next-pos ps)
       (cond
         [(and (stream-empty? (stream-rest (pos-elems ps)))
               (not (at-end? (pos-page ps))))
          (define page (next-page (pos-page ps)))
          (define elems (sequence->stream (page->elems page)))
          (list page elems)]
         [else
          (list (pos-page ps)
                (stream-rest (pos-elems ps)))]))
     (define (continue-with-pos? ps)
       (not (and (stream-empty? (pos-elems ps))
                 (at-end? (pos-page ps)))))
     (initiate-sequence #:init-pos     init-pos
                        #:pos->element pos->element
                        #:next-pos     next-pos
                        #:continue-with-pos?
                        continue-with-pos?))))
