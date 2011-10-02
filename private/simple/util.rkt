#lang racket/base

(require racket/contract)
(provide list*/c non-empty-list*/c)

(define (list*/c ctc)
 (flat-rec-contract rec
  (cons/c ctc rec)
  (list/c (listof ctc))))


(define (non-empty-list*/c ctc)
 (or/c
   (cons/c ctc (list*/c ctc))
   (list/c (non-empty-listof ctc))))

