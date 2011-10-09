#lang racket/base

(require racket/contract)
(provide list*/c non-empty-list*/c power-of-two?)

(define (list*/c ctc)
 (flat-rec-contract rec
  (cons/c ctc rec)
  (list/c (listof ctc))))


(define (non-empty-list*/c ctc)
 (or/c
   (cons/c ctc (list*/c ctc))
   (list/c (non-empty-listof ctc))))

(define (power-of-two? v)
  (and (exact-positive-integer? v)
       (let loop ((w 1))
         (if (>= w v)
             (= w v)
             (loop (* 2 w))))))
