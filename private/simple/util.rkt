#lang racket/base

(require
  racket/contract
  unstable/contract)
(provide
 (contract-out
  (list*/c (-> contract? contract?))
  (non-empty-list*/c (-> contract? contract?))
  (power-of-two? predicate/c)))

(define (list*/c ctc)
 (cond
  ((flat-contract? ctc)
   (flat-rec-contract rec
    (cons/c ctc rec)
    (list/c (listof ctc))))
  ((chaperone-contract? ctc)
   (define rec
     (recursive-contract
       (or/c (cons/c ctc rec)
             (cons/c (listof ctc) null))
       #:chaperone))
   rec)
  (else
   (define rec
     (recursive-contract
       (or/c (cons/c ctc rec)
             (cons/c (listof ctc) null))))
   rec)))



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
