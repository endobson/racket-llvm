#lang racket/base

(require
  racket/contract
  unstable/contract
  "../safe/structs.rkt")


(provide
 (contract-out
  (llvm:value? predicate/c)
  (llvm:type? predicate/c)))

(define (llvm:value? v)
  (llvm-value-ref? v))

(define (llvm:type? t)
  (llvm-type-ref? t))

