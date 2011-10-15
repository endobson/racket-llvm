#lang racket/base

(require
  racket/contract
  unstable/contract
  "../safe/structs.rkt")


(provide
 (contract-out
  (llvm:value? predicate/c)
  (llvm:type? predicate/c)
  (llvm:module? predicate/c)
  (llvm:context? predicate/c)
  (llvm:basic-block? predicate/c)
  (llvm:builder? predicate/c)))

(define (llvm:value? v)
  (llvm-value-ref? v))

(define (llvm:type? t)
  (llvm-type-ref? t))

(define (llvm:module? m)
  (llvm-module-ref? m))

(define (llvm:context? m)
  (llvm-context-ref? m))

(define (llvm:builder? b)
  (llvm-builder-ref? b))

(define (llvm:basic-block? b)
  (llvm-basic-block-ref? b))

