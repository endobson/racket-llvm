#lang racket/base

(require
  racket/contract
  unstable/contract
  "../safe/structs.rkt"
  "../ffi/safe.rkt")

(provide
 (contract-out
  (llvm:value? predicate/c)

 (llvm-terminator-instruction? (-> llvm-value-ref? boolean?))
 (llvm-get-undef (-> llvm-type-ref? llvm-value-ref?))
 (llvm-null (-> llvm-type-ref? llvm-value-ref?))
 (llvm:constant? predicate/c)
  

  ))


(define llvm:value? llvm-value-ref?)


(define (llvm-terminator-instruction? value)
 (LLVMIsTerminatorInstruction value))

(define (llvm:constant? value)
 (and (llvm:value? value)
  (LLVMIsConstant value)))


(define (llvm-get-undef type)
 (LLVMGetUndef type))

(define (llvm-null type)
 (LLVMConstNull type))

