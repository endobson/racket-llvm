#lang racket/base

(require
  racket/contract
  unstable/contract
  "predicates.rkt"
  "../ffi/safe.rkt")

(provide
 (contract-out
  (llvm:value? predicate/c)

  (llvm-type-of (-> llvm:value? llvm:type?))
  (llvm-terminator-instruction? (-> llvm:value? boolean?))
  (llvm-get-undef (-> llvm:type? llvm:value?))
  (llvm-null (-> llvm:type? llvm:value?))
  (llvm:constant? predicate/c)
  (llvm-set-value-name (-> llvm:value? string? void?)) ))




(define (llvm-terminator-instruction? value)
 (LLVMIsTerminatorInstruction value))

(define (llvm:constant? value)
 (and (llvm:value? value)
  (LLVMIsConstant value)))


(define (llvm-get-undef type)
 (LLVMGetUndef type))

(define (llvm-null type)
 (LLVMConstNull type))

(define (llvm-set-value-name value name)
 (LLVMSetValueName value name))


(define (llvm-type-of value)
 (LLVMTypeOf value))

