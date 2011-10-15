#lang racket/base

(require
  racket/contract
  "../ffi/safe.rkt"
  "convertible.rkt"
  "predicates.rkt"
  "parameters.rkt")

(provide/contract
 (llvm-bit-cast (->* (llvm:value? llvm:type?) (#:builder llvm:builder? #:name string?) llvm:value?))
 (llvm-int-to-ptr (->* (llvm-integer/c llvm:type?) (#:builder llvm:builder? #:name string?) llvm:value?))
 (llvm-ptr-to-int (->* (llvm:value?) (llvm:type? #:builder llvm:builder? #:name string?) llvm:value?))
 (llvm-sext (->* (llvm-integer/c llvm:type?) (#:builder llvm:builder? #:name string?) llvm:value?))
 (llvm-zext (->* (llvm-integer/c llvm:type?) (#:builder llvm:builder? #:name string?) llvm:value?))
 (llvm-trunc (->* (llvm-integer/c llvm:type?) (#:builder llvm:builder? #:name string?) llvm:value?)))


(define (llvm-bit-cast pointer type #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildBitCast builder pointer type name))

(define (llvm-ptr-to-int pointer (type (current-integer-type)) #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildPtrToInt builder pointer type name))

(define (llvm-int-to-ptr int type #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildIntToPtr builder (value->llvm int) type name))

(define (llvm-sext value type #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildSExt builder (value->llvm value) type name))

(define (llvm-zext value type #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildZExt builder (value->llvm value) type name))


(define (llvm-trunc value type #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildTrunc builder (value->llvm value) type name))
