#lang racket/base

(require
  racket/contract
  "../safe/structs.rkt"
  "../ffi/safe.rkt"
  "base.rkt")

(provide/contract
 (llvm-bit-cast (->* (llvm-value-ref? llvm-type-ref?) (#:builder llvm-builder-ref? #:name string?) llvm-value-ref?))
 (llvm-int-to-ptr (->* (llvm-integer/c llvm-type-ref?) (#:builder llvm-builder-ref? #:name string?) llvm-value-ref?))
 (llvm-ptr-to-int (->* (llvm-value-ref?) (llvm-type-ref? #:builder llvm-builder-ref? #:name string?) llvm-value-ref?))
 (llvm-sext (->* (llvm-integer/c llvm-type-ref?) (#:builder llvm-builder-ref? #:name string?) llvm-value-ref?))
 (llvm-zext (->* (llvm-integer/c llvm-type-ref?) (#:builder llvm-builder-ref? #:name string?) llvm-value-ref?))
 (llvm-trunc (->* (llvm-integer/c llvm-type-ref?) (#:builder llvm-builder-ref? #:name string?) llvm-value-ref?)))


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
