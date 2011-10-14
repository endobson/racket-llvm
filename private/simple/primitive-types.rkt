#lang racket/base

(require
  "../safe/structs.rkt"
  "../ffi/safe.rkt")

(provide 
  llvm-integer-type-ref?
  llvm-float-type-ref?
  llvm-get-type-kind)

(define (llvm-integer-type-ref? ref)
 (and (llvm-type-ref? ref)
  (let ((type-kind (llvm-get-type-kind ref)))
   (equal? type-kind 'LLVMIntegerTypeKind))))

(define (llvm-float-type-ref? ref)
 (and (llvm-type-ref? ref)
  (let ((type-kind (llvm-get-type-kind ref)))
   (member type-kind
           '(LLVMFloatTypeKind
             LLVMDoubleTypeKind
             LLVMX86_FP80TypeKind
             LLVMFP128TypeKind
             LLVMPPC_FP128TypeKind))
   #t)))

(define (llvm-get-type-kind type)
 (LLVMGetTypeKind type))

