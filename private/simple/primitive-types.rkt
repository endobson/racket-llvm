#lang racket/base

(require
  "predicates.rkt"
  "../ffi/safe.rkt")

(provide 
  llvm:integer-type?
  llvm:float-type?
  llvm-get-type-kind)

(define (llvm:integer-type? ref)
 (and (llvm:type? ref)
  (let ((type-kind (llvm-get-type-kind ref)))
   (equal? type-kind 'LLVMIntegerTypeKind))))

(define (llvm:float-type? ref)
 (and (llvm:type? ref)
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

