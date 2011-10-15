#lang racket/base

(require
  "predicates.rkt"
  "../ffi/safe.rkt")

(provide 
  llvm:integer-type?
  llvm:float-type?
  llvm-get-type-kind)

(define (llvm:integer-type? type)
 (and (llvm:type? type)
  (let ((type-kind (llvm-get-type-kind type)))
   (equal? type-kind 'LLVMIntegerTypeKind))))

(define (llvm:float-type? type)
 (and (llvm:type? type)
  (let ((type-kind (llvm-get-type-kind type)))
   (member type-kind
           '(LLVMFloatTypeKind
             LLVMDoubleTypeKind
             LLVMX86_FP80TypeKind
             LLVMFP128TypeKind
             LLVMPPC_FP128TypeKind))
   #t)))

(define (llvm-get-type-kind type)
 (LLVMGetTypeKind type))

