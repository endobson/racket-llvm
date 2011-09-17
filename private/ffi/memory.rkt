#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))


;/* Memory */
(define-llvm-multiple-unsafe
 (LLVMBuildMalloc
  LLVMBuildAlloca)
 (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm-multiple-unsafe
 (LLVMBuildArrayMalloc
  LLVMBuildArrayAlloca)
 (_fun LLVMBuilderRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildFree (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildLoad
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm-unsafe LLVMBuildStore
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMBuildGEP
  LLVMBuildInBoundsGEP)
 (_fun (builder ptr indices name) ::
       (builder : LLVMBuilderRef)
       (ptr : LLVMValueRef)
       (indices : (_list i LLVMValueRef))
       (_uint = (length indices))
       (name : _string)
       -> LLVMValueRef))


(define-llvm-unsafe LLVMBuildStructGEP
 (_fun LLVMBuilderRef LLVMValueRef _uint _string -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMBuildGlobalString LLVMBuildGlobalStringPtr)
 (_fun LLVMBuilderRef _string _string -> LLVMValueRef))
