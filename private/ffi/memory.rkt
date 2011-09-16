#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))


;/* Memory */
(define-llvm-multiple
 (LLVMBuildMalloc
  LLVMBuildAlloca)
 (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm-multiple
 (LLVMBuildArrayMalloc
  LLVMBuildArrayAlloca)
 (_fun LLVMBuilderRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm LLVMBuildFree (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMBuildLoad
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildStore
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMBuildGEP
  LLVMBuildInBoundsGEP)
 (_fun (builder ptr indices name) ::
       (builder : LLVMBuilderRef)
       (ptr : LLVMValueRef)
       (indices : (_list i LLVMValueRef))
       (_uint = (length indices))
       (name : _string)
       -> LLVMValueRef))


(define-llvm LLVMBuildStructGEP
 (_fun LLVMBuilderRef LLVMValueRef _uint _string -> LLVMValueRef))

(define-llvm-multiple
 (LLVMBuildGlobalString LLVMBuildGlobalStringPtr)
 (_fun LLVMBuilderRef _string _string -> LLVMValueRef))
