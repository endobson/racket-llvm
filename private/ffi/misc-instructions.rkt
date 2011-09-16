#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Miscellaneous instructions */

(define-llvm LLVMBuildPhi (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm LLVMBuildCall
 (_fun (builder fun args name) ::
       (builder : LLVMBuilderRef)
       (fun : LLVMValueRef)
       (args : (_list i LLVMValueRef))
       (_uint = (length args))
       (name : _string)
       -> LLVMValueRef))

(define-llvm LLVMBuildSelect
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm LLVMBuildVAArg
 (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm LLVMBuildExtractElement
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildInsertElement
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm LLVMBuildShuffleVector
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))


(define-llvm LLVMBuildExtractValue
 (_fun LLVMBuilderRef LLVMValueRef _uint  _string -> LLVMValueRef))
(define-llvm LLVMBuildInsertValue
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _uint  _string -> LLVMValueRef))
(define-llvm-multiple
 (LLVMBuildIsNull
  LLVMBuildIsNotNull)
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm LLVMBuildPtrDiff (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

