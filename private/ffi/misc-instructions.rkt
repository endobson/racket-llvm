#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Miscellaneous instructions */

(define-llvm-unsafe LLVMBuildPhi (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildCall
 (_fun (builder fun args name) ::
       (builder : LLVMBuilderRef)
       (fun : LLVMValueRef)
       (args : (_list i LLVMValueRef))
       (_uint = (length args))
       (name : _string)
       -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildSelect
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildVAArg
 (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildExtractElement
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm-unsafe LLVMBuildInsertElement
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildShuffleVector
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))


(define-llvm-unsafe LLVMBuildExtractValue
 (_fun LLVMBuilderRef LLVMValueRef _uint  _string -> LLVMValueRef))
(define-llvm-unsafe LLVMBuildInsertValue
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _uint  _string -> LLVMValueRef))
(define-llvm-multiple-unsafe
 (LLVMBuildIsNull
  LLVMBuildIsNotNull)
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildPtrDiff (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

