#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/*===-- Instruction builders ----------------------------------------------===*/

;/* An instruction builder represents a point within a basic block, and is the
; * exclusive means of building instructions using the C interface.
; */

(define-llvm LLVMCreateBuilderInContext (_fun LLVMContextRef -> LLVMBuilderRef))
(define-llvm LLVMCreateBuilder (_fun -> LLVMBuilderRef))

(define-llvm LLVMPositionBuilder (_fun LLVMBuilderRef LLVMBasicBlockRef LLVMValueRef -> _void))
(define-llvm LLVMPositionBuilderBefore
  (_fun LLVMBuilderRef LLVMValueRef -> _void))

(define-llvm LLVMPositionBuilderAtEnd (_fun LLVMBuilderRef LLVMBasicBlockRef -> _void))

(define-llvm LLVMGetInsertBlock (_fun LLVMBuilderRef -> LLVMBasicBlockRef))

(define-llvm LLVMClearInsertionPosition
 (_fun LLVMBuilderRef -> _void))

(define-llvm LLVMInsertIntoBuilder
 (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm LLVMInsertIntoBuilderWithName
 (_fun LLVMBuilderRef LLVMValueRef _string -> _void))

(define-llvm LLVMDisposeBuilder (_fun LLVMBuilderRef -> _void))




;/* Metadata */
(define-llvm LLVMGetCurrentDebugLocation (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm LLVMSetCurrentDebugLocation (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm LLVMSetInstDebugLocation (_fun LLVMBuilderRef LLVMValueRef -> _void))

