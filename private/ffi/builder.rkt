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

(define-llvm-unsafe LLVMCreateBuilderInContext (_fun LLVMContextRef -> LLVMBuilderRef))
(define-llvm-unsafe LLVMCreateBuilder (_fun -> LLVMBuilderRef))

(define-llvm-unsafe LLVMPositionBuilder (_fun LLVMBuilderRef LLVMBasicBlockRef LLVMValueRef -> _void))
(define-llvm-unsafe LLVMPositionBuilderBefore
  (_fun LLVMBuilderRef LLVMValueRef -> _void))

(define-llvm-unsafe LLVMPositionBuilderAtEnd (_fun LLVMBuilderRef LLVMBasicBlockRef -> _void))

(define-llvm-unsafe LLVMGetInsertBlock (_fun LLVMBuilderRef -> LLVMBasicBlockRef))

(define-llvm-unsafe LLVMClearInsertionPosition
 (_fun LLVMBuilderRef -> _void))

(define-llvm-unsafe LLVMInsertIntoBuilder
 (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm-unsafe LLVMInsertIntoBuilderWithName
 (_fun LLVMBuilderRef LLVMValueRef _string -> _void))

(define-llvm-unsafe LLVMDisposeBuilder (_fun LLVMBuilderRef -> _void))




;/* Metadata */
(define-llvm-unsafe LLVMGetCurrentDebugLocation (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm-unsafe LLVMSetCurrentDebugLocation (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm-unsafe LLVMSetInstDebugLocation (_fun LLVMBuilderRef LLVMValueRef -> _void))

