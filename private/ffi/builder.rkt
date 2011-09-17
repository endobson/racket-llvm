#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (except-out (all-defined-out)
                     safe:position-builder-at-end))



(define safe:position-builder-at-end
  (_fun (builder bb) ::
        (builder : safe:LLVMBuilderRef)
        (bb : safe:LLVMBasicBlockRef) ->
        _void ->
        (begin
          (set-safe:llvm-builder-ref-module! builder
            (safe:llvm-basic-block-ref-module bb))
          (void))))

;/*===-- Instruction builders ----------------------------------------------===*/

;/* An instruction builder represents a point within a basic block, and is the
; * exclusive means of building instructions using the C interface.
; */

(define-llvm-unsafe LLVMCreateBuilderInContext (_fun LLVMContextRef -> LLVMBuilderRef))
(define-llvm-unsafe LLVMCreateBuilder (_fun -> LLVMBuilderRef))

(define-llvm-safe LLVMCreateBuilderInContext safe:LLVMBuilderCreator)

(define-llvm-unsafe LLVMPositionBuilder (_fun LLVMBuilderRef LLVMBasicBlockRef LLVMValueRef -> _void))
(define-llvm-unsafe LLVMPositionBuilderBefore
  (_fun LLVMBuilderRef LLVMValueRef -> _void))

(define-llvm-unsafe LLVMPositionBuilderAtEnd (_fun LLVMBuilderRef LLVMBasicBlockRef -> _void))
(define-llvm-safe LLVMPositionBuilderAtEnd safe:position-builder-at-end)

(define-llvm-unsafe LLVMGetInsertBlock (_fun LLVMBuilderRef -> LLVMBasicBlockRef))
(define-llvm-safe LLVMGetInsertBlock
  (_fun (builder : safe:LLVMBuilderRef) ->
        (ptr : _pointer) ->
        (safe:llvm-basic-block-ref ptr (safe:llvm-builder-ref-module builder))))

(define-llvm-unsafe LLVMClearInsertionPosition
 (_fun LLVMBuilderRef -> _void))

(define-llvm-unsafe LLVMInsertIntoBuilder
 (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm-unsafe LLVMInsertIntoBuilderWithName
 (_fun LLVMBuilderRef LLVMValueRef _string -> _void))

(define-llvm-unsafe LLVMDisposeBuilder (_fun LLVMBuilderRef -> _void))
(define (safe:LLVMDisposeBuilder builder) (void))




;/* Metadata */
(define-llvm-unsafe LLVMGetCurrentDebugLocation (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm-unsafe LLVMSetCurrentDebugLocation (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm-unsafe LLVMSetInstDebugLocation (_fun LLVMBuilderRef LLVMValueRef -> _void))

