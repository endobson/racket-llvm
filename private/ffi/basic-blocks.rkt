#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))


(define-llvm LLVMBlockAddress (_fun LLVMValueRef LLVMBasicBlockRef -> LLVMValueRef))



;/* Operations on basic blocks */

(define-llvm LLVMBasicBlockAsValue (_fun LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm LLVMValueIsBasicBlock (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMValueAsBasicBlock (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMGetBasicBlockParent (_fun LLVMBasicBlockRef -> LLVMValueRef))
;(define-llvm LLVMGetBasicBlockTerminator (_fun LLVMBasicBlockRef -> LLVMValueRef))
;Not yet in my dev repo of llvm

(define-llvm LLVMCountBasicBlocks (_fun LLVMValueRef -> _uint))
(define-llvm LLVMGetBasicBlocks
 (_fun (fun) ::
       (fun : LLVMValueRef)
       (blocks : (_list o LLVMBasicBlockRef (LLVMCountBasicBlocks fun)))
       -> _void
       -> blocks))

(define-llvm-multiple
 (LLVMGetEntryBasicBlock
  LLVMGetFirstBasicBlock
  LLVMGetLastBasicBlock)
 (_fun LLVMValueRef -> LLVMBasicBlockRef))

(define-llvm-multiple
 (LLVMGetNextBasicBlock
  LLVMGetPreviousBasicBlock)
 (_fun LLVMBasicBlockRef -> LLVMBasicBlockRef))

(define-llvm LLVMAppendBasicBlockInContext (_fun LLVMContextRef LLVMValueRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMInsertBasicBlockInContext (_fun LLVMContextRef LLVMBasicBlockRef _string -> LLVMBasicBlockRef))


(define-llvm LLVMAppendBasicBlock
 (_fun LLVMValueRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMInsertBasicBlock
 (_fun LLVMBasicBlockRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMDeleteBasicBlock
 (_fun LLVMBasicBlockRef -> _void))
;(define-llvm LLVMRemoveBasicBlockFromParent
; (_fun LLVMBasicBlockRef -> _void))

(define-llvm-multiple
 (LLVMMoveBasicBlockBefore
  LLVMMoveBasicBlockAfter)
 (_fun LLVMBasicBlockRef LLVMBasicBlockRef -> _void))


(define-llvm-multiple
 (LLVMGetFirstInstruction
  LLVMGetLastInstruction)
 (_fun LLVMBasicBlockRef -> LLVMValueRef))

