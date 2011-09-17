#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))


(define-llvm-unsafe LLVMBlockAddress (_fun LLVMValueRef LLVMBasicBlockRef -> LLVMValueRef))



;/* Operations on basic blocks */

(define-llvm-unsafe LLVMBasicBlockAsValue (_fun LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm-unsafe LLVMValueIsBasicBlock (_fun LLVMValueRef -> LLVMBool))
(define-llvm-unsafe LLVMValueAsBasicBlock (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm-unsafe LLVMGetBasicBlockParent (_fun LLVMBasicBlockRef -> LLVMValueRef))
;(define-llvm LLVMGetBasicBlockTerminator (_fun LLVMBasicBlockRef -> LLVMValueRef))
;Not yet in my dev repo of llvm

(define-llvm-unsafe LLVMCountBasicBlocks (_fun LLVMValueRef -> _uint))
(define-llvm-unsafe LLVMGetBasicBlocks
 (_fun (fun) ::
       (fun : LLVMValueRef)
       (blocks : (_list o LLVMBasicBlockRef (unsafe:LLVMCountBasicBlocks fun)))
       -> _void
       -> blocks))

(define-llvm-multiple-unsafe
 (LLVMGetEntryBasicBlock
  LLVMGetFirstBasicBlock
  LLVMGetLastBasicBlock)
 (_fun LLVMValueRef -> LLVMBasicBlockRef))

(define-llvm-multiple-unsafe
 (LLVMGetNextBasicBlock
  LLVMGetPreviousBasicBlock)
 (_fun LLVMBasicBlockRef -> LLVMBasicBlockRef))

(define-llvm-unsafe LLVMAppendBasicBlockInContext (_fun LLVMContextRef LLVMValueRef _string -> LLVMBasicBlockRef))
(define-llvm-unsafe LLVMInsertBasicBlockInContext (_fun LLVMContextRef LLVMBasicBlockRef _string -> LLVMBasicBlockRef))


(define-llvm-unsafe LLVMAppendBasicBlock
 (_fun LLVMValueRef _string -> LLVMBasicBlockRef))
(define-llvm-unsafe LLVMInsertBasicBlock
 (_fun LLVMBasicBlockRef _string -> LLVMBasicBlockRef))
(define-llvm-unsafe LLVMDeleteBasicBlock
 (_fun LLVMBasicBlockRef -> _void))
;(define-llvm-unsafe LLVMRemoveBasicBlockFromParent
; (_fun LLVMBasicBlockRef -> _void))

(define-llvm-multiple-unsafe
 (LLVMMoveBasicBlockBefore
  LLVMMoveBasicBlockAfter)
 (_fun LLVMBasicBlockRef LLVMBasicBlockRef -> _void))


(define-llvm-multiple-unsafe
 (LLVMGetFirstInstruction
  LLVMGetLastInstruction)
 (_fun LLVMBasicBlockRef -> LLVMValueRef))

