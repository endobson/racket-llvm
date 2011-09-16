#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Operations on instructions */
(define-llvm LLVMGetInstructionParent (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm-multiple
 (LLVMGetNextInstruction
  LLVMGetPreviousInstruction)
 (_fun LLVMValueRef -> LLVMValueRef))

;/* Operations on call sites */

(define-llvm LLVMGetInstructionCallConv (_fun LLVMValueRef -> LLVMCallConv))
(define-llvm LLVMSetInstructionCallConv (_fun LLVMValueRef LLVMCallConv -> _void))

(define-llvm LLVMAddInstrAttribute (_fun LLVMValueRef _uint LLVMAttribute -> _void))
(define-llvm LLVMRemoveInstrAttribute (_fun LLVMValueRef _uint LLVMAttribute -> _void))

(define-llvm LLVMSetInstrParamAlignment (_fun LLVMValueRef _uint _uint -> _void))


;/* Operations on call instructions (only) */

(define-llvm LLVMIsTailCall (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetTailCall (_fun LLVMValueRef LLVMBool -> _void))

;/* Operations on switch instructions (only) */
;(define-llvm LLVMGetSwitchDefaultDest (_fun LLVMValueRef -> LLVMBasicBlockRef))

;/* Operations on phi nodes */
(define-llvm LLVMAddIncoming
 (_fun (phi values blocks) ::
       (phi : LLVMValueRef)
       (values : (_list i LLVMValueRef))
       (blocks : (_list i LLVMBasicBlockRef))
       (list : _uint = (min (length values) (length blocks)))
       -> _void))

   

(define-llvm LLVMCountIncoming (_fun LLVMValueRef -> _uint))
(define-llvm LLVMGetIncomingValue (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMGetIncomingBlock (_fun LLVMValueRef _uint -> LLVMBasicBlockRef))

