#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Operations on instructions */
(define-llvm-unsafe LLVMGetInstructionParent (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm-multiple-unsafe
 (LLVMGetNextInstruction
  LLVMGetPreviousInstruction)
 (_fun LLVMValueRef -> LLVMValueRef))

;/* Operations on call sites */

(define-llvm-unsafe LLVMGetInstructionCallConv (_fun LLVMValueRef -> LLVMCallConv))
(define-llvm-unsafe LLVMSetInstructionCallConv (_fun LLVMValueRef LLVMCallConv -> _void))

(define-llvm-unsafe LLVMAddInstrAttribute (_fun LLVMValueRef _uint LLVMAttribute -> _void))
(define-llvm-unsafe LLVMRemoveInstrAttribute (_fun LLVMValueRef _uint LLVMAttribute -> _void))

(define-llvm-unsafe LLVMSetInstrParamAlignment (_fun LLVMValueRef _uint _uint -> _void))


;/* Operations on call instructions (only) */

(define-llvm-unsafe LLVMIsTailCall (_fun LLVMValueRef -> LLVMBool))
(define-llvm-unsafe LLVMSetTailCall (_fun LLVMValueRef LLVMBool -> _void))

;/* Operations on switch instructions (only) */
;(define-llvm-unsafe LLVMGetSwitchDefaultDest (_fun LLVMValueRef -> LLVMBasicBlockRef))

;/* Operations on phi nodes */
(define-llvm-unsafe LLVMAddIncoming
 (_fun (phi values blocks) ::
       (phi : LLVMValueRef)
       (values : (_list i LLVMValueRef))
       (blocks : (_list i LLVMBasicBlockRef))
       (list : _uint = (min (length values) (length blocks)))
       -> _void))

   

(define-llvm-unsafe LLVMCountIncoming (_fun LLVMValueRef -> _uint))
(define-llvm-unsafe LLVMGetIncomingValue (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm-unsafe LLVMGetIncomingBlock (_fun LLVMValueRef _uint -> LLVMBasicBlockRef))

