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


(define-llvm-safe LLVMGetInstructionParent
  (_fun (v : safe:LLVMValueRef) ->
        (ptr : _pointer) ->
        (safe:llvm-basic-block-ref ptr (safe:llvm-value-ref-owner v))))


;/* Operations on call sites */

(define-llvm-unsafe LLVMGetInstructionCallConv (_fun LLVMValueRef -> LLVMCallConv))
(define-llvm-unsafe LLVMSetInstructionCallConv (_fun LLVMValueRef LLVMCallConv -> _void))

(define-llvm-safe LLVMGetInstructionCallConv (_fun safe:LLVMValueRef -> LLVMCallConv))
(define-llvm-safe LLVMSetInstructionCallConv (_fun safe:LLVMValueRef LLVMCallConv -> _void))


(define-llvm-unsafe LLVMAddInstrAttribute (_fun LLVMValueRef _uint LLVMAttribute -> _void))
(define-llvm-unsafe LLVMRemoveInstrAttribute (_fun LLVMValueRef _uint LLVMAttribute -> _void))

(define-llvm-unsafe LLVMSetInstrParamAlignment (_fun LLVMValueRef _uint _uint -> _void))


;/* Operations on call instructions (only) */

(define-llvm-unsafe LLVMIsTailCall (_fun LLVMValueRef -> LLVMBool))
(define-llvm-unsafe LLVMSetTailCall (_fun LLVMValueRef LLVMBool -> _void))

(define-llvm-safe LLVMIsTailCall (_fun safe:LLVMValueRef -> LLVMBool))
(define-llvm-safe LLVMSetTailCall (_fun safe:LLVMValueRef LLVMBool -> _void))


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


;/* Operations on phi nodes */
(define-llvm-safe LLVMAddIncoming
 (_fun (phi values blocks) ::
       (phi : safe:LLVMValueRef)
       (values : (_list i safe:LLVMValueRef))
       (blocks : (_list i safe:LLVMBasicBlockRef))
       (list : _uint = (min (length values) (length blocks)))
       -> _void))

   

(define-llvm-unsafe LLVMCountIncoming (_fun LLVMValueRef -> _uint))
(define-llvm-unsafe LLVMGetIncomingValue (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm-unsafe LLVMGetIncomingBlock (_fun LLVMValueRef _uint -> LLVMBasicBlockRef))

