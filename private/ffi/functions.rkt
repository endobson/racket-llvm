#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))


;/* Operations on functions */
(define-llvm LLVMAddFunction (_fun LLVMModuleRef _string LLVMTypeRef -> LLVMValueRef))

(define-llvm LLVMGetNamedFunction (_fun LLVMModuleRef _string -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetFirstFunction
  LLVMGetLastFunction)
 (_fun LLVMModuleRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetNextFunction
  LLVMGetPreviousFunction)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMDeleteFunction
 (_fun LLVMValueRef -> _void))

(define-llvm LLVMGetIntrinsicID
 (_fun LLVMValueRef -> _uint))

(define-llvm LLVMGetFunctionCallConv
 (_fun LLVMValueRef -> LLVMCallConv))

(define-llvm LLVMSetFunctionCallConv
 (_fun LLVMValueRef LLVMCallConv -> _void))

(define-llvm LLVMGetGC
 (_fun LLVMValueRef -> _string))
(define-llvm LLVMSetGC (_fun LLVMValueRef _string -> _void))


(define-llvm-multiple
 (LLVMAddFunctionAttr
  LLVMRemoveFunctionAttr)
 (_fun LLVMValueRef LLVMAttribute -> _void))

(define-llvm LLVMGetFunctionAttr
 (_fun LLVMValueRef -> LLVMAttribute))
 

;/* Operations on parameters */

(define-llvm LLVMCountParams (_fun LLVMValueRef -> _uint))
(define-llvm LLVMGetParams
 (_fun (fun) ::
       (fun : LLVMValueRef)
       (params : (_list o LLVMValueRef (LLVMCountParams fun)))
       -> _void
       -> params))

(define-llvm LLVMGetParam (_fun LLVMValueRef _uint -> LLVMValueRef))

(define-llvm LLVMGetParamParent (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetFirstParam
  LLVMGetLastParam)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetNextParam
  LLVMGetPreviousParam)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMAddAttribute
  LLVMRemoveAttribute)
 (_fun LLVMValueRef LLVMAttribute -> _void))

(define-llvm LLVMGetAttribute (_fun LLVMValueRef -> LLVMAttribute))
(define-llvm LLVMSetParamAlignment (_fun LLVMValueRef _uint -> _void))


