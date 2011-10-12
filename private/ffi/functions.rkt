#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))


;/* Operations on functions */
(define-llvm-unsafe LLVMAddFunction (_fun LLVMModuleRef _string LLVMTypeRef -> LLVMValueRef))

(define-llvm-safe LLVMAddFunction
  (_fun (mod : safe:LLVMModuleRef)
        _non-null-string
        safe:LLVMTypeRef ->
        (ptr : _pointer) -> 
        (safe:llvm-value-ref ptr mod)))

(define-llvm-unsafe LLVMGetNamedFunction (_fun LLVMModuleRef _string -> LLVMValueRef))

(define-llvm-safe LLVMGetNamedFunction
  (_fun (mod : safe:LLVMModuleRef)
        _non-null-string ->
        (ptr : _pointer) -> 
        (and ptr
          (safe:llvm-value-ref ptr mod))))


(define-llvm-multiple-unsafe
 (LLVMGetFirstFunction
  LLVMGetLastFunction)
 (_fun LLVMModuleRef -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMGetNextFunction
  LLVMGetPreviousFunction)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-unsafe LLVMDeleteFunction
 (_fun LLVMValueRef -> _void))

(define-llvm-unsafe LLVMGetIntrinsicID
 (_fun LLVMValueRef -> _uint))

(define-llvm-unsafe LLVMGetFunctionCallConv
 (_fun LLVMValueRef -> LLVMCallConv))

(define-llvm-unsafe LLVMSetFunctionCallConv
 (_fun LLVMValueRef LLVMCallConv -> _void))

(define-llvm-safe LLVMSetFunctionCallConv
 (_fun safe:LLVMValueRef LLVMCallConv -> _void))


(define-llvm-unsafe LLVMGetGC
 (_fun LLVMValueRef -> _string))
(define-llvm-unsafe LLVMSetGC (_fun LLVMValueRef _string -> _void))


(define-llvm-multiple-unsafe
 (LLVMAddFunctionAttr
  LLVMRemoveFunctionAttr)
 (_fun LLVMValueRef LLVMAttribute -> _void))

(define-llvm-multiple-safe
 (LLVMAddFunctionAttr
  LLVMRemoveFunctionAttr)
 (_fun safe:LLVMValueRef LLVMAttribute -> _void))


(define-llvm-unsafe LLVMGetFunctionAttr
 (_fun LLVMValueRef -> LLVMAttribute))
 

;/* Operations on parameters */

(define-llvm-unsafe LLVMCountParams (_fun LLVMValueRef -> _uint))
(define-llvm-unsafe LLVMGetParams
 (_fun (fun) ::
       (fun : LLVMValueRef)
       (params : (_list o LLVMValueRef (unsafe:LLVMCountParams fun)))
       -> _void
       -> params))

(define-llvm-unsafe LLVMGetParam (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm-safe LLVMGetParam
  (_fun (f : safe:LLVMValueRef)
        _uint ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-value-ref-owner f))))

(define-llvm-unsafe LLVMGetParamParent (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMGetFirstParam
  LLVMGetLastParam)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMGetNextParam
  LLVMGetPreviousParam)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMAddAttribute
  LLVMRemoveAttribute)
 (_fun LLVMValueRef LLVMAttribute -> _void))

(define-llvm-unsafe LLVMGetAttribute (_fun LLVMValueRef -> LLVMAttribute))
(define-llvm-unsafe LLVMSetParamAlignment (_fun LLVMValueRef _uint -> _void))


