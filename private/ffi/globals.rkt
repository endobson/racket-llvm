#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Operations on global variables, functions, and aliases (globals) */
(define-llvm-unsafe LLVMGetGlobalParent (_fun LLVMValueRef -> LLVMModuleRef))

(define-llvm-unsafe LLVMIsDeclaration (_fun LLVMValueRef -> LLVMBool))

(define-llvm-unsafe LLVMGetLinkage (_fun LLVMValueRef -> LLVMLinkage))
(define-llvm-unsafe LLVMSetLinkage (_fun LLVMValueRef LLVMLinkage -> _void))

(define-llvm-unsafe LLVMGetSection (_fun LLVMValueRef -> _string))
(define-llvm-unsafe LLVMSetSection (_fun LLVMValueRef _string -> _void))

(define-llvm-unsafe LLVMGetVisibility (_fun LLVMValueRef -> LLVMVisibility))
(define-llvm-unsafe LLVMSetVisibility (_fun LLVMValueRef LLVMVisibility -> _void))

(define-llvm-unsafe LLVMGetAlignment (_fun LLVMValueRef -> _uint))
(define-llvm-unsafe LLVMSetAlignment (_fun LLVMValueRef _uint -> _void))

;/* Operations on global variables */
(define-llvm-unsafe LLVMAddGlobal
 (_fun LLVMModuleRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMAddGlobalInAddressSpace
 (_fun LLVMModuleRef LLVMTypeRef _string _uint -> LLVMValueRef))

(define-llvm-unsafe LLVMGetNamedGlobal (_fun LLVMModuleRef _string -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMGetFirstGlobal LLVMGetLastGlobal)
 (_fun LLVMModuleRef -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMGetNextGlobal
  LLVMGetPreviousGlobal
  LLVMDeleteGlobal
  LLVMGetInitializer)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-unsafe LLVMSetInitializer (_fun LLVMValueRef LLVMValueRef -> _void))

(define-llvm-multiple-unsafe
 (LLVMIsThreadLocal LLVMIsGlobalConstant)
 (_fun LLVMValueRef -> LLVMBool))
(define-llvm-multiple-unsafe
 (LLVMSetThreadLocal LLVMSetGlobalConstant)
 (_fun LLVMValueRef LLVMBool -> _void))



;/* Operations on aliases */

(define-llvm-unsafe LLVMAddAlias
 (_fun LLVMModuleRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))
