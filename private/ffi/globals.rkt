#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Operations on global variables, functions, and aliases (globals) */
(define-llvm LLVMGetGlobalParent (_fun LLVMValueRef -> LLVMModuleRef))

(define-llvm LLVMIsDeclaration (_fun LLVMValueRef -> LLVMBool))

(define-llvm LLVMGetLinkage (_fun LLVMValueRef -> LLVMLinkage))
(define-llvm LLVMSetLinkage (_fun LLVMValueRef LLVMLinkage -> _void))

(define-llvm LLVMGetSection (_fun LLVMValueRef -> _string))
(define-llvm LLVMSetSection (_fun LLVMValueRef _string -> _void))

(define-llvm LLVMGetVisibility (_fun LLVMValueRef -> LLVMVisibility))
(define-llvm LLVMSetVisibility (_fun LLVMValueRef LLVMVisibility -> _void))

(define-llvm LLVMGetAlignment (_fun LLVMValueRef -> _uint))
(define-llvm LLVMSetAlignment (_fun LLVMValueRef _uint -> _void))

;/* Operations on global variables */
(define-llvm LLVMAddGlobal
 (_fun LLVMModuleRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm LLVMAddGlobalInAddressSpace
 (_fun LLVMModuleRef LLVMTypeRef _string _uint -> LLVMValueRef))

(define-llvm LLVMGetNamedGlobal (_fun LLVMModuleRef _string -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetFirstGlobal LLVMGetLastGlobal)
 (_fun LLVMModuleRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetNextGlobal
  LLVMGetPreviousGlobal
  LLVMDeleteGlobal
  LLVMGetInitializer)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMSetInitializer (_fun LLVMValueRef LLVMValueRef -> _void))

(define-llvm-multiple
 (LLVMIsThreadLocal LLVMIsGlobalConstant)
 (_fun LLVMValueRef -> LLVMBool))
(define-llvm-multiple
 (LLVMSetThreadLocal LLVMSetGlobalConstant)
 (_fun LLVMValueRef LLVMBool -> _void))



;/* Operations on aliases */

(define-llvm LLVMAddAlias
 (_fun LLVMModuleRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))
