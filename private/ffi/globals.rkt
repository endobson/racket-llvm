#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Operations on global variables, functions, and aliases (globals) */
(define-llvm-unsafe LLVMGetGlobalParent (_fun LLVMValueRef -> LLVMModuleRef))

(define-llvm-safe LLVMGetGlobalParent
  (_fun (v : safe:LLVMValueRef) ->
        (ptr : _pointer) ->
        (let ((o (safe:llvm-value-ref-owner v)))
         (if (equal? ptr (safe:llvm-module-ref-pointer o))
             o
             (error 'safe:llvm-value-ref "Parent is incorrect ~a: should be ~a" v ptr)))))
        

(define-llvm-unsafe LLVMIsDeclaration (_fun LLVMValueRef -> LLVMBool))

(define-llvm-unsafe LLVMGetLinkage (_fun LLVMValueRef -> LLVMLinkage))
(define-llvm-unsafe LLVMSetLinkage (_fun LLVMValueRef LLVMLinkage -> _void))

(define-llvm-safe LLVMGetLinkage (_fun safe:LLVMValueRef -> LLVMLinkage))
(define-llvm-safe LLVMSetLinkage (_fun safe:LLVMValueRef LLVMLinkage -> _void))


(define-llvm-unsafe LLVMGetSection (_fun LLVMValueRef -> _string))
(define-llvm-unsafe LLVMSetSection (_fun LLVMValueRef _string -> _void))

(define-llvm-safe LLVMGetSection (_fun safe:LLVMValueRef -> _string))
(define-llvm-safe LLVMSetSection (_fun safe:LLVMValueRef _string -> _void))

(define-llvm-unsafe LLVMGetVisibility (_fun LLVMValueRef -> LLVMVisibility))
(define-llvm-unsafe LLVMSetVisibility (_fun LLVMValueRef LLVMVisibility -> _void))

(define-llvm-safe LLVMGetVisibility (_fun safe:LLVMValueRef -> LLVMVisibility))
(define-llvm-safe LLVMSetVisibility (_fun safe:LLVMValueRef LLVMVisibility -> _void))

(define-llvm-unsafe LLVMGetAlignment (_fun LLVMValueRef -> _uint))
(define-llvm-unsafe LLVMSetAlignment (_fun LLVMValueRef _uint -> _void))

(define-llvm-safe LLVMGetAlignment (_fun safe:LLVMValueRef -> _uint))
(define-llvm-safe LLVMSetAlignment (_fun safe:LLVMValueRef _uint -> _void))


;/* Operations on global variables */
(define-llvm-unsafe LLVMAddGlobal
 (_fun LLVMModuleRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm-safe LLVMAddGlobal
  (_fun (mod : safe:LLVMModuleRef)
        safe:LLVMTypeRef
        _non-null-string ->
        (ptr : _pointer) -> 
        (safe:llvm-value-ref ptr mod)))




(define-llvm-unsafe LLVMAddGlobalInAddressSpace
 (_fun LLVMModuleRef LLVMTypeRef _string _uint -> LLVMValueRef))

(define-llvm-unsafe LLVMGetNamedGlobal (_fun LLVMModuleRef _string -> LLVMValueRef))

(define-llvm-safe LLVMGetNamedGlobal
  (_fun (mod : safe:LLVMModuleRef)
        _non-null-string ->
        (ptr : _pointer) -> 
        (and ptr
          (safe:llvm-value-ref ptr mod))))

(define-llvm-multiple-unsafe
 (LLVMGetFirstGlobal LLVMGetLastGlobal)
 (_fun LLVMModuleRef -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMGetNextGlobal
  LLVMGetPreviousGlobal
  LLVMDeleteGlobal
  LLVMGetInitializer)
 (_fun LLVMValueRef -> LLVMValueRef))


(define-llvm-multiple-safe
 (LLVMGetNextGlobal
  LLVMGetPreviousGlobal
  LLVMGetInitializer)
 (_fun safe:LLVMValueRef -> safe:LLVMValueRef))


(define-llvm-unsafe LLVMSetInitializer (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm-safe LLVMSetInitializer (_fun safe:LLVMValueRef safe:LLVMValueRef -> _void))

(define-llvm-multiple-unsafe
 (LLVMIsThreadLocal LLVMIsGlobalConstant)
 (_fun LLVMValueRef -> LLVMBool))
(define-llvm-multiple-unsafe
 (LLVMSetThreadLocal LLVMSetGlobalConstant)
 (_fun LLVMValueRef LLVMBool -> _void))


(define-llvm-multiple-safe
 (LLVMIsThreadLocal LLVMIsGlobalConstant)
 (_fun safe:LLVMValueRef -> LLVMBool))
(define-llvm-multiple-safe
 (LLVMSetThreadLocal LLVMSetGlobalConstant)
 (_fun safe:LLVMValueRef LLVMBool -> _void))



;/* Operations on aliases */

(define-llvm-unsafe LLVMAddAlias
 (_fun LLVMModuleRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))
