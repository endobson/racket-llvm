#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))


;/*===-- Contexts ----------------------------------------------------------===*/


;/* Create and destroy contexts. */
(define-llvm LLVMContextCreate (_fun -> LLVMContextRef))
(define-llvm-safe LLVMContextCreate safe:LLVMContextCreator)


(define-llvm LLVMGetGlobalContext (_fun -> LLVMContextRef))
(define-llvm LLVMContextDispose (_fun LLVMContextRef -> _void))

(define-llvm LLVMGetMDKindIDInContext (_fun LLVMContextRef _string _uint -> _uint))

(define-llvm LLVMGetMDKindID (_fun _string _uint -> _uint))


;/*===-- Modules -----------------------------------------------------------===*/

;/* Create and destroy modules. */ 
;/** See llvm::Module::Module. */
(define-llvm LLVMModuleCreateWithName (_fun _string -> LLVMModuleRef))
(define-llvm LLVMModuleCreateWithNameInContext (_fun _string LLVMContextRef -> LLVMModuleRef))

(define-llvm-safe LLVMModuleCreateWithNameInContext safe:LLVMModuleCreator)


;/** See llvm::Module::~Module. */
(define-llvm LLVMDisposeModule (_fun LLVMModuleRef -> _void))


;/** Data layout. See Module::getDataLayout. */
(define-llvm LLVMGetDataLayout (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetDataLayout (_fun LLVMModuleRef _string -> _void))

;/** Target triple. See Module::getTargetTriple. */
(define-llvm LLVMGetTarget (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetTarget (_fun LLVMModuleRef _string -> _void))


;/** See Module::dump. */
(define-llvm LLVMDumpModule (_fun LLVMModuleRef -> _void))

;/** See Module::setModuleInlineAsm. */
(define-llvm LLVMSetModuleInlineAsm (_fun LLVMModuleRef _string -> _void))

;/** See Module::getContext. */
(define-llvm LLVMGetModuleContext (_fun LLVMModuleRef -> LLVMContextRef))



