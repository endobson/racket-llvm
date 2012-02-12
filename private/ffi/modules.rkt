#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))



;/*===-- Contexts ----------------------------------------------------------===*/


;/* Create and destroy contexts. */
(define-llvm LLVMContextCreate
  #:unsafe (_fun -> LLVMContextRef)
  #:safe safe:LLVMContextCreator)


(define-llvm-unsafe LLVMGetGlobalContext (_fun -> LLVMContextRef))
(define-llvm-unsafe LLVMContextDispose (_fun LLVMContextRef -> _void))
;TODO why have this function
(define (safe:LLVMContextDispose ctx) (void))

(define-llvm LLVMGetMDKindIDInContext
  #:both (_fun LLVMContextRef _string _uint -> _uint))

(define-llvm-unsafe LLVMGetMDKindID (_fun _string _uint -> _uint))


;/*===-- Modules -----------------------------------------------------------===*/

;/* Create and destroy modules. */ 
;/** See llvm::Module::Module. */
(define-llvm-unsafe LLVMModuleCreateWithName (_fun _string -> LLVMModuleRef))
(define-llvm LLVMModuleCreateWithNameInContext
  #:unsafe (_fun _string LLVMContextRef -> LLVMModuleRef)
  #:safe safe:LLVMModuleCreator)


;/** See llvm::Module::~Module. */
(define-llvm-unsafe LLVMDisposeModule (_fun LLVMModuleRef -> _void))
;TODO why have this function
(define (safe:LLVMDisposeModule module) (void))


;/** Data layout. See Module::getDataLayout. */
(define-llvm-unsafe LLVMGetDataLayout (_fun LLVMModuleRef -> _string))
(define-llvm-unsafe LLVMSetDataLayout (_fun LLVMModuleRef _string -> _void))

;/** Target triple. See Module::getTargetTriple. */
(define-llvm LLVMGetTarget
  #:both (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetTarget
  #:both (_fun LLVMModuleRef _string -> _void))

;/** See Module::dump. */
(define-llvm-unsafe LLVMDumpModule (_fun LLVMModuleRef -> _void))

;/** See Module::setModuleInlineAsm. */
(define-llvm-unsafe LLVMSetModuleInlineAsm (_fun LLVMModuleRef _string -> _void))

;/** See Module::getContext. */
(define-llvm-unsafe LLVMGetModuleContext (_fun LLVMModuleRef -> LLVMContextRef))



