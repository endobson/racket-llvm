#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (except-out (all-defined-out) safe:type-at-index))


;Racket added functions
;

(define-llvm-racket-unsafe LLVMGetModuleDescription unsafe:LLVMModuleDescriptionMaker)
(define-llvm-racket-unsafe LLVMIsValidTypeIndex (_fun LLVMTypeRef LLVMValueRef -> LLVMBool))
(define-llvm-racket-unsafe LLVMGetTypeAtIndex (_fun LLVMTypeRef LLVMValueRef -> LLVMTypeRef))
(define-llvm-racket-unsafe LLVMIsTerminatorInstruction (_fun LLVMValueRef -> LLVMBool))


(define safe:type-at-index
  (_fun (ty : safe:LLVMTypeRef)
        safe:LLVMValueRef ->
        (ptr : _pointer) ->
        (safe:llvm-type-ref ptr (safe:llvm-type-ref-context ty))))

(define safe:is-valid-type-index
  (_fun safe:LLVMTypeRef
        safe:LLVMValueRef ->
        LLVMBool))



(define-llvm-racket-safe LLVMGetTypeAtIndex safe:type-at-index)
(define-llvm-racket-safe LLVMIsValidTypeIndex safe:is-valid-type-index)
(define-llvm-racket-safe LLVMIsTerminatorInstruction (_fun safe:LLVMValueRef -> LLVMBool))
(define-llvm-racket-safe LLVMGetModuleDescription safe:LLVMModuleDescriptionMaker)

(define-llvm-racket-safe LLVMOptimizeModule (_fun safe:LLVMModuleRef -> LLVMBool))

