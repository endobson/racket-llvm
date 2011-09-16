#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))


;Racket added functions
;

(define-llvm-racket LLVMGetModuleDescription unsafe:LLVMModuleDescriptionMaker)
(define-llvm-racket LLVMIsValidTypeIndex (_fun LLVMTypeRef LLVMValueRef -> LLVMBool))
(define-llvm-racket LLVMGetTypeAtIndex (_fun LLVMTypeRef LLVMValueRef -> LLVMTypeRef))
(define-llvm-racket LLVMIsTerminatorInstruction (_fun LLVMValueRef -> LLVMBool))


(define-llvm-racket-safe LLVMGetModuleDescription safe:LLVMModuleDescriptionMaker)
(define-llvm-racket-safe LLVMIsValidTypeIndex (_fun safe:LLVMTypeRef safe:LLVMValueRef -> LLVMBool))
;(define-llvm-racket-safe LLVMGetTypeAtIndex (_fun LLVMTypeRef LLVMValueRef -> LLVMTypeRef))
(define-llvm-racket-safe LLVMIsTerminatorInstruction (_fun safe:LLVMValueRef -> LLVMBool))


