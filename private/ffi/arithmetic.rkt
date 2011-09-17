#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Arithmetic */
(define-llvm-multiple-unsafe
  (LLVMBuildAdd
   LLVMBuildNSWAdd
   LLVMBuildNUWAdd
   LLVMBuildFAdd
   LLVMBuildSub
   LLVMBuildNSWSub
   LLVMBuildNUWSub
   LLVMBuildFSub
   LLVMBuildMul
   LLVMBuildNSWMul
   LLVMBuildNUWMul
   LLVMBuildFMul
   LLVMBuildUDiv
   LLVMBuildSDiv
   LLVMBuildExactSDiv
   LLVMBuildFDiv
   LLVMBuildURem
   LLVMBuildSRem
   LLVMBuildFRem
   LLVMBuildShl
   LLVMBuildLShr
   LLVMBuildAShr
   LLVMBuildAnd
   LLVMBuildOr
   LLVMBuildXor)
  (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildBinOp
 (_fun LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMBuildNeg
  LLVMBuildNSWNeg
  LLVMBuildNUWNeg
  LLVMBuildFNeg
  LLVMBuildNot)
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))


;/* Comparisons */
(define-llvm-unsafe LLVMBuildICmp
  (_fun LLVMBuilderRef
        LLVMIntPredicate
        LLVMValueRef
        LLVMValueRef
        _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildFCmp
  (_fun LLVMBuilderRef
        LLVMRealPredicate
        LLVMValueRef
        LLVMValueRef
        _string -> LLVMValueRef))

