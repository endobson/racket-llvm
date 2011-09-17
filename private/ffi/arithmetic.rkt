#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide
 (except-out (all-defined-out)
   safe:binop
   safe:uniop
   safe:icmp))

;TODO differentiate types and ensure that types match,
;and contexts match
(define safe:binop
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:uniop
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:icmp
  (_fun (builder : safe:LLVMBuilderRef)
        LLVMIntPredicate
        safe:LLVMValueRef
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))






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


(define-llvm-multiple-safe
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
  safe:binop)


(define-llvm-unsafe LLVMBuildBinOp
 (_fun LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMBuildNeg
  LLVMBuildNSWNeg
  LLVMBuildNUWNeg
  LLVMBuildFNeg
  LLVMBuildNot)
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))


(define-llvm-multiple-safe
 (LLVMBuildNeg
  LLVMBuildNSWNeg
  LLVMBuildNUWNeg
  LLVMBuildFNeg
  LLVMBuildNot)
 safe:uniop)



;/* Comparisons */
(define-llvm-unsafe LLVMBuildICmp
  (_fun LLVMBuilderRef
        LLVMIntPredicate
        LLVMValueRef
        LLVMValueRef
        _string -> LLVMValueRef))


(define-llvm-safe LLVMBuildICmp safe:icmp)

(define-llvm-unsafe LLVMBuildFCmp
  (_fun LLVMBuilderRef
        LLVMRealPredicate
        LLVMValueRef
        LLVMValueRef
        _string -> LLVMValueRef))

