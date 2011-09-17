#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)


;TODO differentiate types and ensure that types match,
;and contexts match
(define safe:icast
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef
        safe:LLVMTypeRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:pcast safe:icast)



(provide (except-out (all-defined-out) safe:pcast safe:icast))

(define-llvm-multiple-unsafe
 (LLVMBuildTrunc
  LLVMBuildZExt
  LLVMBuildSExt
  LLVMBuildFPToUI
  LLVMBuildFPToSI
  LLVMBuildUIToFP
  LLVMBuildSIToFP
  LLVMBuildFPTrunc
  LLVMBuildFPExt)
 (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMBuildPtrToInt
  LLVMBuildIntToPtr
  LLVMBuildBitCast
  LLVMBuildZExtOrBitCast
  LLVMBuildSExtOrBitCast
  LLVMBuildTruncOrBitCast
  LLVMBuildPointerCast
  LLVMBuildIntCast
  LLVMBuildFPCast)
 (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))


(define-llvm-multiple-safe
 (LLVMBuildTrunc
  LLVMBuildZExt
  LLVMBuildSExt
  LLVMBuildFPToUI
  LLVMBuildFPToSI
  LLVMBuildUIToFP
  LLVMBuildSIToFP
  LLVMBuildFPTrunc
  LLVMBuildFPExt)
 safe:icast)

(define-llvm-multiple-safe
 (LLVMBuildPtrToInt
  LLVMBuildIntToPtr
  LLVMBuildBitCast
  LLVMBuildZExtOrBitCast
  LLVMBuildSExtOrBitCast
  LLVMBuildTruncOrBitCast
  LLVMBuildPointerCast
  LLVMBuildIntCast
  LLVMBuildFPCast)
 safe:pcast)


(define-llvm-unsafe LLVMBuildCast
 (_fun LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
