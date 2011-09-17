#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

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

(define-llvm-unsafe LLVMBuildCast
 (_fun LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
