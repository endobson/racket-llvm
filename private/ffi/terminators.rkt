#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Terminators */
(define-llvm LLVMBuildRetVoid (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm LLVMBuildRet (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMBuildAggregateRet
  (_fun (builder vals) ::
        (builder : LLVMBuilderRef)
        (vals : (_list i LLVMValueRef))
        (_uint = (length vals))
        -> LLVMValueRef))


(define-llvm LLVMBuildBr (_fun LLVMBuilderRef LLVMBasicBlockRef -> LLVMValueRef))


(define-llvm LLVMBuildCondBr
 (_fun LLVMBuilderRef
       LLVMValueRef
       LLVMBasicBlockRef
       LLVMBasicBlockRef -> LLVMValueRef))

(define-llvm LLVMBuildSwitch
 (_fun LLVMBuilderRef LLVMValueRef LLVMBasicBlockRef _uint -> LLVMValueRef))

(define-llvm LLVMAddCase (_fun LLVMValueRef LLVMValueRef LLVMBasicBlockRef -> _void))

(define-llvm LLVMBuildIndirectBr (_fun LLVMBuilderRef LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMBuildInvoke
 (_fun (builder fun args then catch name) ::
       (builder : LLVMBuilderRef)
       (fun : LLVMValueRef)
       (args : (_list i LLVMValueRef))
       (_uint = (length args))
       (then : LLVMBasicBlockRef)
       (catch : LLVMBasicBlockRef)
       (name : _string)
       ->
       LLVMValueRef))

(define-llvm LLVMBuildLandingPad
 (_fun LLVMBuilderRef
       LLVMTypeRef
       LLVMValueRef
       _uint
       _string
       -> _void))
(define-llvm LLVMBuildResume
 (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMBuildUnreachable
 (_fun LLVMBuilderRef -> LLVMValueRef))

;/* Add a destination to the indirectbr instruction */
(define-llvm LLVMAddDestination (_fun LLVMValueRef LLVMBasicBlockRef -> _void))

(define-llvm LLVMAddClause (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm LLVMSetCleanup (_fun LLVMValueRef LLVMBool -> _void))
