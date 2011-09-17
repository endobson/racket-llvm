#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Terminators */
(define-llvm-unsafe LLVMBuildRetVoid (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm-unsafe LLVMBuildRet (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))

(define-llvm-safe LLVMBuildRetVoid
  (_fun (builder : safe:LLVMBuilderRef) ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))
        
(define-llvm-safe LLVMBuildRet
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))


(define-llvm-unsafe LLVMBuildAggregateRet
  (_fun (builder vals) ::
        (builder : LLVMBuilderRef)
        (vals : (_list i LLVMValueRef))
        (_uint = (length vals))
        -> LLVMValueRef))


(define-llvm-unsafe LLVMBuildBr (_fun LLVMBuilderRef LLVMBasicBlockRef -> LLVMValueRef))

(define-llvm-safe LLVMBuildBr 
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMBasicBlockRef ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define-llvm-unsafe LLVMBuildCondBr
 (_fun LLVMBuilderRef
       LLVMValueRef
       LLVMBasicBlockRef
       LLVMBasicBlockRef -> LLVMValueRef))


(define-llvm-safe LLVMBuildCondBr
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef
        safe:LLVMBasicBlockRef
        safe:LLVMBasicBlockRef ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))




(define-llvm-unsafe LLVMBuildSwitch
 (_fun LLVMBuilderRef LLVMValueRef LLVMBasicBlockRef _uint -> LLVMValueRef))

(define-llvm-unsafe LLVMAddCase (_fun LLVMValueRef LLVMValueRef LLVMBasicBlockRef -> _void))

(define-llvm-unsafe LLVMBuildIndirectBr (_fun LLVMBuilderRef LLVMValueRef _uint -> LLVMValueRef))
(define-llvm-unsafe LLVMBuildInvoke
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

(define-llvm-unsafe LLVMBuildLandingPad
 (_fun LLVMBuilderRef
       LLVMTypeRef
       LLVMValueRef
       _uint
       _string
       -> _void))
(define-llvm-unsafe LLVMBuildResume
 (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildUnreachable
 (_fun LLVMBuilderRef -> LLVMValueRef))

;/* Add a destination to the indirectbr instruction */
(define-llvm-unsafe LLVMAddDestination (_fun LLVMValueRef LLVMBasicBlockRef -> _void))

(define-llvm-unsafe LLVMAddClause (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm-unsafe LLVMSetCleanup (_fun LLVMValueRef LLVMBool -> _void))
