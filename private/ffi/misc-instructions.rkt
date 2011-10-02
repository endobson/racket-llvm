#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Miscellaneous instructions */

(define-llvm-unsafe LLVMBuildPhi (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))


(define-llvm-safe LLVMBuildPhi
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMTypeRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))


(define-llvm-unsafe LLVMBuildCall
 (_fun (builder fun args name) ::
       (builder : LLVMBuilderRef)
       (fun : LLVMValueRef)
       (args : (_list i LLVMValueRef))
       (_uint = (length args))
       (name : _string)
       -> LLVMValueRef))


(define-llvm-safe LLVMBuildCall
  (_fun (builder fun args name) ::
        (builder : safe:LLVMBuilderRef)
        (fun : safe:LLVMValueRef)
        (args : (_list i safe:LLVMValueRef))
        (_uint = (length args))
        (name : _non-null-string) ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))




(define-llvm-unsafe LLVMBuildSelect
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildVAArg
 (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildExtractElement
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm-unsafe LLVMBuildInsertElement
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))


(define-llvm-safe LLVMBuildExtractElement
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define-llvm-safe LLVMBuildInsertElement
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef
        safe:LLVMValueRef
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))



(define-llvm-unsafe LLVMBuildShuffleVector
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))


(define-llvm-unsafe LLVMBuildExtractValue
 (_fun LLVMBuilderRef LLVMValueRef _uint  _string -> LLVMValueRef))
(define-llvm-unsafe LLVMBuildInsertValue
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _uint  _string -> LLVMValueRef))
(define-llvm-multiple-unsafe
 (LLVMBuildIsNull
  LLVMBuildIsNotNull)
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildPtrDiff (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

