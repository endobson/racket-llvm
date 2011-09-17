#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (except-out (all-defined-out)
 safe:alloc safe:array-alloc safe:free
 safe:load safe:store
 safe:gep safe:struct-gep safe:string-builder))


(define safe:alloc
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMTypeRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:array-alloc
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMTypeRef
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))


(define safe:free
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:load
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))


(define safe:store
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef
        safe:LLVMValueRef ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:gep
  (_fun (builder ptr indices name) ::
        (builder : safe:LLVMBuilderRef)
        (ptr : safe:LLVMValueRef)
        (indices : (_list i safe:LLVMValueRef))
        (_uint = (length indices))
        (name : _non-null-string) ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:struct-gep
  (_fun (builder : safe:LLVMBuilderRef)
        (ptr : safe:LLVMValueRef)
        (index : _uint)
        (name : _non-null-string) ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:string-builder
  (_fun (builder : safe:LLVMBuilderRef)
        (val : _string)
        (name : _non-null-string) ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))


;/* Memory */
(define-llvm-multiple-unsafe
 (LLVMBuildMalloc
  LLVMBuildAlloca)
 (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm-multiple-unsafe
 (LLVMBuildArrayMalloc
  LLVMBuildArrayAlloca)
 (_fun LLVMBuilderRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildFree (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))

(define-llvm-unsafe LLVMBuildLoad
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm-unsafe LLVMBuildStore
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMBuildGEP
  LLVMBuildInBoundsGEP)
 (_fun (builder ptr indices name) ::
       (builder : LLVMBuilderRef)
       (ptr : LLVMValueRef)
       (indices : (_list i LLVMValueRef))
       (_uint = (length indices))
       (name : _string)
       -> LLVMValueRef))


(define-llvm-unsafe LLVMBuildStructGEP
 (_fun LLVMBuilderRef LLVMValueRef _uint _string -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMBuildGlobalString LLVMBuildGlobalStringPtr)
 (_fun LLVMBuilderRef _string _string -> LLVMValueRef))


(define-llvm-multiple-safe
 (LLVMBuildMalloc LLVMBuildAlloca)
 safe:alloc)

(define-llvm-multiple-safe (LLVMBuildArrayMalloc LLVMBuildArrayAlloca) safe:array-alloc)

(define-llvm-safe LLVMBuildFree safe:free)

(define-llvm-safe LLVMBuildLoad safe:load)
(define-llvm-safe LLVMBuildStore safe:store)

(define-llvm-multiple-safe (LLVMBuildGEP LLVMBuildInBoundsGEP) safe:gep)


(define-llvm-safe LLVMBuildStructGEP safe:struct-gep)

(define-llvm-multiple-safe
 (LLVMBuildGlobalString LLVMBuildGlobalStringPtr)
 safe:string-builder)

