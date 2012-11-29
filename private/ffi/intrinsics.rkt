#lang racket/base

(require ffi/unsafe "define.rkt" "enums.rkt" "ctypes.rkt")

(provide safe:LLVMGetIntrinsic)


(define-llvm-racket-unsafe LLVMGetIntrinsic
  (_fun (mod id types) ::
   (mod : LLVMModuleRef)
   (id : LLVMIntrinsic)
   (types : (_list i LLVMTypeRef))
   (_uint = (length types)) ->
   LLVMValueRef))

(define-llvm-racket-safe LLVMGetIntrinsic
  (_fun (mod id types) ::
   (mod : safe:LLVMModuleRef)
   (id : LLVMIntrinsic)
   (types : (_list i safe:LLVMTypeRef))
   (_uint = (length types)) ->
   (ptr : _pointer) ->
   (safe:llvm-value-ref ptr mod)))








