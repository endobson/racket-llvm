#lang racket/base

(require
  racket/contract
  unstable/contract
  "../ffi/safe.rkt"
  "../safe/structs.rkt"
  "parameters.rkt"
  "types.rkt"
  "values.rkt")


(provide
 (contract-out
  (llvm:function-pointer? predicate/c)

  (llvm-add-function
    (->* (llvm-function-type-ref? string?)
         (#:module llvm-module-ref?) llvm-value-ref?))

  (llvm-get-named-function
    (->* (string?) (#:module llvm-module-ref?) llvm-value-ref?))))


(define (llvm:function-pointer? v)
 (and (llvm:value? v)
  (let ((type (llvm-type-of v)))
    (and (llvm-pointer-type-ref? type)
         (llvm-function-type-ref? (llvm-get-element-type type))))))


(define (llvm-add-function type name #:module (module (current-module)))
 (LLVMAddFunction module name type))

(define (llvm-get-named-function name #:module (module (current-module)))
 (LLVMGetNamedFunction module name))

