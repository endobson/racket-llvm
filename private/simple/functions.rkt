#lang racket/base

(require
  racket/contract
  unstable/contract
  "../ffi/safe.rkt"
  "parameters.rkt"
  "predicates.rkt"
  "convertible.rkt"
  "types.rkt")

(provide
 (contract-out
  (llvm:function-pointer? predicate/c)

  (llvm-add-function
    (->* (llvm:function-type? string?)
         (#:module llvm:module?) llvm:value?))

  (llvm-get-named-function
    (->* (string?) (#:module llvm:module?) llvm:value?))))


(define (llvm:function-pointer? v)
 (and (llvm:value? v)
  (let ((type (value->llvm-type v)))
    (and (llvm:pointer-type? type)
         (llvm:function-type? (llvm-get-element-type type))))))


(define (llvm-add-function type name #:module (module (current-module)))
 (LLVMAddFunction module name type))

(define (llvm-get-named-function name #:module (module (current-module)))
 (LLVMGetNamedFunction module name))

