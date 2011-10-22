#lang racket/base



(require
  "../ffi/safe.rkt"
  "convertible.rkt"
  "types.rkt"
  "builder.rkt"
  "misc-instructions.rkt"
  "predicates.rkt"
  "parameters.rkt")
(require racket/contract)

(provide
 (contract-out
  (llvm:sqrt (->* (llvm-float/c)
                  (#:builder llvm:builder?
                   #:name string?)
                  llvm-float/c))))



(define (sqrt-intrinsic module ty)
  (LLVMGetIntrinsic module 'sqrt (list ty)))

(define (llvm:sqrt v #:builder (builder (current-builder)) #:name (name ""))
  (let* ((v (value->llvm v))
         (ty (value->llvm-type v)))
    (llvm-call (sqrt-intrinsic (builder->module builder) ty)
               v #:builder builder #:name name)))

