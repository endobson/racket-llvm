#lang racket/base



(require "../ffi/safe.rkt"
  "convertible.rkt" "values.rkt"
  "builder.rkt"
  "misc-instructions.rkt"
  "../safe/structs.rkt" "parameters.rkt")
(require racket/contract)

(provide
 (contract-out
  (llvm:sqrt (->* (llvm-float/c)
                  (#:builder llvm-builder-ref?
                   #:name string?)
                  llvm-float/c))))



(define (sqrt-intrinsic module ty)
  (LLVMGetIntrinsic module 'sqrt (list ty)))

(define (llvm:sqrt v #:builder (builder (current-builder)) #:name (name ""))
  (let* ((v (value->llvm v))
         (ty (llvm-type-of v)))
    (llvm-call (sqrt-intrinsic (builder->module builder) ty)
               v #:builder builder #:name name)))

