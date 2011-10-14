#lang racket/base



(require "../ffi/safe.rkt"
  "convertible.rkt" "values.rkt"
  "../safe/structs.rkt" "parameters.rkt")
(require racket/contract)

(provide
 (contract-out
  (llvm:sqrt (->* (llvm-float/c)
                  (#:builder llvm-builder-ref?
                   #:name string?)
                  llvm-float/c))))

;TODO remove to common code
(define (llvm-call function #:builder (builder (current-builder)) #:name (name "") . arguments)
 (LLVMBuildCall builder function (map value->llvm arguments) name))

(define (builder->function builder)
  (LLVMGetBasicBlockParent (LLVMGetInsertBlock builder)))

(define (builder->module builder)
  (LLVMGetGlobalParent (builder->function builder)))



(define (sqrt-intrinsic module ty)
  (LLVMGetIntrinsic module 'sqrt (list ty)))

(define (llvm:sqrt v #:builder (builder (current-builder)) #:name (name ""))
  (let* ((v (value->llvm v))
         (ty (llvm-type-of v)))
    (llvm-call (sqrt-intrinsic (builder->module builder) ty)
               v #:builder builder #:name name)))

