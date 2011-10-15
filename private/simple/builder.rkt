#lang racket/base

(require
  racket/contract
  "../ffi/safe.rkt"
  "parameters.rkt"
  "predicates.rkt"
  "functions.rkt")


(provide
 (contract-out
  (llvm-create-builder
    (->* () (#:context llvm:context?) llvm:builder?))

  
  (llvm-set-position
    (->* (llvm:basic-block?) (#:builder llvm:builder?) void?))
  (llvm-get-insert-block
    (->* () (#:builder llvm:builder?) llvm:basic-block?))
  (llvm-add-block
    (->* ()
         (#:context llvm:context?
          #:builder llvm:builder?
          #:name string?)
         llvm:basic-block?))

  (llvm-add-block-to-function
    (->* (llvm:function-pointer?)
         (#:context llvm:context?
          #:name string?)
         llvm:basic-block?))
  (builder->function (-> llvm:builder? llvm:function-pointer?))
  (builder->module (-> llvm:builder? llvm:module?))))


(define (llvm-create-builder #:context (context (current-context)))
 (LLVMCreateBuilderInContext context))

(define (llvm-add-block-to-function function
           #:context (context (current-context))
           #:name    (name ""))
 (LLVMAppendBasicBlockInContext context function name))


(define (builder->function builder)
  (LLVMGetBasicBlockParent (LLVMGetInsertBlock builder)))

(define (builder->module builder)
  (LLVMGetGlobalParent (builder->function builder)))


(define (llvm-get-insert-block #:builder (builder (current-builder)))
 (LLVMGetInsertBlock builder))


(define (llvm-set-position block #:builder (builder (current-builder)))
 (LLVMPositionBuilderAtEnd builder block))

(define (llvm-add-block
           #:context (context (current-context))
           #:builder (builder (current-builder))
           #:name    (name ""))
 (LLVMAppendBasicBlockInContext context (builder->function builder) name))


