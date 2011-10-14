#lang racket/base

(require
  racket/contract
  "../ffi/safe.rkt"
  "../safe/structs.rkt"
  "parameters.rkt"
  "functions.rkt")


(provide
 (contract-out
  (llvm-create-builder
    (->* () (#:context llvm-context-ref?) llvm-builder-ref?))

  
  (llvm-set-position
    (->* (llvm-basic-block-ref?) (#:builder llvm-builder-ref?) void?))
  (llvm-get-insert-block
    (->* () (#:builder llvm-builder-ref?) llvm-basic-block-ref?))
  (llvm-add-block
    (->* ()
         (#:context llvm-context-ref?
          #:builder llvm-builder-ref?
          #:name string?)
         llvm-basic-block-ref?))

  (llvm-add-block-to-function
    (->* (llvm:function-pointer?)
         (#:context llvm-context-ref?
          #:name string?)
         llvm-basic-block-ref?))
  (builder->function (-> llvm-builder-ref? llvm:function-pointer?))


  ))


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


