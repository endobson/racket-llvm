#lang racket/base

(require
  racket/list
  racket/contract
  "../ffi/safe.rkt"
  "util.rkt"
  "parameters.rkt"
  "predicates.rkt"
  "builder.rkt"
  "convertible.rkt")


(provide
 (contract-out
  (llvm-ret      (->* () (llvm-value/c #:builder llvm:builder?) llvm:value?))
  (llvm-cond-br (->* (llvm-boolean/c llvm:basic-block? llvm:basic-block?)
                     (#:builder llvm:builder?)
                     llvm:value?))
  (llvm-br (->* (llvm:basic-block?)
                     (#:builder llvm:builder?)
                     llvm:value?))

  (llvm-phi (->* (llvm:type?)
                   (#:builder llvm:builder?
                    #:name string?)
                   llvm:value?))

  (llvm-add-incoming
    (->* (llvm:value?)
         ()
         #:rest (listof (or/c (cons/c llvm-value/c llvm:basic-block?) llvm:value?))
         void?))


  (llvm-get-param (->*  (integer?) (#:function llvm:value?) llvm:value?))
  (llvm-call (->* (llvm:value?)  (#:builder llvm:builder? #:name string?) #:rest (listof llvm-value/c) llvm:value?))
  (llvm-call* (->* (llvm:value?)  (#:builder llvm:builder? #:name string?) #:rest (list*/c llvm-value/c) llvm:value?))))



(define (llvm-get-param index #:function (function (builder->function (current-builder))))
 (LLVMGetParam function index))

(define (llvm-ret (val (void)) #:builder (builder (current-builder)))
  (if (void? val)
    (LLVMBuildRetVoid builder)
    (LLVMBuildRet builder (value->llvm val))))



(define (llvm-cond-br cond true-block false-block #:builder (builder (current-builder)))
 (LLVMBuildCondBr builder (boolean->llvm cond) true-block false-block))

(define (llvm-br block #:builder (builder (current-builder)))
 (LLVMBuildBr builder block))


(define (llvm-phi type #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildPhi builder type name))

;TODO change the API on this
(define (llvm-add-incoming phi . input-values)
 (define (extract-value v)
  (if (cons? v) (value->llvm (car v)) v))
 (define (extract-block v)
  (if (cons? v) (cdr v)  (LLVMGetInstructionParent v)))

 (define values (map extract-value input-values))
 (define blocks (map extract-block input-values))
  
 (LLVMAddIncoming phi values blocks))


(define (llvm-call function #:builder (builder (current-builder)) #:name (name "") . arguments)
 (LLVMBuildCall builder function (map value->llvm arguments) name))

(define (llvm-call* function #:builder (builder (current-builder)) #:name (name "") . arguments)
 (LLVMBuildCall builder function (map value->llvm (apply list* arguments)) name))

