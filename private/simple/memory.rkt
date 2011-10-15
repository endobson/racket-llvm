#lang racket/base

(require racket/contract)

(require
  "../ffi/safe.rkt"
  "parameters.rkt"
  "predicates.rkt"
  "convertible.rkt"
  "indexed-types.rkt"
  "types.rkt")

(define load/c
 (->* (llvm-any-pointer/c)
      (#:builder llvm:builder?
       #:name string?)
      llvm:value?))


(define store/c
 (->i ((value llvm-value/c)
       (pointer llvm-any-pointer/c))
      (#:builder (builder llvm:builder?))
      #:pre (value pointer)
       (equal?
        (llvm-get-element-type (llvm-type-of pointer))
        (value->llvm-type value))
      (_ llvm:value?)))

(define gep/c
 (->i ((pointer llvm:value?))
      (#:builder (builder llvm:builder?)
       #:name (name string?))
      #:rest (args (listof llvm-integer/c))
      #:pre (pointer args)
       (llvm-valid-gep-indices? (llvm-type-of pointer) (map integer->llvm args))
      (_ llvm:value?)))
 
(define alloc/c
  (->* (llvm:type?)
       (#:builder llvm:builder?
        #:name string?)
       llvm:value?))

(define array-alloc/c
  (->* (llvm:type?
        llvm-integer/c)
       (#:builder llvm:builder?
        #:name string?)
       llvm:value?))


(provide/contract
 (llvm-alloca alloc/c)
 (llvm-array-alloca array-alloc/c)
 (llvm-malloc alloc/c)
 (llvm-array-malloc array-alloc/c)
 (llvm-load load/c)
 (llvm-store store/c)
 (llvm-gep gep/c))



(define (llvm-load pointer #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildLoad builder pointer name))

(define (llvm-store value pointer #:builder (builder (current-builder)))
 (LLVMBuildStore builder (value->llvm value) pointer))


(define (llvm-alloca type #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildAlloca builder type name))

(define (llvm-array-alloca type size #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildArrayAlloca builder type (integer->llvm size) name))


(define (llvm-malloc type #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildMalloc builder type name))

(define (llvm-array-malloc type size #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildArrayMalloc builder type (integer->llvm size) name))


(define (llvm-gep pointer #:builder (builder (current-builder)) #:name (name "") . indicies)
 (LLVMBuildGEP builder pointer (map integer->llvm indicies) name))

