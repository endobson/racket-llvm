#lang racket/base



(require "base.rkt"
         "../ffi/safe.rkt")

;TODO add contracts
(provide llvm-array-type
         llvm-vector-type
         llvm-struct-type
         llvm-struct-type*
         llvm-named-struct-type)

(define (llvm-array-type type (size 0))
 (LLVMArrayType type size))

(define (llvm-vector-type type size)
 (LLVMVectorType type size))


(define (llvm-struct-type #:context (context (current-context)) #:packed (packed #f) . types)
 (LLVMStructTypeInContext context types packed))

(define (llvm-struct-type* #:context (context (current-context)) #:packed (packed #f) . types)
 (LLVMStructTypeInContext context (apply list* types) packed))

(define (llvm-named-struct-type (name "") #:context (context (current-context)))
 (LLVMStructCreateNamed context name))

