#lang racket/base

(require
  racket/contract
  unstable/contract
  racket/list
  "../ffi/safe.rkt"
  "../safe/structs.rkt"
  "parameters.rkt"
  "convertible.rkt"
  "values.rkt"
  "types.rkt")

(provide
 (contract-out
  (llvm-valid-gep-indices? (-> llvm-type-ref? (listof llvm-integer/c) boolean?))
  (llvm-gep-type
    (->i ((type llvm-type-ref?)
          (indices (listof llvm-integer/c)))
         #:pre (type indices)
          (llvm-valid-gep-indices? type indices)
         (_ llvm-type-ref?)))
 
  (llvm-type-of (-> llvm:value? llvm-type-ref?))
 
  (llvm-is-valid-type-index
   (-> llvm-pointer-type-ref?
       llvm-integer/c
       boolean?))
 
  (llvm-get-type-at-index
   (->i ((type llvm-pointer-type-ref?)
         (index llvm-integer/c))
        #:pre (type index)
         (llvm-is-valid-type-index type index)
        (_ llvm-type-ref?))))) 



;TODO add contract
(provide value->llvm-type)



(define (llvm-type-of value)
 (LLVMTypeOf value))

(define (llvm-get-type-at-index type idx)
 (LLVMGetTypeAtIndex type (value->llvm idx)))

(define (llvm-is-valid-type-index type idx)
 (LLVMIsValidTypeIndex type (value->llvm idx)))

(define (llvm-valid-gep-indices? type indices)
 (let ((type (llvm-gep-type type indices)))
  (if type #t #f)))
      

;First index is checked by contract
(define (llvm-gep-type type indices)
 (and (equal? (llvm-get-type-kind type)
              'LLVMPointerTypeKind)
  (let loop ((type (llvm-get-element-type type)) (indices (rest indices)))
   (or (and (empty? indices) type)
    (let ((kind (llvm-get-type-kind type)))
     (and (memq kind '(LLVMStructTypeKind LLVMArrayTypeKind LLVMVectorTypeKind))
          (llvm-is-valid-type-index type (first indices))
          (loop (llvm-get-type-at-index type (first indices)) (rest indices))))))))





(define (value->llvm-type v)
 (cond
  ((exact-integer? v) (current-integer-type))
  ((real? v) (current-float-type))
  ((boolean? v) (current-boolean-type))
  ((llvm:value? v) (llvm-type-of v))
  (else (error 'value->llvm-type "Unknown input value ~a" v))))


  

