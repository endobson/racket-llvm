#lang racket/base

(require
  racket/contract
  racket/list
  "../ffi/safe.rkt"
  "predicates.rkt"
  "convertible.rkt"
  "types.rkt")

(provide
 (contract-out
  (llvm-valid-gep-indices? (-> llvm:type? (listof llvm-integer/c) boolean?))
  (llvm-gep-type
    (->i ((type llvm:type?)
          (indices (listof llvm-integer/c)))
         #:pre (type indices)
          (llvm-valid-gep-indices? type indices)
         (_ llvm:type?)))
 
 
  (llvm-is-valid-type-index
   (-> llvm:pointer-type?
       llvm-integer/c
       boolean?))
 
  (llvm-get-type-at-index
   (->i ((type llvm:pointer-type?)
         (index llvm-integer/c))
        #:pre (type index)
         (llvm-is-valid-type-index type index)
        (_ llvm:type?))))) 




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

