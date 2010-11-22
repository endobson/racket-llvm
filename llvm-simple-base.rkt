#lang racket/base

(require
  racket/contract
  (except-in ffi/unsafe ->) "llvm.rkt")

(provide/contract
 (current-builder         (parameter/c llvm-builder-ref?))
 (current-context         (parameter/c llvm-context-ref?))
 (current-module          (parameter/c llvm-module-ref?))
 (current-integer-type    (parameter/c llvm-type-ref?))

 (llvm-value/c contract?)
 (llvm-any-pointer/c contract?)
 (llvm-current-integer/c contract?)
 (llvm-integer/c contract?)
 (llvm-boolean/c contract?)
 
  


 (llvm-type-of (-> llvm-value-ref? llvm-type-ref?))
 (llvm-get-element-type (-> llvm-type-ref? llvm-type-ref?))

 )

(provide
  llvm-type-equal?
  integer->llvm
  boolean->llvm
  value->llvm
  value->llvm-type)


;Parameters
(define current-builder
 (make-derived-parameter
  (make-parameter #f)
  (lambda (x) x)
  (lambda (builder)
   (or builder
    (error 'current-builder "Current builder was never set")))))


(define current-module
 (make-derived-parameter
  (make-parameter #f)
  (lambda (x) x)
  (lambda (module)
   (or module
    (error 'current-module "Current module was never set")))))


(define current-context
 (make-derived-parameter
  (make-parameter #f)
  (lambda (x) x)
  (lambda (context)
   (or context
    (error 'current-context "Current context was never set")))))

(define current-integer-type
 (make-derived-parameter
  (make-parameter #f)
  (lambda (x) x)
  (lambda (context)
   (or context
    (error 'current-context "Current integer-type was never set")))))

(define (current-boolean-type)
 (LLVMInt1TypeInContext (current-context)))

;Helpers

(define (llvm-get-element-type type)
 (LLVMGetElementType type))

(define (llvm-type-of value)
 (LLVMTypeOf value))

(define (llvm-get-type-kind type)
 (LLVMGetTypeKind type))

(define (llvm-get-int-type-width type)
 (LLVMGetIntTypeWidth type))

(define (llvm-type-equal? t1 t2)
 (ptr-equal?
  (llvm-type-ref-pointer t1)
  (llvm-type-ref-pointer t2)))

;Coercions

(define (integer->llvm n)
 (cond
  ((integer? n) (LLVMConstInt (current-integer-type) n #t))
  ((llvm-value-ref? n) n)
  (else (error 'integer->llvm "Unknown input value ~a" n))))


(define (boolean->llvm n)
 (cond
  ((boolean? n) (LLVMConstInt (current-boolean-type) (if n 1 0) #t))
  ((llvm-value-ref? n) n)
  (else (error 'boolean->llvm "Unknown input value ~a" n))))


(define (value->llvm n)
 (cond
  ((integer? n) (LLVMConstInt (current-integer-type) n #t))
  ((llvm-value-ref? n) n)
  (else (error 'value->llvm "Unknown input value ~a" n))))


(define (value->llvm-type v)
 (cond
  ((integer? v) (current-integer-type))
  ((boolean? v) (current-boolean-type))
  ((llvm-value-ref v) (llvm-type-of v))
  (else (error 'value->llvm-type "Unknown input value ~a" v))))

;Contracts


(define llvm-current-integer/c
 (flat-named-contract 'llvm-current-integer
  (lambda (n) (or (integer? n)
    (and (llvm-value-ref? n)
         (llvm-type-equal?
           (current-integer-type)
           (llvm-type-of n)))))))


(define llvm-integer/c
 (flat-named-contract 'llvm-integer
  (lambda (n) (or (integer? n)
    (and (llvm-value-ref? n)
         (equal?
          (llvm-get-type-kind 
           (llvm-type-of n))
          'LLVMIntegerTypeKind))))))



(define llvm-any-pointer/c
 (flat-named-contract 'llvm-any-pointer/c
  (lambda (v) 
    (and (llvm-value-ref? v)
     (let ((t (llvm-type-of v)))
      (and (eq? (llvm-get-type-kind t)
                'LLVMPointerTypeKind)))))))




(define llvm-boolean/c
 (flat-named-contract 'llvm-boolean
  (lambda (n) (or (boolean? n) (llvm-value-ref? n)))))


(define llvm-value/c
 (flat-named-contract 'llvm-value
  (lambda (v) (or (integer? v) (llvm-value-ref? v)))))

