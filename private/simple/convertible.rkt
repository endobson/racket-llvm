#lang racket

(require
  "../safe/structs.rkt"
  "../ffi/safe.rkt"
  "types.rkt"
  "values.rkt"
  "parameters.rkt")

(provide
 (contract-out
  (llvm-value/c contract?)
  (llvm-constant-value/c contract?)
  (llvm-any-pointer/c contract?)
  (llvm-current-integer/c contract?)
  (llvm-integer/c contract?)
  (llvm-integer32/c contract?)
  (llvm-float/c contract?)
  (llvm-boolean/c contract?)))
 
(provide
  integer->llvm
  float->llvm
  boolean->llvm
  string->llvm
  value->llvm)

;TODO remove
(define (llvm-type-of value)
 (LLVMTypeOf value))


;Coercions
(define (integer->llvm n)
 (cond
  ((exact-integer? n) (LLVMConstInt (current-integer-type) n #t))
  ((llvm-value-ref? n) n)
  (else (error 'integer->llvm "Unknown input value ~a" n))))


(define (float->llvm n)
 (cond
  ((real? n) (LLVMConstReal (current-float-type) n))
  ((llvm-value-ref? n) n)
  (else (error 'float->llvm "Unknown input value ~a" n))))


(define (boolean->llvm n)
 (cond
  ((boolean? n) (LLVMConstInt (current-boolean-type) (if n 1 0) #t))
  ((llvm-value-ref? n) n)
  (else (error 'boolean->llvm "Unknown input value ~a" n))))

(define (string->llvm v #:null-terminate (null-terminate #f))
 (cond
  ((string? v) (LLVMConstStringInContext (current-context) v (not null-terminate)))
  ((llvm-value-ref? v) v)
  (else (error 'string->llvm "Unknown input value ~a" v))))


(define (value->llvm n)
 (cond
  ((boolean? n) (LLVMConstInt (current-boolean-type) (if n 1 0) #t))
  ((exact-integer? n) (LLVMConstInt (current-integer-type) n #t))
  ((real? n) (LLVMConstReal (current-float-type) n))
  ((string? n) (LLVMConstStringInContext (current-context) n #t))
  ((llvm-value-ref? n) n)
  (else (error 'value->llvm "Unknown input value ~a" n))))

;Contracts


(define llvm-current-integer/c
 (flat-named-contract 'llvm-current-integer/c
  (lambda (n) (or (exact-integer? n)
    (and (llvm-value-ref? n)
         (equal?
           (current-integer-type)
           (llvm-type-of n)))))))




(define llvm-integer/c
 (flat-named-contract 'llvm-integer/c
  (lambda (n) (or (exact-integer? n)
    (and (llvm-value-ref? n)
         (llvm-integer-type-ref?
           (llvm-type-of n)))))))

(define llvm-integer32/c
 (flat-named-contract 'llvm-integer32/c
  (lambda (n)
    (define (check-type ty)
     (equal? 32
      (llvm-get-int-type-width ty)))
    (cond
      ((exact-integer? n)
       (check-type (current-integer-type)))
      ((llvm-value-ref? n)
       (let ((ty (llvm-type-of n)))
         (and (llvm-integer-type-ref? ty)
              (check-type ty))))
      (else #f)))))



(define llvm-float/c
 (flat-named-contract 'llvm-float/c
  (lambda (n) (or (real? n)
    (and (llvm-value-ref? n)
         (llvm-float-type-ref?
           (llvm-type-of n)))))))

(define llvm-any-pointer/c
 (flat-named-contract 'llvm-any-pointer/c
  (lambda (v) 
    (and (llvm-value-ref? v)
     (let ((t (llvm-type-of v)))
      (and (eq? (llvm-get-type-kind t)
                'LLVMPointerTypeKind)))))))

(define llvm-function-pointer/c
 (flat-named-contract 'llvm-function-pointer/c
  (lambda (v) 
    (and (llvm-value-ref? v)
     (let ((t (llvm-type-of v)))
      (and (eq? (llvm-get-type-kind t)
                'LLVMPointerTypeKind)
           (llvm-function-type-ref? (llvm-get-element-type t))))))))






(define llvm-boolean/c
 (flat-named-contract 'llvm-boolean/c
  (lambda (n) (or (boolean? n) (llvm-value-ref? n)))))


(define llvm-value/c
 (flat-named-contract 'llvm-value
  (lambda (v) (or (string? v)
                  (boolean? v)
                  (exact-integer? v)
                  (real? v)
                  (llvm-value-ref? v)))))


(define llvm-constant-value/c
 (flat-named-contract 'llvm-constant-value
  (lambda (v) (or (string? v)
                  (boolean? v)
                  (exact-integer? v)
                  (real? v)
                  (and (llvm-value-ref? v)
                       (llvm:constant? v))))))
                       



