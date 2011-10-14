#lang racket

(require
  "../ffi/safe.rkt"
  "types.rkt"
  "values.rkt"
  "predicates.rkt"
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
  (llvm-boolean/c contract?)
  (llvm-int
    (->* (integer?)
         (llvm:integer-type?
          #:signed? boolean?)
         llvm:value?))))

 
;TODO enhance contract
(provide
  integer->llvm
  float->llvm
  boolean->llvm
  string->llvm
  value->llvm
  value->llvm-type)



;Constructors

;Integer Creation
(define (llvm-int n (type (current-integer-type)) #:signed? (signed #t))
 (LLVMConstInt type n signed))


;Coercions
(define (integer->llvm n)
 (cond
  ((exact-integer? n) (LLVMConstInt (current-integer-type) n #t))
  ((llvm:value? n) n)
  (else (error 'integer->llvm "Unknown input value ~a" n))))


(define (float->llvm n)
 (cond
  ((real? n) (LLVMConstReal (current-float-type) n))
  ((llvm:value? n) n)
  (else (error 'float->llvm "Unknown input value ~a" n))))


(define (boolean->llvm n)
 (cond
  ((boolean? n) (LLVMConstInt (current-boolean-type) (if n 1 0) #t))
  ((llvm:value? n) n)
  (else (error 'boolean->llvm "Unknown input value ~a" n))))

(define (string->llvm v #:null-terminate (null-terminate #f))
 (cond
  ((string? v) (LLVMConstStringInContext (current-context) v (not null-terminate)))
  ((llvm:value? v) v)
  (else (error 'string->llvm "Unknown input value ~a" v))))


(define (value->llvm n)
 (cond
  ((boolean? n) (LLVMConstInt (current-boolean-type) (if n 1 0) #t))
  ((exact-integer? n) (LLVMConstInt (current-integer-type) n #t))
  ((real? n) (LLVMConstReal (current-float-type) n))
  ((string? n) (LLVMConstStringInContext (current-context) n #t))
  ((llvm:value? n) n)
  (else (error 'value->llvm "Unknown input value ~a" n))))

;Type Level
(define (value->llvm-type v)
 (cond
  ((exact-integer? v) (current-integer-type))
  ((real? v) (current-float-type))
  ((boolean? v) (current-boolean-type))
  ((llvm:value? v) (llvm-type-of v))
  (else (error 'value->llvm-type "Unknown input value ~a" v))))


;Contracts


(define llvm-current-integer/c
 (flat-named-contract 'llvm-current-integer/c
  (lambda (n) (or (exact-integer? n)
    (and (llvm:value? n)
         (equal?
           (current-integer-type)
           (llvm-type-of n)))))))




(define llvm-integer/c
 (flat-named-contract 'llvm-integer/c
  (lambda (n) (or (exact-integer? n)
    (and (llvm:value? n)
         (llvm:integer-type?
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
      ((llvm:value? n)
       (let ((ty (llvm-type-of n)))
         (and (llvm:integer-type? ty)
              (check-type ty))))
      (else #f)))))



(define llvm-float/c
 (flat-named-contract 'llvm-float/c
  (lambda (n) (or (real? n)
    (and (llvm:value? n)
         (llvm:float-type?
           (llvm-type-of n)))))))

(define llvm-any-pointer/c
 (flat-named-contract 'llvm-any-pointer/c
  (lambda (v) 
    (and (llvm:value? v)
     (let ((t (llvm-type-of v)))
      (and (eq? (llvm-get-type-kind t)
                'LLVMPointerTypeKind)))))))






;TODO make tighter
(define llvm-boolean/c
 (flat-named-contract 'llvm-boolean/c
  (lambda (n) (or (boolean? n) (llvm:value? n)))))


(define llvm-value/c
 (flat-named-contract 'llvm-value
  (lambda (v) (or (string? v)
                  (boolean? v)
                  (exact-integer? v)
                  (real? v)
                  (llvm:value? v)))))


(define llvm-constant-value/c
 (flat-named-contract 'llvm-constant-value
  (lambda (v) (or (string? v)
                  (boolean? v)
                  (exact-integer? v)
                  (real? v)
                  (and (llvm:value? v)
                       (llvm:constant? v))))))
                       



