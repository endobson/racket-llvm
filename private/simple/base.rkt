#lang racket/base

(require
  racket/contract
  unstable/contract
  racket/list
  (except-in ffi/unsafe ->)
  "../safe/structs.rkt"
  "../ffi/safe.rkt")


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
  (lambda (int-type)
   (or int-type
    (error 'current-integer-type "Current integer-type was never set")))))


(define current-float-type
 (make-derived-parameter
  (make-parameter #f)
  (lambda (x) x)
  (lambda (float-type)
   (or float-type
    (error 'float-type "Current float-type was never set")))))


(define (current-boolean-type)
 (LLVMInt1TypeInContext (current-context)))

;Helpers

(define (llvm-get-type-at-index type idx)
 (LLVMGetTypeAtIndex type idx))

(define (llvm-is-valid-type-index type idx)
 (LLVMIsValidTypeIndex type idx))


(define (llvm-valid-gep-indices? type indices)
 (let ((type (llvm-gep-type type indices)))
  (if type #t #f)))
      

(define (llvm-gep-type type indices)
 (and (equal? (llvm-get-type-kind type)
              'LLVMPointerTypeKind)
  (let loop ((type (llvm-get-element-type type)) (indices (rest indices)))
   (or (and (empty? indices) type)
    (let ((kind (llvm-get-type-kind type)))
     (and (memq kind '(LLVMStructTypeKind LLVMArrayTypeKind LLVMVectorTypeKind))
          (llvm-is-valid-type-index type (first indices))
          (loop (llvm-get-type-at-index type (first indices)) (rest indices))))))))
  

(define (llvm-get-return-type type)
 (LLVMGetReturnType type))

(define (llvm-get-element-type type)
 (LLVMGetElementType type))

(define (llvm-type-of value)
 (LLVMTypeOf value))

(define (llvm-get-type-kind type)
 (LLVMGetTypeKind type))

(define (llvm-get-int-type-width type)
 (LLVMGetIntTypeWidth type))

(define (llvm-get-array-type-length type)
 (LLVMGetArrayLength type))


(define (llvm-integer-type-ref? ref)
 (and (llvm-type-ref? ref)
  (let ((type-kind (llvm-get-type-kind ref)))
   (equal? type-kind 'LLVMIntegerTypeKind))))

(define (llvm-float-type-ref? ref)
 (and (llvm-type-ref? ref)
  (let ((type-kind (llvm-get-type-kind ref)))
   (member type-kind
           '(LLVMFloatTypeKind
             LLVMDoubleTypeKind
             LLVMX86_FP80TypeKind
              LLVMFP128TypeKind
             LLVMPPC_FP128TypeKind))
   #t)))

(define (llvm-unnamed-struct-type-ref? ref) #t)
(define (llvm-named-struct-type-ref? ref) #t)
(define (llvm-unset-named-struct-type-ref? ref) #t)
(define (llvm-array-type-ref? ref) #t)
(define (llvm-vector-type-ref? ref) #t)
(define (llvm-pointer-type-ref? ref) #t)
(define (llvm-void-type-ref? ref) #t)




(define (llvm-function-type-ref? type)
 (eq? (llvm-get-type-kind type)
      'LLVMFunctionTypeKind))

(define (llvm-composite-type-ref? type)
 (memq (llvm-get-type-kind type)
  '(LLVMStructTypeKind
    LLVMArrayTypeKind
    LLVMPointerTypeKind
    LLVMVectorTypeKind)))

(define (llvm-sequential-type-ref? type)
 (memq (llvm-get-type-kind type)
  '(LLVMArrayTypeKind
    LLVMPointerTypeKind
    LLVMVectorTypeKind)))

(define (llvm-terminator-instruction? value)
 (LLVMIsTerminatorInstruction value))

(define (llvm-is-constant? value)
 (LLVMIsConstant value))


(define (llvm-get-undef type)
 (LLVMGetUndef type))
(define (llvm-null type)
 (LLVMConstNull type))



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


(define (value->llvm-type v)
 (cond
  ((exact-integer? v) (current-integer-type))
  ((real? v) (current-float-type))
  ((boolean? v) (current-boolean-type))
  ((llvm-value-ref? v) (llvm-type-of v))
  (else (error 'value->llvm-type "Unknown input value ~a" v))))

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

(define (llvm-vector? v)
  (and (llvm-value-ref? v)
   (llvm-vector-type-ref?
     (llvm-type-of v))))


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
                       (llvm-is-constant? v))))))
                       



(provide/contract
 (current-builder         (parameter/c llvm-builder-ref?))
 (current-context         (parameter/c llvm-context-ref?))
 (current-module          (parameter/c llvm-module-ref?))
 (current-integer-type    (parameter/c llvm-integer-type-ref?))
 (current-float-type      (parameter/c llvm-float-type-ref?))

 (llvm-value/c contract?)
 (llvm-constant-value/c contract?)
 (llvm-any-pointer/c contract?)
 (llvm-current-integer/c contract?)
 (llvm-integer/c contract?)
 (llvm-integer32/c contract?)
 (llvm-float/c contract?)
 (llvm-boolean/c contract?)
 
 (llvm-integer-type-ref? predicate/c)
 (llvm-float-type-ref? predicate/c)
 (llvm-function-type-ref? predicate/c)

 (llvm-unnamed-struct-type-ref? predicate/c)
 (llvm-named-struct-type-ref? predicate/c)
 (llvm-unset-named-struct-type-ref? predicate/c)
 (llvm-array-type-ref? predicate/c)
 (llvm-vector-type-ref? predicate/c)
 (llvm-pointer-type-ref? predicate/c)
 (llvm-void-type-ref? predicate/c)
  
 (llvm-vector? predicate/c)

 (llvm-valid-gep-indices? (-> llvm-type-ref? (listof llvm-integer/c) boolean?))
 (llvm-gep-type
   (->i ((type llvm-type-ref?)
         (indices (listof llvm-integer/c)))
        #:pre (type indices)
         (llvm-valid-gep-indices? type indices)
        (_ llvm-type-ref?)))

 (llvm-get-array-type-length (-> llvm-array-type-ref? exact-nonnegative-integer?))

 (llvm-type-of (-> llvm-value-ref? llvm-type-ref?))
 (llvm-get-type-kind (-> llvm-type-ref? symbol?))
 (llvm-get-element-type (-> llvm-sequential-type-ref? llvm-type-ref?))
 (llvm-get-return-type (-> llvm-function-type-ref? llvm-type-ref?))
 (llvm-terminator-instruction? (-> llvm-value-ref? boolean?))
 (llvm-get-undef (-> llvm-type-ref? llvm-value-ref?))
 (llvm-null (-> llvm-type-ref? llvm-value-ref?))

 (llvm-get-type-at-index
  (->i ((type llvm-composite-type-ref?)
        (index llvm-value/c))
       #:pre (type index)
        (llvm-is-valid-type-index type index)
       (_ llvm-value-ref?)))

 )

(provide
  integer->llvm
  float->llvm
  boolean->llvm
  string->llvm
  value->llvm
  value->llvm-type)



