#lang racket/base

(require (only-in ffi/unsafe cpointer?) racket/contract unstable/contract)
(require "../ffi/safe.rkt" "parameters.rkt" "types.rkt"
  "convertible.rkt")

(provide
 (contract-out
  (llvm:int->generic (->* (exact-integer?) (#:type llvm-integer-type-ref? #:signed boolean?) llvm-generic-value?))
  (llvm:int32->generic (->* (exact-integer?) (#:signed boolean?) llvm-generic-value?))
                     


  (llvm:float->generic (->* (real?) (#:type (or/c llvm-float-type-ref? 'single 'double)) llvm-generic-value?))
  (llvm:single->generic (-> real? llvm-generic-value?))
  (llvm:double->generic (-> real? llvm-generic-value?))
                     
  (llvm:pointer->generic (-> cpointer? llvm-generic-value?))

  (llvm:generic-get-int-width (-> llvm-generic-value? exact-positive-integer?))
  (llvm:generic->int (->* (llvm-generic-value?) (#:signed boolean?) exact-integer?))
  (llvm:generic->pointer (-> llvm-generic-value?  cpointer?))

  (llvm:generic->float (->* (llvm-generic-value?) (#:type (or/c 'single 'double llvm-float-type-ref?)) real?))
  (llvm:generic->single (->* (llvm-generic-value?)  real?))
  (llvm:generic->double (->* (llvm-generic-value?)  real?))

  (llvm-generic-value? predicate/c)))

;TODO implement
(define (llvm-generic-value? v) #t)

;TODO remove
(define (llvm-create-context)
 (LLVMContextCreate))

(define (llvm-single-type #:context (context (current-context)))
 (LLVMFloatTypeInContext context))

(define (llvm-double-type #:context (context (current-context)))
 (LLVMDoubleTypeInContext context))

(define (llvm-int32-type #:context (context (current-context)))
 (LLVMInt32TypeInContext context))


;---
(define context (llvm-create-context))
(define single (llvm-single-type #:context context))
(define double (llvm-double-type #:context context))
(define int32 (llvm-int32-type #:context context))


(define (llvm:int->generic n #:type (type (current-integer-type)) #:signed (signed #t))
 (LLVMCreateGenericValueOfInt type n signed))

;TODO clean this up
(define (llvm:int32->generic n #:signed (signed #t))
  (LLVMCreateGenericValueOfInt int32 n signed))



(define (llvm:pointer->generic ptr)
 (LLVMCreateGenericValueOfPointer ptr))



(define (llvm:float->generic x #:type (type (current-float-type)))
  (let ((type (cond
               ((llvm-float-type-ref? type) type)
               ((equal? 'double type) double)
               ((equal? 'single type) single))))
    (LLVMCreateGenericValueOfFloat type x)))

(define (llvm:double->generic x)
  (llvm:float->generic x #:type double))

(define (llvm:single->generic x)
  (llvm:float->generic x #:type single))



(define (llvm:generic-get-int-width gen)
  (LLVMGenericValueIntWidth gen))


(define (llvm:generic->int gen #:signed (signed #t))
  (LLVMGenericValueToInt gen signed))

(define (llvm:generic->pointer gen)
  (LLVMGenericValueToPointer gen))

(define (llvm:generic->float gen #:type (type (current-float-type)))
 (let ((type (cond
              ((llvm-float-type-ref? type) type)
              ((equal? 'double type) double)
              ((equal? 'single type) single))))
  (LLVMGenericValueToFloat type gen)))
    

(define (llvm:generic->double x)
 (llvm:generic->float x #:type double))

(define (llvm:generic->single x)
 (llvm:generic->float x #:type single))




