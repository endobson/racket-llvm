#lang racket/base



(require 
  racket/contract
  unstable/contract
  "../ffi/safe.rkt"
  "util.rkt"
  "parameters.rkt"
  "predicates.rkt"
  "primitive-types.rkt")

;TODO add contracts
(provide llvm-array-type
         llvm-vector-type
         llvm-struct-type
         llvm-struct-type*
         llvm-named-struct-type)

(provide
 (contract-out



  ;Deconstructors
  (llvm-get-type-kind (-> llvm:type? symbol?))
  (llvm-get-element-type (-> llvm:sequential-type? llvm:type?))
  (llvm-get-return-type (-> llvm:function-type? llvm:type?))

  ;Constructors
  (llvm-type-of (-> llvm:value? llvm:type?))

  (llvm-int-type  (-> llvm:integer-type?))
  (llvm-int1-type  (->* () (#:context llvm:context?) llvm:integer-type?))
  (llvm-int8-type  (->* () (#:context llvm:context?) llvm:integer-type?))
  (llvm-int16-type (->* () (#:context llvm:context?) llvm:integer-type?))
  (llvm-int32-type (->* () (#:context llvm:context?) llvm:integer-type?))
  (llvm-int64-type (->* () (#:context llvm:context?) llvm:integer-type?))


  (llvm-single-type  (->* () (#:context llvm:context?) llvm:float-type?))
  (llvm-double-type  (->* () (#:context llvm:context?) llvm:float-type?))
  (llvm-fp128-type (->* () (#:context llvm:context?) llvm:float-type?))
  (llvm-x86-fp80-type (->* () (#:context llvm:context?) llvm:float-type?))
  (llvm-ppc-fp128-type (->* () (#:context llvm:context?) llvm:float-type?))

  ;Mutators
  (llvm-named-struct-type-set-body!
    (->* (llvm:unset-named-struct-type?)
         (#:packed boolean?)
         #:rest (listof llvm:type?)
         void?))
  (llvm-named-struct-type-set-body*!
    (->* (llvm:unset-named-struct-type?)
         (#:packed boolean?)
         #:rest (list*/c llvm:type?)
         void?))

  (llvm-pointer-type (->* (llvm:type?) (#:address-space integer?) llvm:pointer-type?))
  (llvm-function-type (->* (llvm:type?) (#:varargs boolean?) #:rest (listof llvm:type?) llvm:function-type?))
  (llvm-function-type* (->* (llvm:type?) (#:varargs boolean?) #:rest (list*/c llvm:type?) llvm:function-type?))
  (llvm-void-type  (->* () (#:context llvm:context?) llvm:void-type?))



  ;Predicates
  (llvm:integer-type? predicate/c)
  (llvm:float-type? predicate/c)
  (llvm:function-type? predicate/c)
  (llvm:struct-type? predicate/c)
  (llvm:unnamed-struct-type? predicate/c)
  (llvm:named-struct-type? predicate/c)
  (llvm:unset-named-struct-type? predicate/c)
  (llvm:array-type? predicate/c)
  (llvm:vector-type? predicate/c)
  (llvm:pointer-type? predicate/c)
  (llvm:void-type? predicate/c)
  ;TODO rename to llvm-integer-type-width, etc
  (llvm-get-int-type-width
    (-> llvm:integer-type? exact-positive-integer?))
  (llvm-get-array-type-length
    (-> llvm:array-type? exact-nonnegative-integer?))
  (llvm-get-vector-type-size
    (-> llvm:vector-type? exact-positive-integer?))


  ))









;Deconstructors


  

(define (llvm-get-return-type type)
 (LLVMGetReturnType type))

(define (llvm-get-element-type type)
 (LLVMGetElementType type))



(define (llvm-get-int-type-width type)
 (LLVMGetIntTypeWidth type))

(define (llvm-get-array-type-length type)
 (LLVMGetArrayLength type))

(define (llvm-get-vector-type-size type)
 (LLVMGetVectorSize type))





;Constructors
(define (llvm-type-of value)
 (LLVMTypeOf value))

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

(define (llvm-pointer-type type  #:address-space (space 0))
 (LLVMPointerType type space))

(define (llvm-function-type return-type #:varargs (varargs #f) . args)
 (LLVMFunctionType return-type args varargs))

(define (llvm-function-type* return-type #:varargs (varargs #f) . args)
 (LLVMFunctionType return-type (apply list* args) varargs))


(define (llvm-void-type #:context (context (current-context)))
 (LLVMVoidTypeInContext context))



(define (llvm-int-type)
 (current-integer-type))

(define (llvm-int1-type #:context (context (current-context)))
 (LLVMInt1TypeInContext context))

(define (llvm-int8-type #:context (context (current-context)))
 (LLVMInt8TypeInContext context))

(define (llvm-int16-type #:context (context (current-context)))
 (LLVMInt16TypeInContext context))

(define (llvm-int32-type #:context (context (current-context)))
 (LLVMInt32TypeInContext context))

(define (llvm-int64-type #:context (context (current-context)))
 (LLVMInt64TypeInContext context))


(define (llvm-single-type #:context (context (current-context)))
 (LLVMFloatTypeInContext context))

(define (llvm-double-type #:context (context (current-context)))
 (LLVMDoubleTypeInContext context))

(define (llvm-fp128-type #:context (context (current-context)))
 (LLVMFP128TypeInContext context))

(define (llvm-x86-fp80-type #:context (context (current-context)))
 (LLVMX86FP80TypeInContext context))

(define (llvm-ppc-fp128-type #:context (context (current-context)))
 (LLVMPPCFP128TypeInContext context))



;Mutators
(define (llvm-named-struct-type-set-body! #:packed (packed #f)
                              type . types)
 (LLVMStructSetBody type types packed))

(define (llvm-named-struct-type-set-body*! #:packed (packed #f)
                              type . types)
 (LLVMStructSetBody type (apply list* types) packed))



;Predicates


(define (llvm:function-type? type)
 (and (llvm:type? type)
  (eq? (llvm-get-type-kind type)
       'LLVMFunctionTypeKind)))

(define (llvm:composite-type? type)
 (and (llvm:type? type)
  (memq (llvm-get-type-kind type)
   '(LLVMStructTypeKind
     LLVMArrayTypeKind
     LLVMPointerTypeKind
     LLVMVectorTypeKind))))

(define (llvm:sequential-type? type)
 (and (llvm:type? type)
  (memq (llvm-get-type-kind type)
   '(LLVMArrayTypeKind
     LLVMPointerTypeKind
     LLVMVectorTypeKind))))


;TODO implement
(define (llvm:vector-type? t) #t)
(define (llvm:pointer-type? t) #t)
(define (llvm:void-type? t) #t)


(define (llvm:struct-type? type)
  (equal? 'LLVMStructTypeKind (llvm-get-type-kind type)))
(define (llvm:unnamed-struct-type? t) #t)
(define (llvm:named-struct-type? t) #t)
(define (llvm:unset-named-struct-type? t) #t)
(define (llvm:array-type? type) 
  (equal? 'LLVMArrayTypeKind (llvm-get-type-kind type)))


