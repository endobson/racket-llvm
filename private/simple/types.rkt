#lang racket/base



(require 
  racket/contract
  unstable/contract
  "../safe/structs.rkt"
  "../ffi/safe.rkt"
  "util.rkt"
  "parameters.rkt"
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
  (llvm-get-type-kind (-> llvm-type-ref? symbol?))
  (llvm-get-element-type (-> llvm-sequential-type-ref? llvm-type-ref?))
  (llvm-get-return-type (-> llvm-function-type-ref? llvm-type-ref?))

  ;Constructors
  (llvm-int-type  (-> llvm-integer-type-ref?))
  (llvm-int1-type  (->* () (#:context llvm-context-ref?) llvm-integer-type-ref?))
  (llvm-int8-type  (->* () (#:context llvm-context-ref?) llvm-integer-type-ref?))
  (llvm-int16-type (->* () (#:context llvm-context-ref?) llvm-integer-type-ref?))
  (llvm-int32-type (->* () (#:context llvm-context-ref?) llvm-integer-type-ref?))
  (llvm-int64-type (->* () (#:context llvm-context-ref?) llvm-integer-type-ref?))


  (llvm-single-type  (->* () (#:context llvm-context-ref?) llvm-float-type-ref?))
  (llvm-double-type  (->* () (#:context llvm-context-ref?) llvm-float-type-ref?))
  (llvm-fp128-type (->* () (#:context llvm-context-ref?) llvm-float-type-ref?))
  (llvm-x86-fp80-type (->* () (#:context llvm-context-ref?) llvm-float-type-ref?))
  (llvm-ppc-fp128-type (->* () (#:context llvm-context-ref?) llvm-float-type-ref?))

  ;Mutators
  (llvm-named-struct-type-set-body!
    (->* (llvm-unset-named-struct-type-ref?)
         (#:packed boolean?)
         #:rest (listof llvm-type-ref?)
         void?))
  (llvm-named-struct-type-set-body*!
    (->* (llvm-unset-named-struct-type-ref?)
         (#:packed boolean?)
         #:rest (list*/c llvm-type-ref?)
         void?))

  (llvm-pointer-type (->* (llvm-type-ref?) (#:address-space integer?) llvm-pointer-type-ref?))
  (llvm-function-type (->* (llvm-type-ref?) (#:varargs boolean?) #:rest (listof llvm-type-ref?) llvm-function-type-ref?))
  (llvm-function-type* (->* (llvm-type-ref?) (#:varargs boolean?) #:rest (list*/c llvm-type-ref?) llvm-function-type-ref?))
  (llvm-void-type  (->* () (#:context llvm-context-ref?) llvm-void-type-ref?))



  ;Predicates
  (llvm-integer-type-ref? predicate/c)
  (llvm-float-type-ref? predicate/c)
  (llvm-function-type-ref? predicate/c)
  (llvm-struct-type-ref? predicate/c)
  (llvm-unnamed-struct-type-ref? predicate/c)
  (llvm-named-struct-type-ref? predicate/c)
  (llvm-unset-named-struct-type-ref? predicate/c)
  (llvm-array-type-ref? predicate/c)
  (llvm-vector-type-ref? predicate/c)
  (llvm-pointer-type-ref? predicate/c)
  (llvm-void-type-ref? predicate/c)
  ;TODO rename to llvm-integer-type-width, etc
  (llvm-get-int-type-width
    (-> llvm-integer-type-ref? exact-positive-integer?))
  (llvm-get-array-type-length
    (-> llvm-array-type-ref? exact-nonnegative-integer?))
  (llvm-get-vector-type-size
    (-> llvm-vector-type-ref? exact-positive-integer?))


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


(define (llvm-function-type-ref? type)
 (and (llvm-type-ref? type)
  (eq? (llvm-get-type-kind type)
       'LLVMFunctionTypeKind)))

(define (llvm-composite-type-ref? type)
 (and (llvm-type-ref? type)
  (memq (llvm-get-type-kind type)
   '(LLVMStructTypeKind
     LLVMArrayTypeKind
     LLVMPointerTypeKind
     LLVMVectorTypeKind))))

(define (llvm-sequential-type-ref? type)
 (and (llvm-type-ref? type)
  (memq (llvm-get-type-kind type)
   '(LLVMArrayTypeKind
     LLVMPointerTypeKind
     LLVMVectorTypeKind))))


;TODO implement
(define (llvm-vector-type-ref? ref) #t)
(define (llvm-pointer-type-ref? ref) #t)
(define (llvm-void-type-ref? ref) #t)


(define (llvm-struct-type-ref? type)
  (equal? 'LLVMStructTypeKind (llvm-get-type-kind type)))
(define (llvm-unnamed-struct-type-ref? ref) #t)
(define (llvm-named-struct-type-ref? ref) #t)
(define (llvm-unset-named-struct-type-ref? ref) #t)
(define (llvm-array-type-ref? type) 
  (equal? 'LLVMArrayTypeKind (llvm-get-type-kind type)))


