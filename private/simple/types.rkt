#lang racket/base



(require 
  racket/contract
  unstable/contract
  "../safe/structs.rkt"
  "../ffi/safe.rkt"
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


