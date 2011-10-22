#lang racket/base

(require
  racket/contract
  unstable/contract
  racket/list
  "parameters.rkt"
  "types.rkt"
  "indexed-types.rkt"
  "values.rkt"
  "util.rkt"
  "convertible.rkt"
  "predicates.rkt"
  "../ffi/safe.rkt")

(provide
 (contract-out
  ;Predicates
  (llvm:array? predicate/c)
  (llvm:struct? predicate/c)
  (llvm:vector? predicate/c)
  (llvm-extract-element llvm-extract-element/c)
  (llvm-insert-element llvm-insert-element/c)
  (llvm-extract-value llvm-extract-value/c)
  (llvm-insert-value llvm-insert-value/c)
  (llvm-vector llvm-vector/c)
  (llvm-vector* llvm-vector*/c)
  (llvm-constant-array llvm-constant-array/c)
  (llvm-constant-array* llvm-constant-array*/c)
  (llvm-struct
    (->* () (#:context llvm:context?
             #:packed boolean?)
         #:rest (listof llvm-value/c) llvm:value?)) 
 (llvm-named-struct
   (->* (llvm:named-struct-type?)
        #:rest (listof llvm-value/c) llvm:value?)))) ;TODO Make contract tighter 

 


;Predicates
(define (llvm:array? v)
  (and (llvm:value? v)
    (llvm:array-type?
      (llvm-type-of v))))

(define (llvm:struct? v)
 (and (llvm:value? v)
   (llvm:struct-type?
     (llvm-type-of v))))

(define (llvm:vector? v)
  (and (llvm:value? v)
   (llvm:vector-type?
     (llvm-type-of v))))


;Contracts
(define llvm-extract-element/c
  (->* (llvm:vector?
        llvm-integer32/c)
       (#:builder llvm:builder?
        #:name string?)
       llvm:value?))

(define llvm-insert-element/c
  (->i ((vector llvm:vector?)
        (arg llvm-value/c)
        (index llvm-integer32/c))
       (#:builder (builder llvm:builder?)
        #:name (name string?))
       #:pre/name (vector arg)
        "Element and vector types don't match"
        (equal? (llvm-get-element-type (llvm-type-of vector))
                (value->llvm-type arg))
       (_ llvm:value?)))


(define llvm-extract-value/c
  (->i ((aggregate (or/c llvm:array? llvm:struct?))
        (index exact-nonnegative-integer?))
       (#:builder (builder llvm:builder?)
        #:name (name string?))
       #:pre/name (aggregate index)
        "Invalid array index"
        (or (not (llvm:array? aggregate))
            (let ((size (llvm-get-array-type-length (llvm-type-of aggregate))))
              (or (zero? size)
                  (< index size))))
       #:pre/name (aggregate index)
        "Invalid struct index"
        (or (not (llvm:struct? aggregate))
            (llvm-is-valid-type-index (llvm-type-of aggregate) index))
       (_ llvm:value?)))

(define llvm-insert-value/c
  (->i ((aggregate (or/c llvm:array? llvm:struct?))
        (arg llvm-value/c)
        (index exact-nonnegative-integer?))
       (#:builder (builder llvm:builder?)
        #:name (name string?))
       #:pre/name (aggregate index)
        "Invalid array index"
        (or (not (llvm:array? aggregate))
            (let ((size (llvm-get-array-type-length (llvm-type-of aggregate))))
              (or (zero? size)
                  (< index size))))
       #:pre/name (aggregate index)
        "Invalid struct index"
        (or (not (llvm:struct? aggregate))
            (llvm-is-valid-type-index (llvm-type-of aggregate) index))
       #:pre/name (aggregate index arg)
        "Element and aggregate types don't match"
        (equal? (llvm-get-type-at-index (llvm-type-of aggregate) index)
                (value->llvm-type arg))
       (_ llvm:value?)))


(define llvm-vector/c
  (->i ()
       (#:builder (builder llvm:builder?))
       #:rest (args (non-empty-listof llvm-value/c))
       #:pre/name (args)
        "Element types don't match"
        (let ((t (value->llvm-type (first args))))
          (andmap (lambda (e) (equal? t (value->llvm-type e)))
                  (rest args)))
       (_ llvm:value?)))


(define llvm-vector*/c
  (->i ()
       (#:builder (builder llvm:builder?))
       #:rest (args (non-empty-list*/c llvm-value/c))
       #:pre/name (args)
        "Element types don't match"
        (let ((args (apply list* args)))
         (let ((t (value->llvm-type (first args))))
           (andmap (lambda (e) (equal? t (value->llvm-type e)))
                   (rest args))))
       (_ llvm:value?)))


(define llvm-constant-array/c
  (->i ()
       ()
       #:rest (args (non-empty-listof llvm-constant-value/c))
       #:pre/name (args)
        "Element types don't match"
        (let ((elem-type (value->llvm-type (first args))))
         (for ((arg (rest args)))
           (equal? elem-type (value->llvm-type arg))))
       (_ llvm:value?)))


(define llvm-constant-array*/c
  (->i ()
       ()
       #:rest (args (non-empty-list*/c llvm-constant-value/c))
       #:pre/name (args)
        "Element types don't match"
        (let ((args (apply list* args)))
         (let ((elem-type (value->llvm-type (first args))))
          (for ((arg (rest args)))
            (equal? elem-type (value->llvm-type arg)))))
       (_ llvm:value?)))


(define (llvm-extract-element v index #:builder (builder (current-builder)) #:name (name ""))
  (LLVMBuildExtractElement builder (value->llvm v) (value->llvm index) name))

(define (llvm-insert-element v arg index #:builder (builder (current-builder)) #:name (name ""))
  (LLVMBuildInsertElement builder (value->llvm v) (value->llvm arg) (value->llvm index) name))


(define (llvm-extract-value v index #:builder (builder (current-builder)) #:name (name ""))
  (LLVMBuildExtractValue builder (value->llvm v) index name))

(define (llvm-insert-value v arg index #:builder (builder (current-builder)) #:name (name ""))
  (LLVMBuildInsertValue builder (value->llvm v) (value->llvm arg) index name))



(define (llvm-vector #:builder (builder (current-builder)) . args)
  (for/fold ((acc (llvm-null (llvm-vector-type (value->llvm-type (first args)) (length args)))))
            ((arg args) (i (in-naturals)))
    (llvm-insert-element acc arg i #:builder builder)))


(define (llvm-vector* #:builder (builder (current-builder)) . args)
  (apply llvm-vector #:builder builder (apply list* args)))



(define (llvm-constant-array . args)
 (LLVMConstArray (value->llvm-type (first args))
                 (map value->llvm args)))
(define (llvm-constant-array* . args)
 (apply llvm-constant-array (apply list* args)))






(define (llvm-struct #:context (context (current-context)) #:packed (packed #f) . args)
 (LLVMConstStructInContext context (map value->llvm args) packed))

(define (llvm-named-struct ty . args)
 (LLVMConstNamedStruct ty (map value->llvm args)))




