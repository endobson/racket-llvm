#lang racket/base

(require racket/contract
         racket/list)
(require "base.rkt"
         "types.rkt"
         "util.rkt"
         "../safe/structs.rkt"
         "../ffi/safe.rkt")

(define llvm-extract-element/c
  (->* (llvm-vector?
        llvm-integer32/c)
       (#:builder llvm-builder-ref?
        #:name string?)
       llvm-value-ref?))

(define llvm-insert-element/c
  (->i ((vector llvm-vector?)
        (arg llvm-value/c)
        (index llvm-integer32/c))
       (#:builder (builder llvm-builder-ref?)
        #:name (name string?))
       #:pre/name (vector arg)
        "Matching vector and element types"
        (equal? (llvm-get-element-type (llvm-type-of vector))
                (value->llvm-type arg))
       (result llvm-value-ref?)))

(define llvm-vector/c
  (->i ()
       (#:builder (builder llvm-builder-ref?))
       #:rest (args (non-empty-listof llvm-value/c))
       #:pre/name (args)
        "Matching element types"
        (let ((t (value->llvm-type (first args))))
          (andmap (lambda (e) (equal? t (value->llvm-type e)))
                  (rest args)))
       (result llvm-value-ref?)))


(define llvm-vector*/c
  (->i ()
       (#:builder (builder llvm-builder-ref?))
       #:rest (args (non-empty-list*/c llvm-value/c))
       #:pre/name (args)
        "Matching element types"
        (let ((args (apply list* args)))
         (let ((t (value->llvm-type (first args))))
           (andmap (lambda (e) (equal? t (value->llvm-type e)))
                   (rest args))))
       (result llvm-value-ref?)))


(define llvm-constant-array/c
  (->i ()
       ()
       #:rest (args (non-empty-listof llvm-constant-value/c))
       #:pre/name (args)
        "Element types match"
        (let ((elem-type (value->llvm-type (first args))))
         (for ((arg (rest args)))
           (equal? elem-type (value->llvm-type arg))))
       (result llvm-value-ref?)))


(define llvm-constant-array*/c
  (->i ()
       ()
       #:rest (args (non-empty-list*/c llvm-constant-value/c))
       #:pre/name (args)
        "Element types match"
        (let ((args (apply list* args)))
         (let ((elem-type (value->llvm-type (first args))))
          (for ((arg (rest args)))
            (equal? elem-type (value->llvm-type arg)))))
       (result llvm-value-ref?)))



(provide/contract
  (llvm-extract-element llvm-extract-element/c)
  (llvm-insert-element llvm-insert-element/c)
  (llvm-vector llvm-vector/c)
  (llvm-vector* llvm-vector*/c)
  (llvm-constant-array llvm-constant-array/c)
  (llvm-constant-array* llvm-constant-array*/c))


(define (llvm-extract-element v index #:builder (builder (current-builder)) #:name (name ""))
  (LLVMBuildExtractElement builder (value->llvm v) (value->llvm index) name))

(define (llvm-insert-element v arg index #:builder (builder (current-builder)) #:name (name ""))
  (LLVMBuildInsertElement builder (value->llvm v) (value->llvm arg) (value->llvm index) name))


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





