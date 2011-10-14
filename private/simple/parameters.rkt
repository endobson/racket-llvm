#lang racket/base

(require 
  racket/contract
  "../ffi/safe.rkt"
  "predicates.rkt"
  "primitive-types.rkt")

;TODO contracts
(provide
 (contract-out
  (current-builder         (parameter/c llvm:builder?))
  (current-context         (parameter/c llvm:context?))
  (current-module          (parameter/c llvm:module?))
  (current-integer-type    (parameter/c llvm:integer-type?))
  (current-float-type      (parameter/c llvm:float-type?))
  (current-boolean-type (-> llvm:integer-type?))))

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

