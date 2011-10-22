#lang racket/base
(require 
  racket/contract
  "convertible.rkt"
  "memory.rkt"
  "types.rkt"
  "parameters.rkt"
  "predicates.rkt")

(provide
 (contract-out
  (llvm:reference (-> llvm-any-pointer/c llvm:reference?))
  (llvm:set set/c)
  (llvm:read read/c)))

;TODO add contract
(provide
  llvm:set-multiple)

(struct llvm:reference (pointer)
        #:property prop:llvm-value
        (lambda (ref)
          (define pointer (llvm:reference-pointer ref))
          (values (llvm-load pointer)
                  (llvm-get-element-type (value->llvm-type pointer)))))


(define set/c
 (->i ((reference llvm:reference?)
       (value llvm:value?))
      (#:builder (builder llvm:builder?))
      #:pre/name (reference value)
       "Type of value does not match type of reference"
       (equal?
        (llvm-get-element-type (value->llvm-type (llvm:reference-pointer reference)))
        (value->llvm-type value))
      (_ llvm:value?)))

(define read/c
 (->* (llvm:reference?)
      (#:builder llvm:builder?
       #:name string?)
      llvm:value?))

(define (llvm:set-multiple references #:builder (builder (current-builder)) . values)
  (for ((ref references) (v values))
       (llvm:set ref v #:builder builder)))

(define (llvm:set reference value #:builder (builder (current-builder)))
  (llvm-store value (llvm:reference-pointer reference) #:builder builder))

(define (llvm:read reference #:builder (builder (current-builder)) #:name (name ""))
  (llvm-load (llvm:reference-pointer reference) #:builder builder #:name name))
