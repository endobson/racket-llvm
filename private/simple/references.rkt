#lang racket/base
(require 
  racket/contract
  unstable/contract
  "convertible.rkt"
  "memory.rkt"
  "types.rkt"
  "indexed-types.rkt"
  "memory.rkt"
  "parameters.rkt"
  "predicates.rkt")

(provide
 (contract-out

  (llvm:reference? predicate/c)
  (llvm:reference (-> llvm-any-pointer/c llvm:reference?))
  (llvm:reference-pointer (-> llvm:reference? llvm-any-pointer/c))
  (llvm:reference/c (-> llvm:type? contract?))
  (llvm:ger ger/c)
  (llvm:ger0 ger0/c)
  (llvm:set set/c)
  (llvm:read read/c)))

;TODO add contract
(provide
  llvm:set-multiple)

;TODO add printing
(struct llvm:reference (pointer)
        #:property prop:llvm-value
        (lambda (ref)
          (define pointer (llvm:reference-pointer ref))
          (values (llvm-load pointer)
                  (llvm-get-element-type (value->llvm-type pointer)))))


(define set/c
 (->i ((reference llvm:reference?)
       (value llvm:value/c))
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

(define ger/c
 (->i ((reference llvm:reference?))
      (#:builder (builder llvm:builder?)
       #:name (name string?))
      #:rest (args (listof llvm-integer/c))
      #:pre/name (reference args)
       "Invalid indices"
       (llvm-valid-gep-indices? (value->llvm-type (llvm:reference-pointer reference)) (map integer->llvm args))
      (_ llvm:reference?)))


(define ger0/c
 (->i ((reference llvm:reference?))
      (#:builder (builder llvm:builder?)
       #:name (name string?))
      #:rest (args (listof llvm-integer/c))
      #:pre/name (reference args)
       "Invalid indices"
       (llvm-valid-gep-indices? (value->llvm-type (llvm:reference-pointer reference)) (map integer->llvm (cons 0 args)))
      (_ llvm:reference?)))




(define (llvm:set-multiple references #:builder (builder (current-builder)) . values)
  (for ((ref references) (v values))
       (llvm:set ref v #:builder builder)))

(define (llvm:set reference value #:builder (builder (current-builder)))
  (llvm-store value (llvm:reference-pointer reference) #:builder builder))

(define (llvm:read reference #:builder (builder (current-builder)) #:name (name ""))
  (llvm-load (llvm:reference-pointer reference) #:builder builder #:name name))

(define (llvm:reference/c type)
  (struct/c llvm:reference (llvm:type/c (llvm-pointer-type type))))

(define (llvm:ger reference #:builder (builder (current-builder)) #:name (name "") . indices)
  (llvm:reference (apply llvm-gep (llvm:reference-pointer reference) #:builder builder #:name name indices)))

(define (llvm:ger0 reference #:builder (builder (current-builder)) #:name (name "") . indices)
  (llvm:reference (apply llvm-gep0 (llvm:reference-pointer reference) #:builder builder #:name name indices)))
