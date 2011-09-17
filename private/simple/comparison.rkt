#lang racket/base

(require racket/contract)
(require
  "../safe/structs.rkt"
  "../ffi/safe.rkt"
  "base.rkt")


(define llvm-integer-or-pointer/c
 (or/c llvm-any-pointer/c llvm-integer/c))

(define comparison-symbol/c
 (symbols '= '/= '< '> '<= '>=))

(define icmp/c
 (->i ((symbol comparison-symbol/c)
       (left llvm-integer/c)
       (right llvm-integer/c))
      (#:builder (builder llvm-builder-ref?)
       #:signed? (signed boolean?)
       #:name (name string?))
      #:pre (left right)
       (equal?
        (value->llvm-type left)
        (value->llvm-type left))
      (_ llvm-value-ref?)))

(define comparison/c
 (->i ((left llvm-integer-or-pointer/c)
       (right llvm-integer-or-pointer/c))
      (#:builder (builder llvm-builder-ref?)
       #:signed? (signed boolean?)
       #:name (name string?))
      #:pre (left right)
       (equal?
        (value->llvm-type left)
        (value->llvm-type left))
      (_ llvm-value-ref?)))

(provide/contract
 (llvm-icmp icmp/c)
 (llvm-=  comparison/c)
 (llvm-/= comparison/c)
 (llvm->  comparison/c)
 (llvm->= comparison/c)
 (llvm-<  comparison/c)
 (llvm-<= comparison/c)
)

(define (llvm-icmp type lhv rhv #:signed? (signed #t)  #:builder (builder (current-builder)) #:name (name ""))
  (LLVMBuildICmp builder
   (symbol->llvm-int-predicate type signed)
   (integer->llvm lhv)
   (integer->llvm rhv)
   name))

(define ((make-specific-comparison symbol) lhv rhv #:signed? (signed #t)  #:builder (builder (current-builder)) #:name (name ""))
 (llvm-icmp symbol lhv rhv #:signed? signed #:builder builder #:name name))

(define llvm-= (make-specific-comparison '=))
(define llvm-/= (make-specific-comparison '/=))
(define llvm-< (make-specific-comparison '<))
(define llvm-> (make-specific-comparison '>))
(define llvm-<= (make-specific-comparison '<=))
(define llvm->= (make-specific-comparison '>=))


(define (symbol->llvm-int-predicate sym signed)
 (case sym
  ((=) 'LLVMIntEQ)
  ((/=) 'LLVMIntNE)        
  ((>)  (if signed 'LLVMIntSGT 'LLVMIntUGT))
  ((>=) (if signed 'LLVMIntSGE 'LLVMIntUGE))
  ((<)  (if signed 'LLVMIntSLT 'LLVMIntULT))
  ((<=) (if signed 'LLVMIntSLE 'LLVMIntULE))
  (else (error 'symbol->llvm-int-predicate "Expected comparison symbol, got ~a" sym))))



