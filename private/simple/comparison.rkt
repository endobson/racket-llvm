#lang racket/base

(require racket/contract racket/match)
(require
  "../safe/structs.rkt"
  "../ffi/safe.rkt"
  "base.rkt")


(define llvm-integer-or-pointer/c
 (or/c llvm-any-pointer/c llvm-integer/c))

(define comparison-symbol/c
 (symbols '= '/= '< '> '<= '>=))

(define float-comparison-symbol/c
 (symbols 'true 'false 'order '= '/= '< '> '<= '>=))


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

(define fcmp/c
 (->i ((symbol float-comparison-symbol/c)
       (left llvm-float/c)
       (right llvm-float/c))
      (#:builder (builder llvm-builder-ref?)
       #:ordered? (ordered boolean?)
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




(define float-comparison/c
 (->i ((left llvm-float/c)
       (right llvm-float/c))
      (#:builder (builder llvm-builder-ref?)
       #:ordered? (ordered boolean?)
       #:name (name string?))
      #:pre (left right)
       (equal?
        (value->llvm-type left)
        (value->llvm-type left))
      (_ llvm-value-ref?)))



(provide/contract
 (llvm-icmp icmp/c)
 (llvm-fcmp fcmp/c)
 (llvm-=  comparison/c)
 (llvm-/= comparison/c)
 (llvm->  comparison/c)
 (llvm->= comparison/c)
 (llvm-<  comparison/c)
 (llvm-<= comparison/c)

 (llvm-fl=  float-comparison/c)
 (llvm-fl/= float-comparison/c)
 (llvm-fl>  float-comparison/c)
 (llvm-fl>= float-comparison/c)
 (llvm-fl<  float-comparison/c)
 (llvm-fl<= float-comparison/c)


)

(define (llvm-icmp type lhv rhv #:signed? (signed #t)  #:builder (builder (current-builder)) #:name (name ""))
  (LLVMBuildICmp builder
   (symbol->llvm-int-predicate type signed)
   (integer->llvm lhv)
   (integer->llvm rhv)
   name))


(define (llvm-fcmp symbol
                   lhv rhv
                   #:ordered? (ordered #t)
                   #:builder (builder (current-builder))
                   #:name (name ""))
  (LLVMBuildFCmp builder
   (symbol->llvm-float-predicate symbol ordered)
   (float->llvm lhv)
   (float->llvm rhv)
   name))


(define ((make-specific-comparison symbol) lhv rhv #:signed? (signed #t)  #:builder (builder (current-builder)) #:name (name ""))
 (llvm-icmp symbol lhv rhv #:signed? signed #:builder builder #:name name))

(define llvm-= (make-specific-comparison '=))
(define llvm-/= (make-specific-comparison '/=))
(define llvm-< (make-specific-comparison '<))
(define llvm-> (make-specific-comparison '>))
(define llvm-<= (make-specific-comparison '<=))
(define llvm->= (make-specific-comparison '>=))


(define ((make-specific-float-comparison symbol) lhv rhv
            #:builder (builder (current-builder))
            #:ordered? (ordered #f)
            #:name (name ""))
 (llvm-fcmp symbol 
            lhv rhv #:builder builder #:ordered? ordered #:name name))

(define llvm-fl=  (make-specific-float-comparison '=))
(define llvm-fl>  (make-specific-float-comparison '>))
(define llvm-fl>= (make-specific-float-comparison '>=))
(define llvm-fl<  (make-specific-float-comparison '<))
(define llvm-fl<= (make-specific-float-comparison '<=))
(define llvm-fl/= (make-specific-float-comparison '/=))


(define (symbol->llvm-float-predicate sym ordered)
 (match* (sym ordered)
  (('false _) 'LLVMRealPredicateFalse)
  (('true _) 'LLVMRealPredicateFalse)
  (('= #t) 'LLVMRealOEQ)
  (('= #f) 'LLVMRealUEQ)
  (('> #t) 'LLVMRealOGT)
  (('> #f) 'LLVMRealUGT)
  (('>= #t) 'LLVMRealOGE)
  (('>= #f) 'LLVMRealUGE)
  (('< #t) 'LLVMRealOLT)
  (('< #f) 'LLVMRealULT)
  (('<= #t) 'LLVMRealOLE)
  (('<= #f) 'LLVMRealULE)
  (('/= #t) 'LLVMRealONE)
  (('/= #f) 'LLVMRealUNE)
  (('order #t) 'LLVMRealORD)
  (('order #f) 'LLVMRealUNO)))



(define (symbol->llvm-int-predicate sym signed)
 (case sym
  ((=) 'LLVMIntEQ)
  ((/=) 'LLVMIntNE)        
  ((>)  (if signed 'LLVMIntSGT 'LLVMIntUGT))
  ((>=) (if signed 'LLVMIntSGE 'LLVMIntUGE))
  ((<)  (if signed 'LLVMIntSLT 'LLVMIntULT))
  ((<=) (if signed 'LLVMIntSLE 'LLVMIntULE))
  (else (error 'symbol->llvm-int-predicate "Expected comparison symbol, got ~a" sym))))



