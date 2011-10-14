#lang racket/base

(require racket/contract racket/match)
(require
  "../safe/structs.rkt"
  "../ffi/safe.rkt"
  "convertible.rkt"
  "types-values.rkt"
  "parameters.rkt")


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
       #:signed (signed boolean?)
       #:name (name string?))
      #:pre/name (left right)
       "Equal types"
       (equal?
        (value->llvm-type left)
        (value->llvm-type left))
      (_ llvm-value-ref?)))

(define fcmp/c
 (->i ((symbol float-comparison-symbol/c)
       (left llvm-float/c)
       (right llvm-float/c))
      (#:builder (builder llvm-builder-ref?)
       #:ordered (ordered boolean?)
       #:name (name string?))
      #:pre/name (left right)
       "Equal types"
       (equal?
        (value->llvm-type left)
        (value->llvm-type left))
      (_ llvm-value-ref?)))



(define int-comparison/c
 (->i ((left llvm-integer-or-pointer/c)
       (right llvm-integer-or-pointer/c))
      (#:builder (builder llvm-builder-ref?)
       #:signed (signed boolean?)
       #:name (name string?))
      #:pre/name (left right)
       "Equal types"
       (equal?
        (value->llvm-type left)
        (value->llvm-type left))
      (_ llvm-value-ref?)))


(define comparison/c
 (->i ((left (or/c llvm-float/c llvm-integer-or-pointer/c))
       (right (or/c llvm-float/c llvm-integer-or-pointer/c)))
      (#:builder (builder llvm-builder-ref?)
       #:signed (signed boolean?)
       #:ordered (ordered boolean?)
       #:name (name string?))
      #:pre/name (left right)
       "Equal types"
       (equal?
        (value->llvm-type left)
        (value->llvm-type left))
      #:pre/name (left ordered)
       "Ordered only used with float types"
       (or (unsupplied-arg? ordered)
           (llvm-float/c left))
      #:pre/name (left signed)
       "Signed only used with integer types"
       (or (unsupplied-arg? signed)
           (llvm-integer-or-pointer/c left))
      (_ llvm-value-ref?)))





(define float-comparison/c
 (->i ((left llvm-float/c)
       (right llvm-float/c))
      (#:builder (builder llvm-builder-ref?)
       #:ordered (ordered boolean?)
       #:name (name string?))
      #:pre/name (left right)
       "Equal types"
       (equal?
        (value->llvm-type left)
        (value->llvm-type left))
      (_ llvm-value-ref?)))



(provide/contract
 (llvm-icmp icmp/c)
 (llvm-fcmp fcmp/c)
 (llvm-i=  int-comparison/c)
 (llvm-i/= int-comparison/c)
 (llvm-i>  int-comparison/c)
 (llvm-i>= int-comparison/c)
 (llvm-i<  int-comparison/c)
 (llvm-i<= int-comparison/c)

 (llvm-fl=  float-comparison/c)
 (llvm-fl/= float-comparison/c)
 (llvm-fl>  float-comparison/c)
 (llvm-fl>= float-comparison/c)
 (llvm-fl<  float-comparison/c)
 (llvm-fl<= float-comparison/c)

 (llvm-=  comparison/c)
 (llvm-/= comparison/c)
 (llvm->  comparison/c)
 (llvm->= comparison/c)
 (llvm-<  comparison/c)
 (llvm-<= comparison/c)

)

(define (llvm-icmp type lhv rhv #:signed (signed #t)  #:builder (builder (current-builder)) #:name (name ""))
  (LLVMBuildICmp builder
   (symbol->llvm-int-predicate type signed)
   (integer->llvm lhv)
   (integer->llvm rhv)
   name))


(define (llvm-fcmp symbol
                   lhv rhv
                   #:ordered (ordered #t)
                   #:builder (builder (current-builder))
                   #:name (name ""))
  (LLVMBuildFCmp builder
   (symbol->llvm-float-predicate symbol ordered)
   (float->llvm lhv)
   (float->llvm rhv)
   name))


(define ((make-specific-comparison symbol) lhv rhv #:signed (signed #t)  #:builder (builder (current-builder)) #:name (name ""))
 (llvm-icmp symbol lhv rhv #:signed signed #:builder builder #:name name))

(define llvm-i= (make-specific-comparison '=))
(define llvm-i/= (make-specific-comparison '/=))
(define llvm-i< (make-specific-comparison '<))
(define llvm-i> (make-specific-comparison '>))
(define llvm-i<= (make-specific-comparison '<=))
(define llvm-i>= (make-specific-comparison '>=))


(define ((make-specific-float-comparison symbol) lhv rhv
            #:builder (builder (current-builder))
            #:ordered (ordered #t)
            #:name (name ""))
 (llvm-fcmp symbol 
            lhv rhv #:builder builder #:ordered ordered #:name name))

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

(define ((llvm-comparison-chooser int float)
         lhv rhv
         #:builder (builder (current-builder))
         #:signed (signed #t)
         #:ordered (ordered #t)
         #:name (name ""))
  (if (llvm-integer-or-pointer/c lhv)
      (int  lhv rhv #:builder builder #:signed signed  #:name name)
      (float  lhv rhv #:builder builder #:ordered ordered  #:name name)))

(define llvm-=  (llvm-comparison-chooser llvm-i=  llvm-fl=))
(define llvm-/= (llvm-comparison-chooser llvm-i/= llvm-fl/=))
(define llvm->  (llvm-comparison-chooser llvm-i>  llvm-fl>))
(define llvm->= (llvm-comparison-chooser llvm-i>= llvm-fl>=))
(define llvm-<  (llvm-comparison-chooser llvm-i<  llvm-fl<))
(define llvm-<= (llvm-comparison-chooser llvm-i<= llvm-fl<=))


