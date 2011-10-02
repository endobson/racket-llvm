#lang racket/base



(require
  "../safe/structs.rkt"
  "../ffi/safe.rkt"
  "base.rkt")

(require racket/contract)


(define llvm-float-binop/c
 (->i ((left llvm-float/c)
       (right llvm-float/c))
      (#:builder (builder llvm-builder-ref?)
       #:name (name string?))
      #:pre (left right)
       (equal? (value->llvm-type left) (value->llvm-type right))
      (_ llvm-value-ref?)))



(define llvm-binop/c
 (->i ((left llvm-integer/c)
       (right llvm-integer/c))
      (#:builder (builder llvm-builder-ref?)
       #:name (name string?))
      #:pre (left right)
       (equal? (value->llvm-type left) (value->llvm-type right))
      (_ llvm-value-ref?)))


(provide/contract
 (llvm+ llvm-binop/c)
 (llvm* llvm-binop/c)
 (llvm- llvm-binop/c)
 (llvm/ llvm-binop/c)
 (llvm% llvm-binop/c)
 (llvm-and llvm-binop/c)
 (llvm-or llvm-binop/c)
 (llvm-xor llvm-binop/c)
 (llvm-lshr llvm-binop/c)
 (llvm-ashr llvm-binop/c)
 (llvm-shl llvm-binop/c)
 
 (llvm-fl+ llvm-float-binop/c)
 (llvm-fl- llvm-float-binop/c)
 (llvm-fl* llvm-float-binop/c)
 (llvm-fl/ llvm-float-binop/c)
 
 )


;Provided functions
(define (llvm+ lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildAdd builder (integer->llvm lhv) (integer->llvm rhv) name))

(define (llvm* lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildMul builder (integer->llvm lhv) (integer->llvm rhv) name))

(define (llvm- lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildSub builder (integer->llvm lhv) (integer->llvm rhv) name))

(define (llvm/ lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildSDiv builder (integer->llvm lhv) (integer->llvm rhv) name))

(define (llvm% lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildSRem builder (integer->llvm lhv) (integer->llvm rhv) name))


(define (llvm-and lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildAnd builder (integer->llvm lhv) (integer->llvm rhv) name))

(define (llvm-or lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildOr builder (integer->llvm lhv) (integer->llvm rhv) name))

(define (llvm-xor lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildXor builder (integer->llvm lhv) (integer->llvm rhv) name))


(define (llvm-ashr lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildAShr builder (integer->llvm lhv) (integer->llvm rhv) name))

(define (llvm-lshr lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildLShr builder (integer->llvm lhv) (integer->llvm rhv) name))



(define (llvm-shl lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildShl builder (integer->llvm lhv) (integer->llvm rhv) name))


(define (llvm-fl+ lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildFAdd builder (float->llvm lhv) (float->llvm rhv) name))

(define (llvm-fl* lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildFMul builder (float->llvm lhv) (float->llvm rhv) name))


(define (llvm-fl- lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildFSub builder (float->llvm lhv) (float->llvm rhv) name))

(define (llvm-fl/ lhv rhv #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildFDiv builder (float->llvm lhv) (float->llvm rhv) name))




