#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "misc-operations.rkt"
  "ctypes.rkt")

(require ffi/unsafe)


;TODO differentiate types and ensure that types match,
;and contexts match
(define safe:binop
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define (safe:llvm-value-for-builder/c builder)
  (and/c
    safe:llvm-value-ref?
    (lambda (v)
      (let ((owner (safe:llvm-value-ref-owner v)))
        (if (safe:llvm-module-ref? owner)
          (equal? owner (safe:llvm-builder-ref-module builder))
          (equal? owner (safe:llvm-module-ref-context
                          (safe:llvm-builder-ref-module builder))))))))


(define safe:binop/c
  (->i  ((builder safe:llvm-builder-ref?)
         (left-value (builder) (safe:llvm-value-for-builder/c builder))
         (right-value (builder) (safe:llvm-value-for-builder/c builder))
         (name string?))
        #:pre/name (left-value right-value) "Values of different types"
        (equal? (safe:LLVMTypeOf left-value)
                (safe:LLVMTypeOf right-value))
        (result safe:llvm-value-ref?)))

(define safe:uniop
  (_fun (builder : safe:LLVMBuilderRef)
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:icmp
  (_fun (builder : safe:LLVMBuilderRef)
        LLVMIntPredicate
        safe:LLVMValueRef
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:icmp/c
  (->i  ((builder safe:llvm-builder-ref?)
         (predicate LLVMIntPredicate?)
         (left-value (builder) (safe:llvm-value-for-builder/c builder))
         (right-value (builder) (safe:llvm-value-for-builder/c builder))
         (name string?))
        #:pre/name (left-value right-value) "Values of different types"
        (equal? (safe:LLVMTypeOf left-value)
                (safe:LLVMTypeOf right-value))
        (result safe:llvm-value-ref?)))


(define safe:fcmp
  (_fun (builder : safe:LLVMBuilderRef)
        LLVMRealPredicate
        safe:LLVMValueRef
        safe:LLVMValueRef
        _non-null-string ->
        (ptr : _pointer) ->
        (safe:llvm-value-ref ptr (safe:llvm-builder-ref-module builder))))

(define safe:fcmp/c
  (->i  ((builder safe:llvm-builder-ref?)
         (predicate LLVMRealPredicate?)
         (left-value (builder) (safe:llvm-value-for-builder/c builder))
         (right-value (builder) (safe:llvm-value-for-builder/c builder))
         (name string?))
        #:pre/name (left-value right-value) "Values of different types"
        (equal? (safe:LLVMTypeOf left-value)
                (safe:LLVMTypeOf right-value))
        (result safe:llvm-value-ref?)))








;/* Arithmetic */
(define-llvm-multiple
  (LLVMBuildAdd
   LLVMBuildNSWAdd
   LLVMBuildNUWAdd
   LLVMBuildFAdd
   LLVMBuildSub
   LLVMBuildNSWSub
   LLVMBuildNUWSub
   LLVMBuildFSub
   LLVMBuildMul
   LLVMBuildNSWMul
   LLVMBuildNUWMul
   LLVMBuildFMul
   LLVMBuildUDiv
   LLVMBuildSDiv
   LLVMBuildExactSDiv
   LLVMBuildFDiv
   LLVMBuildURem
   LLVMBuildSRem
   LLVMBuildFRem
   LLVMBuildShl
   LLVMBuildLShr
   LLVMBuildAShr
   LLVMBuildAnd
   LLVMBuildOr
   LLVMBuildXor)
  #:unsafe (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef)
  #:safe safe:binop
  (#:provide (#:safe safe:binop/c)))



(define-llvm-unsafe LLVMBuildBinOp
 (_fun LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-multiple
 (LLVMBuildNeg
  LLVMBuildNSWNeg
  LLVMBuildNUWNeg
  LLVMBuildFNeg
  LLVMBuildNot)
 #:unsafe (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef)
 #:safe safe:uniop)




;/* Comparisons */
(define-llvm LLVMBuildICmp
  #:unsafe 
  (_fun LLVMBuilderRef
        LLVMIntPredicate
        LLVMValueRef
        LLVMValueRef
        _string -> LLVMValueRef)
  #:safe safe:icmp
  (#:provide (#:safe safe:icmp/c)))



(define-llvm LLVMBuildFCmp
  #:unsafe
  (_fun LLVMBuilderRef
        LLVMRealPredicate
        LLVMValueRef
        LLVMValueRef
        _string -> LLVMValueRef)
  #:safe safe:fcmp
  (#:provide (#:safe safe:fcmp/c)))
