#lang racket/base
(require
  "../simple.rkt"
  "../private/short.rkt")


(ll
 (llvm-define-module module
  #:exports (add-float add-byte max-byte)


 (llvm-define-function add-float
  ((left float) (right float) -> float)
  (return (+ left right)))

 (llvm-define-function add-byte
  ((left i8) (right i8) -> i8)
  (return (+ left right)))

 (llvm-define-function max-byte
  ((left i8) (right i8) -> i8)
  (return
    (llvm-if (< left right)
      right
      left)))


 ))


(llvm-assert-module-valid module)
(void (llvm:optimize-module module))
(define jit (llvm:create-jit module))



(define add-float* (llvm:extract-function jit add-float))
(define add-byte* (llvm:extract-function jit add-byte))
(define max-byte* (llvm:extract-function jit max-byte))


(llvm:generic->double (add-float* (llvm:double->generic 1) (llvm:double->generic 2)))
(llvm:generic->int (add-byte* (llvm:int8->generic 10) (llvm:int8->generic 3)))
(llvm:generic->int (max-byte* (llvm:int8->generic 3) (llvm:int8->generic 10)))
