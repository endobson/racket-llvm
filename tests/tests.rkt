#lang racket/base
(require
  "../simple.rkt"
  "../private/short.rkt")


(ll
 (llvm-define-module module
  #:exports (add-float add-byte)


 (llvm-define-function add-float
  ((left float) (right float) -> float)
  (return (+ left right)))

 (llvm-define-function add-byte
  ((left i8) (right i8) -> i8)
  (return (+ left right)))))

 
  
(llvm-assert-module-valid module)
(void (llvm:optimize-module module))
(define jit (llvm:create-jit module))



(define add-float* (llvm:extract-function jit add-float))
(define add-byte* (llvm:extract-function jit add-byte))


(llvm:generic->double (add-float* (llvm:double->generic 1) (llvm:double->generic 2)))
(llvm:generic->int (add-byte* (llvm:int8->generic 10) (llvm:int8->generic 3)))
