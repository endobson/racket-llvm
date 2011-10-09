#lang racket/base

(require "base.rkt" "../ffi/safe.rkt" "../safe/structs.rkt" racket/contract)


(provide/contract

  (llvm:optimize-module (->* () (llvm-module-ref?) boolean?))
  (llvm:create-jit (->* () (llvm-module-ref? #:level (one-of/c 0 1 2 3)) llvm-execution-engine-ref?))
  (llvm:extract-function (-> llvm-execution-engine-ref?
                             llvm-value-ref?
                             procedure?))

  )


(define (llvm:optimize-module (module (current-module)))
  (LLVMOptimizeModule module))


(define (llvm:create-jit (module (current-module)) #:level (level 3))
  (LLVMCreateJITCompilerForModule module level))

(define (llvm:extract-function ee function)
  (lambda args 
    (LLVMRunFunction ee function args)))
