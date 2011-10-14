#lang racket/base

(require
  "../ffi/safe.rkt"
  "../safe/structs.rkt"
  "parameters.rkt"
  "types-values.rkt"
  "globals.rkt"
  racket/contract
  unstable/contract)
(require (only-in ffi/unsafe cpointer?) "types.rkt")


(provide
 (contract-out

  (llvm:optimize-module (->* () (llvm-module-ref?) boolean?))
  (llvm:create-jit (->* () (llvm-module-ref? #:level (one-of/c 0 1 2 3)) llvm-execution-engine-ref?))
  (llvm:extract-function (-> llvm-execution-engine-ref?
                             llvm-value-ref?
                             procedure?))
  (llvm:extract-global   (-> llvm-execution-engine-ref?
                             llvm:global-variable?
                             cpointer?))
  (llvm-execution-engine-ref? predicate/c)))


;TODO implement
(define (llvm-execution-engine-ref? v) #t)

(define (llvm:optimize-module (module (current-module)))
  (LLVMOptimizeModule module))


(define (llvm:create-jit (module (current-module)) #:level (level 3))
  (LLVMCreateJITCompilerForModule module level))

(define (llvm:extract-function ee function)
  (let ((runner (if (equal? (llvm-get-type-kind
                              (llvm-get-return-type
                               (llvm-get-element-type
                                (llvm-type-of function))))
                            'LLVMVoidTypeKind)
                    LLVMRunVoidFunction LLVMRunFunction)))
   (lambda args 
     (runner ee function args))))

(define (llvm:extract-global ee global)
  (LLVMGetPointerToGlobal ee global))
