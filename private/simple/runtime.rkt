#lang racket/base

(require
  "../ffi/safe.rkt"
  "parameters.rkt"
  "values.rkt"
  "globals.rkt"
  "predicates.rkt"
  racket/contract
  unstable/contract)
(require (only-in ffi/unsafe cpointer?) "types.rkt")


(provide
 (contract-out

  (llvm:optimize-module (->* () (llvm:module?) boolean?))
  (llvm:create-jit (->* () (llvm:module? #:level (one-of/c 0 1 2 3)) llvm:execution-engine?))
  (llvm:extract-function (-> llvm:execution-engine?
                             llvm:value?
                             procedure?))
  (llvm:extract-global   (-> llvm:execution-engine?
                             llvm:global-variable?
                             cpointer?))
  (llvm:execution-engine? predicate/c)))


;TODO implement
(define (llvm:execution-engine? v) #t)

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
