#lang racket/base

(require
  racket/contract
  "../ffi/safe.rkt"
  "predicates.rkt"
  "parameters.rkt")


(provide
 (contract-out
  (llvm-create-context (-> llvm:context?))
  (llvm-create-module
    (->* () (string? #:context llvm:context?) llvm:module?))


  (llvm-verify-module (->* () (llvm:module?) (or/c #f string?)))
  (llvm-assert-module-valid (->* () (llvm:module?) void?))
  (llvm-module-description (->* () (llvm:module?) string?))


  (llvm-create-module-from-bitcode-file
   (->* (path-string?) (#:context llvm:context?) llvm:module?))
  (llvm-write-bitcode-to-file
    (case-> (-> path-string? void?)
            (-> llvm:module? path-string? void?)))))

(define (llvm-create-context)
 (LLVMContextCreate))

(define (llvm-create-module (name "") #:context (context (current-context)))
 (LLVMModuleCreateWithNameInContext name context))

(define (llvm-create-module-from-bitcode-file path #:context (context (current-context)))
 (LLVMParseBitcodeInContext
  context
  (LLVMCreateMemoryBufferWithContentsOfFile path)))

(define (llvm-module-description (module (current-module)))
 (LLVMGetModuleDescription module))

(define llvm-write-bitcode-to-file 
 (case-lambda
  ((module path)
   (let ((err (LLVMWriteBitcodeToFile module path)))
    (unless (zero? err)
     (error 'llvm-write-bitcode-to-file "Error ~a" err))))
  ((path)
   (llvm-write-bitcode-to-file (current-module) path))))

(define (llvm-verify-module (module (current-module)))
 (LLVMVerifyModule module 'LLVMReturnStatusAction))

(define (llvm-assert-module-valid (module (current-module)))
 (let ((err (llvm-verify-module module)))
  (void
   (and err
        (error 'assert-module-valid
               "Bad module: ~n~a" err)))))



