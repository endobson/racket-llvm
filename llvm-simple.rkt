#lang racket

(require "llvm.rkt" "llvm-simple-base.rkt")
(require "llvm-simple-binop.rkt"
         "llvm-simple-comparison.rkt"
         "llvm-simple-cast.rkt"
         "llvm-simple-memory.rkt")






(provide llvm-value-ref? llvm-value/c enter-module/32)
(provide (all-from-out
  "llvm-simple-binop.rkt"
  "llvm-simple-comparison.rkt"
  "llvm-simple-cast.rkt"
  "llvm-simple-memory.rkt"))

(provide/contract
 (llvm-create-module (->* () (string? #:context llvm-context-ref?) llvm-module-ref?))
 (llvm-create-builder (->* () (#:context llvm-context-ref?) llvm-builder-ref?))
 (llvm-add-block-to-function (->* (llvm-value-ref?) (#:context llvm-context-ref? #:name string?) llvm-basic-block-ref?))
 (llvm-add-function (->* (llvm-type-ref? string?) (#:module llvm-module-ref?) llvm-value-ref))
 (llvm-set-position (->* (llvm-basic-block-ref?) (#:builder llvm-builder-ref?) void?))
 (llvm-get-named-function (->* (string?) (#:module llvm-module-ref?) llvm-value-ref?))
 (llvm-get-named-global (->* (string?) (#:module llvm-module-ref?) llvm-value-ref?))
 (llvm-add-global
  (->* (llvm-type-ref? string?) (#:module llvm-module-ref?) llvm-value-ref?))
 (llvm-set-initializer
  (->* (llvm-value-ref? llvm-value-ref?) void?))
 (llvm-set-value-name (-> llvm-value-ref? string? void?))

 (llvm-global-string-ptr (->* (string?) (#:builder llvm-builder-ref? #:name string?) llvm-value-ref?))


 (llvm-ret (->* (llvm-value/c) (#:builder llvm-builder-ref?) llvm-value-ref?))
 (llvm-add-block (->* ()
                      (#:context llvm-context-ref?
                       #:builder llvm-builder-ref?
                       #:name string?) llvm-basic-block-ref?))
 (llvm-cond-br (->* (llvm-boolean/c llvm-basic-block-ref? llvm-basic-block-ref?)
                    (#:builder llvm-builder-ref?)
                    llvm-value-ref?))

 (llvm-get-param (->*  (integer?) (#:function llvm-value-ref?) llvm-value-ref?))
 (llvm-call (->* (llvm-value-ref?)  (#:builder llvm-builder-ref? #:name string?) #:rest (listof llvm-value/c) llvm-value-ref?))



 (llvm-int (->* (integer?) (llvm-type-ref? #:signed? boolean?) llvm-value-ref?))
 (llvm-null (-> llvm-type-ref? llvm-value-ref?))



 (llvm-int-type  (-> llvm-type-ref?))
 (llvm-int1-type  (->* () (#:context llvm-context-ref?) llvm-type-ref?))
 (llvm-int8-type  (->* () (#:context llvm-context-ref?) llvm-type-ref?))
 (llvm-int16-type (->* () (#:context llvm-context-ref?) llvm-type-ref?))
 (llvm-int32-type (->* () (#:context llvm-context-ref?) llvm-type-ref?))
 (llvm-int64-type (->* () (#:context llvm-context-ref?) llvm-type-ref?))

 (llvm-ptr-type (->* (llvm-type-ref?) (#:address-space integer?) llvm-type-ref?))
 (llvm-fun-type (->* (llvm-type-ref?) (#:varargs? boolean?) #:rest (listof llvm-type-ref?) llvm-type-ref?))

 (llvm-void-type  (->* () (#:context llvm-context-ref?) llvm-type-ref?))
 



 )



(define-syntax (enter-module/32 stx)
 (syntax-case stx ()
  ((_ context-expr module-expr body bodies ...)
   #'(let ((context context-expr)
           (module module-expr))
      (parameterize ((current-module module)
                     (current-context context)
                     (current-builder (llvm-create-builder #:context context))
                     (current-integer-type (llvm-int32-type #:context context)))
       body bodies ...)))))


;Coercions

(define (builder->function builder)
  (LLVMGetBasicBlockParent (LLVMGetInsertBlock builder)))

(define (builder->module builder)
  (LLVMGetGlobalParent (builder->function builder)))



;Utility functions


(define (llvm-create-module (name "") #:context (context (current-context)))
 (LLVMModuleCreateWithNameInContext name context))

(define (llvm-create-builder #:context (context (current-context)))
 (LLVMCreateBuilderInContext context))

(define (llvm-add-block-to-function function
           #:context (context (current-context))
           #:name    (name ""))
 (LLVMAppendBasicBlockInContext context function name))

(define (llvm-add-function type name #:module (module (current-module)))
 (LLVMAddFunction module name type))

(define (llvm-get-param index #:function (function (builder->function (current-builder))))
 (LLVMGetParam function index))

(define (llvm-set-position block #:builder (builder (current-builder)))
 (LLVMPositionBuilderAtEnd builder block))

(define (llvm-get-named-function name #:module (module (current-module)))
 (LLVMGetNamedFunction module name))

(define (llvm-get-named-global name #:module (module (current-module)))
 (LLVMGetNamedGlobal module name))


(define (llvm-add-global type name  #:module (module (current-module)))
 (LLVMAddGlobal module type name))

(define (llvm-set-initializer global value)
 (LLVMSetInitializer global value))

(define (llvm-global-string-ptr string #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildGlobalStringPtr builder string name))



(define (llvm-ret val #:builder (builder (current-builder)))
 (LLVMBuildRet builder (value->llvm val)))

(define (llvm-cond-br cond true-block false-block #:builder (builder (current-builder)))
 (LLVMBuildCondBr builder (boolean->llvm cond) true-block false-block))


(define (llvm-set-value-name value name)
 (LLVMSetValueName value name))






(define (llvm-add-block
           #:context (context (current-context))
           #:builder (builder (current-builder))
           #:name    (name ""))
 (LLVMAppendBasicBlockInContext context (builder->function builder) name))

(define (llvm-call function #:builder (builder (current-builder)) #:name (name "") . arguments)
 (LLVMBuildCall builder function (map value->llvm arguments) name))

;Casts



;Integer Creation
(define (llvm-int n (type (current-integer-type)) #:signed? (signed #t))
 (LLVMConstInt type n signed))
(define (llvm-null type)
 (LLVMConstNull type))


(define (llvm-ptr-type type  #:address-space (space 0))
 (LLVMPointerType type space))

(define (llvm-fun-type return-type #:varargs? (varargs #f) . args)
 (LLVMFunctionType return-type args varargs))

(define (llvm-void-type #:context (context (current-context)))
 (LLVMVoidTypeInContext context))

(define (llvm-int-type)
 (current-integer-type))

(define (llvm-int1-type #:context (context (current-context)))
 (LLVMInt1TypeInContext context))

(define (llvm-int8-type #:context (context (current-context)))
 (LLVMInt8TypeInContext context))

(define (llvm-int16-type #:context (context (current-context)))
 (LLVMInt16TypeInContext context))

(define (llvm-int32-type #:context (context (current-context)))
 (LLVMInt32TypeInContext context))

(define (llvm-int64-type #:context (context (current-context)))
 (LLVMInt64TypeInContext context))



