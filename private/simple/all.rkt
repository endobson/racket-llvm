#lang racket

(require
  "../ffi/safe.rkt"
  "../safe/structs.rkt"
  "util.rkt"
  "aggregate.rkt"
  "base.rkt"
  "binop.rkt"
  "comparison.rkt"
  "cast.rkt"
  "extra.rkt"
  "generic.rkt"
  "globals.rkt"
  "memory.rkt"
  "runtime.rkt"
  "types.rkt")




(provide
  llvm-value-ref?
  llvm-value/c
  enter-module/32
  define-basic-block
  llvm-type-of
  llvm-get-type-kind
  llvm-get-return-type
  llvm-gep-type
  llvm-get-element-type
  llvm-null
  llvm-get-undef)
  





(provide (all-from-out
  "aggregate.rkt"
  "binop.rkt"
  "comparison.rkt"
  "cast.rkt"
  "extra.rkt"
  "generic.rkt"
  "globals.rkt"
  "memory.rkt"
  "runtime.rkt"
  "types.rkt"))

(provide/contract
 (llvm-create-context (-> llvm-context-ref?))
 (llvm-create-module (->* () (string? #:context llvm-context-ref?) llvm-module-ref?))
 (llvm-create-module-from-bitcode-file
  (->* (path-string?) (#:context llvm-context-ref?) llvm-module-ref?))
 (llvm-create-builder (->* () (#:context llvm-context-ref?) llvm-builder-ref?))
 (llvm-add-block-to-function (->* (llvm-value-ref?) (#:context llvm-context-ref? #:name string?) llvm-basic-block-ref?))
 (llvm-add-function (->* (llvm-function-type-ref? string?) (#:module llvm-module-ref?) llvm-value-ref?))
 (llvm-set-position (->* (llvm-basic-block-ref?) (#:builder llvm-builder-ref?) void?))
 (llvm-get-insert-block (->* () (#:builder llvm-builder-ref?) llvm-basic-block-ref?))


 (llvm-get-named-function (->* (string?) (#:module llvm-module-ref?) llvm-value-ref?))
 (llvm-get-named-global (->* (string?) (#:module llvm-module-ref?) llvm-value-ref?))
 (llvm-add-global
  (->* (llvm-type-ref? string?) (#:module llvm-module-ref?) llvm-value-ref?))
 (llvm-set-initializer
  (->* (llvm-value-ref? llvm-value-ref?) void?))
 (llvm-set-value-name (-> llvm-value-ref? string? void?))

 (llvm-global-string-ptr (->* (string?) (#:builder llvm-builder-ref? #:name string?) llvm-value-ref?))

 (llvm-verify-module (->* () (llvm-module-ref?) (or/c #f string?)))
 (llvm-assert-module-valid (->* () (llvm-module-ref?) void?))
 (llvm-module-description (->* () (llvm-module-ref?) string?))

 (llvm-write-bitcode-to-file (case-> (-> path-string? void?) (-> llvm-module-ref? path-string? void?)))


 (llvm-ret      (->* () (llvm-value/c #:builder llvm-builder-ref?) llvm-value-ref?))
 (llvm-add-block (->* ()
                      (#:context llvm-context-ref?
                       #:builder llvm-builder-ref?
                       #:name string?) llvm-basic-block-ref?))
 (llvm-cond-br (->* (llvm-boolean/c llvm-basic-block-ref? llvm-basic-block-ref?)
                    (#:builder llvm-builder-ref?)
                    llvm-value-ref?))
 (llvm-br (->* (llvm-basic-block-ref?)
                    (#:builder llvm-builder-ref?)
                    llvm-value-ref?))

 (llvm-phi (->* (llvm-type-ref?)
                  (#:builder llvm-builder-ref?
                   #:name string?)
                  llvm-value-ref?))

 (llvm-add-incoming
   (->* (llvm-value-ref?)
        ()
        #:rest (listof (or/c (cons/c llvm-value/c llvm-basic-block-ref?) llvm-value-ref?))
        void?))


 (llvm-get-param (->*  (integer?) (#:function llvm-value-ref?) llvm-value-ref?))
 (llvm-call (->* (llvm-value-ref?)  (#:builder llvm-builder-ref? #:name string?) #:rest (listof llvm-value/c) llvm-value-ref?))
 (llvm-call* (->* (llvm-value-ref?)  (#:builder llvm-builder-ref? #:name string?) #:rest (list*/c llvm-value/c) llvm-value-ref?))



 (llvm-int (->* (integer?) (llvm-integer-type-ref? #:signed? boolean?) llvm-value-ref?))
 (llvm-struct (->* () (#:context llvm-context-ref? #:packed boolean?) #:rest (listof llvm-value/c) llvm-value-ref?)) 
 (llvm-named-struct (->* (llvm-named-struct-type-ref?) #:rest (listof llvm-value/c) llvm-value-ref?)) ;TODO Make contract tighter 



 (llvm-int-type  (-> llvm-integer-type-ref?))
 (llvm-int1-type  (->* () (#:context llvm-context-ref?) llvm-integer-type-ref?))
 (llvm-int8-type  (->* () (#:context llvm-context-ref?) llvm-integer-type-ref?))
 (llvm-int16-type (->* () (#:context llvm-context-ref?) llvm-integer-type-ref?))
 (llvm-int32-type (->* () (#:context llvm-context-ref?) llvm-integer-type-ref?))
 (llvm-int64-type (->* () (#:context llvm-context-ref?) llvm-integer-type-ref?))


 (llvm-single-type  (->* () (#:context llvm-context-ref?) llvm-float-type-ref?))
 (llvm-double-type  (->* () (#:context llvm-context-ref?) llvm-float-type-ref?))
 (llvm-fp128-type (->* () (#:context llvm-context-ref?) llvm-float-type-ref?))
 (llvm-x86-fp80-type (->* () (#:context llvm-context-ref?) llvm-float-type-ref?))
 (llvm-ppc-fp128-type (->* () (#:context llvm-context-ref?) llvm-float-type-ref?))


 (llvm-struct-set-body! (->* (llvm-unset-named-struct-type-ref?) (#:packed boolean?) #:rest (listof llvm-type-ref?) void?))
 (llvm-struct-set-body*! (->* (llvm-unset-named-struct-type-ref?) (#:packed boolean?) #:rest (list*/c llvm-type-ref?) void?))

 (llvm-pointer-type (->* (llvm-type-ref?) (#:address-space integer?) llvm-pointer-type-ref?))
 (llvm-function-type (->* (llvm-type-ref?) (#:varargs boolean?) #:rest (listof llvm-type-ref?) llvm-function-type-ref?))
 (llvm-function-type* (->* (llvm-type-ref?) (#:varargs boolean?) #:rest (list*/c llvm-type-ref?) llvm-function-type-ref?))
 (llvm-void-type  (->* () (#:context llvm-context-ref?) llvm-void-type-ref?))
 



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

(define-syntax (define-basic-block stx)
 (syntax-case stx ()
  ((_ id ...)
   (with-syntax ((((id name) ...)
    (for/list ((id (syntax->list #'(id ...))))
     (list id (symbol->string (syntax-e id))))))
    #`(begin
       (define id (llvm-add-block #:name name)) ...)))))


;Coercions

(define (builder->function builder)
  (LLVMGetBasicBlockParent (LLVMGetInsertBlock builder)))

(define (builder->module builder)
  (LLVMGetGlobalParent (builder->function builder)))



;Utility functions


(define (llvm-create-module (name "") #:context (context (current-context)))
 (LLVMModuleCreateWithNameInContext name context))

(define (llvm-create-module-from-bitcode-file path #:context (context (current-context)))
 (LLVMParseBitcodeInContext
  context
  (LLVMCreateMemoryBufferWithContentsOfFile path)))

(define (llvm-create-context)
 (LLVMContextCreate))

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

(define (llvm-get-insert-block #:builder (builder (current-builder)))
 (LLVMGetInsertBlock builder))


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



(define (llvm-global-string-ptr string #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildGlobalStringPtr builder string name))



(define (llvm-ret (val (void)) #:builder (builder (current-builder)))
  (if (void? val)
    (LLVMBuildRetVoid builder)
    (LLVMBuildRet builder (value->llvm val))))



(define (llvm-cond-br cond true-block false-block #:builder (builder (current-builder)))
 (LLVMBuildCondBr builder (boolean->llvm cond) true-block false-block))

(define (llvm-br block #:builder (builder (current-builder)))
 (LLVMBuildBr builder block))


(define (llvm-phi type #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildPhi builder type name))

(define (llvm-add-incoming phi . input-values)
 (define (extract-value v)
  (if (cons? v) (value->llvm (car v)) v))
 (define (extract-block v)
  (if (cons? v) (cdr v)  (LLVMGetInstructionParent v)))

 (define values (map extract-value input-values))
 (define blocks (map extract-block input-values))
  
 (LLVMAddIncoming phi values blocks))
 


(define (llvm-set-value-name value name)
 (LLVMSetValueName value name))






(define (llvm-add-block
           #:context (context (current-context))
           #:builder (builder (current-builder))
           #:name    (name ""))
 (LLVMAppendBasicBlockInContext context (builder->function builder) name))

(define (llvm-call function #:builder (builder (current-builder)) #:name (name "") . arguments)
 (LLVMBuildCall builder function (map value->llvm arguments) name))

(define (llvm-call* function #:builder (builder (current-builder)) #:name (name "") . arguments)
 (LLVMBuildCall builder function (map value->llvm (apply list* arguments)) name))

;Casts


(define (llvm-struct #:context (context (current-context)) #:packed (packed #f) . args)
 (LLVMConstStructInContext context (map value->llvm args) packed))

(define (llvm-named-struct ty . args)
 (LLVMConstNamedStruct ty (map value->llvm args)))


;Integer Creation
(define (llvm-int n (type (current-integer-type)) #:signed? (signed #t))
 (LLVMConstInt type n signed))






(define (llvm-struct-set-body! #:packed (packed #f)
                              type . types)
 (LLVMStructSetBody type types packed))

(define (llvm-struct-set-body*! #:packed (packed #f)
                              type . types)
 (LLVMStructSetBody type (apply list* types) packed))




(define (llvm-pointer-type type  #:address-space (space 0))
 (LLVMPointerType type space))

(define (llvm-function-type return-type #:varargs (varargs #f) . args)
 (LLVMFunctionType return-type args varargs))

(define (llvm-function-type* return-type #:varargs (varargs #f) . args)
 (LLVMFunctionType return-type (apply list* args) varargs))


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


(define (llvm-single-type #:context (context (current-context)))
 (LLVMFloatTypeInContext context))

(define (llvm-double-type #:context (context (current-context)))
 (LLVMDoubleTypeInContext context))

(define (llvm-fp128-type #:context (context (current-context)))
 (LLVMFP128TypeInContext context))

(define (llvm-x86-fp80-type #:context (context (current-context)))
 (LLVMX86FP80TypeInContext context))


(define (llvm-ppc-fp128-type #:context (context (current-context)))
 (LLVMPPCFP128TypeInContext context))



;Output



