#lang racket/base

(require (for-syntax "../llvm-util-exptime.rkt" racket/base syntax/parse))
(require ffi/unsafe)
(require (prefix-in c: racket/contract))

(require "define.rkt")

(provide
  LLVMBool
  LLVMContextRef
  unsafe:LLVMContextRef
  LLVMModuleRef
  LLVMTypeRef
  LLVMValueRef
  LLVMBasicBlockRef
  LLVMBuilderRef
  LLVMModuleProviderRef
  LLVMMemoryBufferRef
  LLVMPassManagerRef
  LLVMPassRegistryRef
  LLVMUseRef
  LLVMTargetDataRef
  LLVMExecutionEngineRef
  LLVMGenericValueRef
  safe:LLVMContextRef
  safe:LLVMModuleRef
  safe:LLVMTypeRef
  safe:LLVMValueRef
  safe:LLVMBasicBlockRef
  safe:LLVMBuilderRef
  safe:LLVMExecutionEngineRef
  LLVMMessage
  set-safe:llvm-builder-ref-module!
  safe:llvm-builder-ref-module
  safe:llvm-basic-block-ref-module
  (c:contract-out
   (safe:llvm-value-ref
    (c:-> cpointer? (c:or/c safe:llvm-module-ref? safe:llvm-context-ref?)
        safe:llvm-value-ref?))
   (safe:llvm-basic-block-ref
    (c:-> cpointer? safe:llvm-module-ref? safe:llvm-basic-block-ref?)))
  safe:llvm-value-ref-owner
  safe:llvm-type-ref
  safe:llvm-type-ref-context
  safe:llvm-module-ref-context
  safe:llvm-module-ref-pointer

  safe:LLVMContextCreator
  safe:LLVMModuleCreator
  safe:LLVMModuleCreatorFromBitcode
  safe:LLVMMemoryBufferCreatorFromFile
  safe:LLVMBuilderCreator
  safe:LLVMModuleDescriptionMaker
  safe:LLVMExecutionEngineCreator
  safe:LLVMJITCreator
  safe:LLVMGenericValueRef
  unsafe:LLVMModuleDescriptionMaker

  _non-null-string
  unsafe:llvm-value-ref?
  unsafe:llvm-type-ref?
  unsafe:llvm-module-ref?
  unsafe:llvm-context-ref?
  unsafe:llvm-basic-block-ref?
  unsafe:llvm-builder-ref?

  safe:llvm-value-ref?
  safe:llvm-type-ref?
  safe:llvm-module-ref?
  safe:llvm-context-ref?
  safe:llvm-basic-block-ref?
  safe:llvm-builder-ref?)




(define llvm-will-executor (make-will-executor))
(void (thread (lambda ()
                (let loop ()
                  (will-execute llvm-will-executor)
                  (loop)))))



(define LLVMBool _bool)


(define-syntax-rule (write-llvm-type-ref get-desc)
 (lambda (val port mode)
  (if (equal? 0 mode)
      (write-string "(llvm-type-ref " port)
      (write-string "#<llvm-type-ref: " port))
  (write-string (get-desc val) port)
  (if (equal? 0 mode)
      (write-string ")" port)
      (write-string ">" port))))

;TODO add pred
(define-syntax-rule (llvm-ref-equal+hash acc type)
 (list
  (lambda (left right =?)
   (ptr-equal? (acc left)
               (acc right)))
  (lambda (val recur)
   (recur (cast val type _intptr)))
  (lambda (val recur)
   (recur (cast val type _intptr)))))

(define-syntax-rule (write-llvm-value-ref desc)
  (lambda (val port mode)
   (if (equal? 0 mode)
       (write-string "(llvm-value-ref " port)
       (write-string "#<llvm-value-ref: " port))
   (write-string (desc val) port)
   (if (equal? 0 mode)
       (write-string ")" port)
       (write-string ">" port))))



(struct unsafe:llvm-context-ref (pointer))
(struct unsafe:llvm-module-ref (pointer))

(struct safe:llvm-context-ref (pointer))
(struct safe:llvm-module-ref (pointer context))



(struct unsafe:llvm-type-ref (pointer)
 #:property prop:custom-print-quotable 'never
 #:property prop:custom-write
 (write-llvm-type-ref unsafe:LLVMGetTypeDescription)
 #:property prop:equal+hash
  (llvm-ref-equal+hash unsafe:llvm-type-ref-pointer unsafe:LLVMTypeRef))
(struct unsafe:llvm-value-ref (pointer)
 #:property prop:custom-print-quotable 'never
 #:property prop:custom-write
 (write-llvm-value-ref unsafe:LLVMGetValueDescription)
 #:property prop:equal+hash
 (llvm-ref-equal+hash unsafe:llvm-value-ref-pointer unsafe:LLVMValueRef))


(struct safe:llvm-type-ref (pointer context)
 #:property prop:custom-print-quotable 'never
 #:property prop:custom-write
 (write-llvm-type-ref safe:LLVMGetTypeDescription)
 #:property prop:equal+hash
  (llvm-ref-equal+hash safe:llvm-type-ref-pointer safe:LLVMTypeRef))

(struct safe:llvm-value-ref (pointer owner)
 #:property prop:custom-print-quotable 'never
 #:property prop:custom-write
 (write-llvm-value-ref safe:LLVMGetValueDescription)
 #:property prop:equal+hash
 (llvm-ref-equal+hash safe:llvm-value-ref-pointer safe:LLVMValueRef))


(struct unsafe:llvm-basic-block-ref (pointer))
(struct unsafe:llvm-builder-ref (pointer))
(struct unsafe:llvm-module-provider-ref (pointer))
(struct unsafe:llvm-memory-buffer-ref (pointer))
(struct unsafe:llvm-pass-manager-ref (pointer))
(struct unsafe:llvm-pass-registry-ref (pointer))
(struct unsafe:llvm-use-ref (pointer))
(struct unsafe:llvm-target-data-ref (pointer))
(struct unsafe:llvm-execution-engine-ref (pointer))
(struct unsafe:llvm-generic-value-ref (pointer))


(struct safe:llvm-basic-block-ref (pointer module))
(struct safe:llvm-builder-ref (pointer context (module #:mutable)))
;(struct safe:llvm-module-provider-ref (pointer))
(struct safe:llvm-memory-buffer-ref (pointer))
;(struct safe:llvm-pass-manager-ref (pointer))
;(struct safe:llvm-pass-registry-ref (pointer))
;(struct safe:llvm-use-ref (pointer))
;(struct safe:llvm-target-data-ref (pointer))
(struct safe:llvm-execution-engine-ref (pointer modules))
(struct safe:llvm-generic-value-ref (pointer))




(define-syntax (make-llvm-types stx)
  (define-syntax-class id-pair
   (pattern (type-name:id cons:id acc:id)))
  (syntax-parse stx
   ((_ p:id-pair ...)
    #'(begin
        (define p.type-name
          (make-ctype _pointer p.acc p.cons))
        ...))))

(define-syntax (make-safe-llvm-types stx)
  (define-syntax-class id-pair
   (pattern (type-name:id cons:id acc:id)))
  (syntax-parse stx
   ((_ p:id-pair ...)
    #'(begin
        (define p.type-name
          (make-ctype _pointer p.acc
                      (lambda (ptr)
                        (error 'p.type-name "Cannot be used to convert from c"))))
        ...))))


(define-syntax (make-types stx)
  (syntax-parse stx
   ((_ name:id ...)
    (with-syntax (((unsafe-id ...) (map (lambda (id) (id-prefix 'unsafe: id))
                                        (syntax->list #'(name ...)))))
     #'(begin
        (define name unsafe-id) ...)))))



(make-llvm-types
  (unsafe:LLVMContextRef         unsafe:llvm-context-ref          unsafe:llvm-context-ref-pointer)
  (unsafe:LLVMModuleRef          unsafe:llvm-module-ref           unsafe:llvm-module-ref-pointer)
  (unsafe:LLVMTypeRef            unsafe:llvm-type-ref             unsafe:llvm-type-ref-pointer)
  (unsafe:LLVMValueRef           unsafe:llvm-value-ref            unsafe:llvm-value-ref-pointer)
  (unsafe:LLVMBasicBlockRef      unsafe:llvm-basic-block-ref      unsafe:llvm-basic-block-ref-pointer)
  (unsafe:LLVMBuilderRef         unsafe:llvm-builder-ref          unsafe:llvm-builder-ref-pointer)
  (unsafe:LLVMModuleProviderRef  unsafe:llvm-module-provider-ref  unsafe:llvm-module-provider-ref-pointer)
  (unsafe:LLVMMemoryBufferRef    unsafe:llvm-memory-buffer-ref    unsafe:llvm-memory-buffer-ref-pointer)
  (unsafe:LLVMPassManagerRef     unsafe:llvm-pass-manager-ref     unsafe:llvm-pass-manager-ref-pointer)
  (unsafe:LLVMPassRegistryRef    unsafe:llvm-pass-registry-ref    unsafe:llvm-pass-registry-ref-pointer)
  (unsafe:LLVMUseRef             unsafe:llvm-use-ref              unsafe:llvm-use-ref-pointer)
  (unsafe:LLVMTargetDataRef      unsafe:llvm-target-data-ref      unsafe:llvm-target-data-ref-pointer)
  (unsafe:LLVMExecutionEngineRef unsafe:llvm-execution-engine-ref unsafe:llvm-execution-engine-ref-pointer)
  (unsafe:LLVMGenericValueRef    unsafe:llvm-generic-value-ref    unsafe:llvm-generic-value-ref-pointer)
  (safe:LLVMContextRef   safe:llvm-context-ref    safe:llvm-context-ref-pointer))

(make-types
  LLVMContextRef
  LLVMModuleRef
  LLVMTypeRef
  LLVMValueRef
  LLVMBasicBlockRef
  LLVMBuilderRef
  LLVMModuleProviderRef
  LLVMMemoryBufferRef
  LLVMPassManagerRef
  LLVMPassRegistryRef
  LLVMUseRef
  LLVMTargetDataRef
  LLVMGenericValueRef
  LLVMExecutionEngineRef)



(make-safe-llvm-types
  (safe:LLVMModuleRef          safe:llvm-module-ref           safe:llvm-module-ref-pointer)
  (safe:LLVMMemoryBufferRef    safe:llvm-memory-buffer-ref    safe:llvm-memory-buffer-ref-pointer)
  (safe:LLVMTypeRef            safe:llvm-type-ref             safe:llvm-type-ref-pointer)
  (safe:LLVMValueRef           safe:llvm-value-ref            safe:llvm-value-ref-pointer)
  (safe:LLVMBasicBlockRef      safe:llvm-basic-block-ref      safe:llvm-basic-block-ref-pointer)
  (safe:LLVMBuilderRef         safe:llvm-builder-ref          safe:llvm-builder-ref-pointer)
  (safe:LLVMExecutionEngineRef safe:llvm-execution-engine-ref safe:llvm-execution-engine-ref-pointer))

(define safe:LLVMGenericValueRef
 (let ()
  (define-llvm-safe LLVMDisposeGenericValue (_fun _pointer -> _void))
  (make-ctype _pointer
          safe:llvm-generic-value-ref-pointer
          (lambda (ptr)
            (let ((v (safe:llvm-generic-value-ref ptr)))
              (will-register llvm-will-executor v 
                             (lambda (v) (safe:LLVMDisposeGenericValue (safe:llvm-generic-value-ref-pointer v))))
              v)))))


(define (make-byte-string ptr)
 (let loop ((i 0))
  (cond
   ((zero? (ptr-ref ptr _byte i))
    (make-sized-byte-string ptr i))
   (else (loop (add1 i))))))


(define LLVMMessage 
 (let ()
  (define-llvm-unsafe LLVMDisposeMessage (_fun _pointer -> _void))
  (make-ctype
   _pointer
   (lambda (scheme)
    (when scheme
     (error 'LLVMMessage "Cannot Convert non null pointers to C"))
    #f)
   (lambda (cptr)
    (and cptr
     (begin0
      (bytes->string/utf-8 (make-byte-string cptr))
      (unsafe:LLVMDisposeMessage cptr)))))))

(define _non-null-string
 (make-ctype
  _string
  (lambda (scheme)
    (or scheme
      (error '_non-null-string "Cannot convert #f")))
  (lambda (c)
    (or c
      (error '_non-null-string "Cannot convert #f")))))


(define _malloc-string
 (make-ctype
  _pointer
  (lambda (scheme)
   (when scheme
    (error 'LLVMMessage "Cannot Convert non null pointers to C"))
   #f)
  (lambda (cptr)
   (and cptr
    (begin0
     (bytes->string/utf-8 (make-byte-string cptr))
     (free cptr))))))

(define-values (safe:LLVMModuleCreator safe:LLVMModuleCreatorFromBitcode)
 (let ()
   (define-llvm-safe LLVMDisposeModule (_fun safe:LLVMModuleRef -> _void))
   (values
     (_fun _string (context : safe:LLVMContextRef)
       -> (ptr : _pointer)
       -> (let ((mod (safe:llvm-module-ref ptr context)))
            (will-register llvm-will-executor mod safe:LLVMDisposeModule)
            mod))
     (_fun (context buffer) ::
           (context : safe:LLVMContextRef)
           (buffer : safe:LLVMMemoryBufferRef)
           (module-ptr : (_ptr o _pointer))
           (message : (_ptr io LLVMMessage) = #f)
           ->
           (err : LLVMBool)
           ->
           (if err message
            (let ((mod (safe:llvm-module-ref module-ptr context)))
                        (will-register llvm-will-executor mod safe:LLVMDisposeModule)
                        mod))))))
             


(define safe:LLVMContextCreator
 (let ()
   (define-llvm-safe LLVMContextDispose (_fun safe:LLVMContextRef -> _void))
   (_fun -> (v : safe:LLVMContextRef)
         -> (begin
             (will-register llvm-will-executor v safe:LLVMContextDispose)
             v))))

(define safe:LLVMBuilderCreator
 (let ()
   (define-llvm-safe LLVMDisposeBuilder (_fun safe:LLVMBuilderRef -> _void))
   (_fun (context : safe:LLVMContextRef)
     -> (ptr : _pointer)
     -> (let ((build (safe:llvm-builder-ref ptr context #f)))
          (will-register llvm-will-executor build safe:LLVMDisposeBuilder)
          build))))

(define safe:LLVMMemoryBufferCreatorFromFile
 (let ()
   (define-llvm-safe LLVMDisposeMemoryBuffer (_fun safe:LLVMMemoryBufferRef -> _void))

   (_fun (path) ::
          (path : _string)
          (buffer-ptr : (_ptr o _pointer))
          (message : (_ptr io LLVMMessage) = #f)
          ->
          (ans : LLVMBool)
          ->
          (if ans message
            (let ((buffer (safe:llvm-memory-buffer-ref buffer-ptr)))
              (will-register llvm-will-executor buffer safe:LLVMDisposeMemoryBuffer)
              buffer)))))

(define safe:LLVMExecutionEngineCreator
 (let ()
  (define-llvm-safe LLVMDisposeExecutionEngine (_fun safe:LLVMExecutionEngineRef -> _void))
  (define-llvm-safe LLVMRemoveModule 
    (_fun (ee module) ::
       (ee : safe:LLVMExecutionEngineRef)
       (module : safe:LLVMModuleRef)
       (out-ptr : (_ptr o _pointer))
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message 
         (let ((in-ptr (safe:llvm-module-ref-pointer module)))
           (unless (equal? in-ptr out-ptr)
             (error 'LLVMRemoveModule "Returned module ~a is not the same as argument module ~a" out-ptr in-ptr))
           module))))
  (define (engine-disposer engine)
    (for ((mod (safe:llvm-execution-engine-ref-modules engine)))
      (let ((mod (safe:LLVMRemoveModule engine mod)))
        (when (string? mod)
          (error 'engine-disposer "Error when trying to clean up a module of the exection engine: ~a" mod))))
    (safe:LLVMDisposeExecutionEngine engine))
  (_fun (module) ::
        (execution-ptr : (_ptr o _pointer))
        (module : safe:LLVMModuleRef)
        (message : (_ptr io LLVMMessage) = #f)
        ->
        (err : LLVMBool)
        ->
         (if err message 
           (let ((execution-engine (safe:llvm-execution-engine-ref execution-ptr (list module))))
             (will-register llvm-will-executor execution-engine engine-disposer)
             execution-engine)))))
         
(define safe:LLVMJITCreator
 (let ()
  (define-llvm-safe LLVMDisposeExecutionEngine (_fun safe:LLVMExecutionEngineRef -> _void))
  (define-llvm-safe LLVMRemoveModule 
    (_fun (ee module) ::
       (ee : safe:LLVMExecutionEngineRef)
       (module : safe:LLVMModuleRef)
       (out-ptr : (_ptr o _pointer))
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message 
         (let ((in-ptr (safe:llvm-module-ref-pointer module)))
           (unless (equal? in-ptr out-ptr)
             (error 'LLVMRemoveModule "Returned module ~a is not the same as argument module ~a" out-ptr in-ptr))
           module))))
  (define (engine-disposer engine)
    (for ((mod (safe:llvm-execution-engine-ref-modules engine)))
      (let ((mod (safe:LLVMRemoveModule engine mod)))
        (when (string? mod)
          (error 'engine-disposer "Error when trying to clean up a module of the exection engine: ~a" mod))))
    (safe:LLVMDisposeExecutionEngine engine))
  (_fun (module opt) ::
        (execution-ptr : (_ptr o _pointer))
        (module : safe:LLVMModuleRef)
        (opt : _uint)
        (message : (_ptr io LLVMMessage) = #f)
        ->
        (err : LLVMBool)
        ->
         (if err message 
           (let ((execution-engine (safe:llvm-execution-engine-ref execution-ptr (list module))))
             (will-register llvm-will-executor execution-engine engine-disposer)
             execution-engine)))))




(define unsafe:LLVMModuleDescriptionMaker (_fun unsafe:LLVMModuleRef -> _malloc-string))
(define safe:LLVMModuleDescriptionMaker (_fun safe:LLVMModuleRef -> _malloc-string))



;/*===-- Error handling ----------------------------------------------------===*/




(define-llvm-racket-unsafe LLVMGetTypeDescription (_fun unsafe:LLVMTypeRef -> _malloc-string))
(define-llvm-racket-unsafe LLVMGetValueDescription (_fun unsafe:LLVMValueRef -> _malloc-string))

(define-llvm-racket-safe LLVMGetTypeDescription (_fun safe:LLVMTypeRef -> _malloc-string))
(define-llvm-racket-safe LLVMGetValueDescription (_fun safe:LLVMValueRef -> _malloc-string))


