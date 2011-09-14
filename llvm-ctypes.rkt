#lang racket/base

(require (for-syntax racket/base syntax/parse))
(require racket/stxparam)
(require ffi/unsafe)

(require "llvm-define.rkt")

(provide
  LLVMBool
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
  safe:LLVMContextRef
  safe:LLVMModuleRef
  safe:LLVMTypeRef
  safe:LLVMValueRef
  safe:LLVMBasicBlockRef
  safe:LLVMBuilderRef
  LLVMMessage

  safe:LLVMContextCreator
  safe:LLVMModuleCreator
  safe:LLVMModuleDescriptionMaker
  unsafe:LLVMModuleDescriptionMaker
  (rename-out
   (unsafe:llvm-value-ref? llvm-value-ref?)
   (unsafe:llvm-type-ref? llvm-type-ref?)
   (unsafe:llvm-module-ref? llvm-module-ref?)
   (unsafe:llvm-context-ref? llvm-context-ref?)
   (unsafe:llvm-basic-block-ref? llvm-basic-block-ref?)
   (unsafe:llvm-builder-ref? llvm-builder-ref?)))




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
 (write-llvm-type-ref LLVMGetTypeDescription)
 #:property prop:equal+hash
  (llvm-ref-equal+hash unsafe:llvm-type-ref-pointer unsafe:LLVMTypeRef))
(struct unsafe:llvm-value-ref (pointer)
 #:property prop:custom-print-quotable 'never
 #:property prop:custom-write
 (write-llvm-value-ref LLVMGetValueDescription)
 #:property prop:equal+hash
 (llvm-ref-equal+hash unsafe:llvm-value-ref-pointer unsafe:LLVMValueRef))


(struct safe:llvm-type-ref (pointer context)
 #:property prop:custom-print-quotable 'never
 #:property prop:custom-write
 (write-llvm-type-ref safe:LLVMGetTypeDescription)
 #:property prop:equal+hash
  (llvm-ref-equal+hash safe:llvm-type-ref-pointer safe:LLVMTypeRef))

(struct safe:llvm-value-ref (pointer module)
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


(struct safe:llvm-basic-block-ref (pointer))
(struct safe:llvm-builder-ref (pointer))
;(struct safe:llvm-module-provider-ref (pointer))
;(struct safe:llvm-memory-buffer-ref (pointer))
;(struct safe:llvm-pass-manager-ref (pointer))
;(struct safe:llvm-pass-registry-ref (pointer))
;(struct safe:llvm-use-ref (pointer))
;(struct safe:llvm-target-data-ref (pointer))




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


(define-syntax (make-syntax-parameter-types stx)
  (syntax-parse stx
   ((_ name:id ...)
    #'(begin
        (define-syntax-parameter name
          (let ((unsafe-id (id-prefix 'unsafe: #'name))
                (safe-id   (id-prefix 'safe:   #'name)))
            (lambda (stx)
              (with-syntax ((id 
                             (case (syntax-parameter-value #'llvm-safety)
                               ((unsafe) unsafe-id)
                               ((safe) safe-id)
                               (else (raise-syntax-error #f "Not in an llvm-safety-context"
                                                         stx)))))
                (syntax-case stx ()
                  ((_ . args) #'(id . args))
                  (_ #'id))))))
        ...))))


(make-syntax-parameter-types
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
  LLVMTargetDataRef)


(make-llvm-types
  (unsafe:LLVMContextRef        unsafe:llvm-context-ref         unsafe:llvm-context-ref-pointer)
  (unsafe:LLVMModuleRef         unsafe:llvm-module-ref          unsafe:llvm-module-ref-pointer)
  (unsafe:LLVMTypeRef           unsafe:llvm-type-ref            unsafe:llvm-type-ref-pointer)
  (unsafe:LLVMValueRef          unsafe:llvm-value-ref           unsafe:llvm-value-ref-pointer)
  (unsafe:LLVMBasicBlockRef     unsafe:llvm-basic-block-ref     unsafe:llvm-basic-block-ref-pointer)
  (unsafe:LLVMBuilderRef        unsafe:llvm-builder-ref         unsafe:llvm-builder-ref-pointer)
  (unsafe:LLVMModuleProviderRef unsafe:llvm-module-provider-ref unsafe:llvm-module-provider-ref-pointer)
  (unsafe:LLVMMemoryBufferRef   unsafe:llvm-memory-buffer-ref   unsafe:llvm-memory-buffer-ref-pointer)
  (unsafe:LLVMPassManagerRef    unsafe:llvm-pass-manager-ref    unsafe:llvm-pass-manager-ref-pointer)
  (unsafe:LLVMPassRegistryRef   unsafe:llvm-pass-registry-ref   unsafe:llvm-pass-registry-ref-pointer)
  (unsafe:LLVMUseRef            unsafe:llvm-use-ref             unsafe:llvm-use-ref-pointer)
  (unsafe:LLVMTargetDataRef     unsafe:llvm-target-data-ref     unsafe:llvm-target-data-ref-pointer)
  (safe:LLVMContextRef   safe:llvm-context-ref    safe:llvm-context-ref-pointer))

(make-safe-llvm-types
  (safe:LLVMModuleRef         safe:llvm-module-ref          safe:llvm-module-ref-pointer)
  (safe:LLVMTypeRef           safe:llvm-type-ref            safe:llvm-type-ref-pointer)
  (safe:LLVMValueRef          safe:llvm-value-ref           safe:llvm-value-ref-pointer)
  (safe:LLVMBasicBlockRef     safe:llvm-basic-block-ref     safe:llvm-basic-block-ref-pointer)
  (safe:LLVMBuilderRef        safe:llvm-builder-ref         safe:llvm-builder-ref-pointer))


(define (make-byte-string ptr)
 (let loop ((i 0))
  (cond
   ((zero? (ptr-ref ptr _byte i))
    (make-sized-byte-string ptr i))
   (else (loop (add1 i))))))


(define LLVMMessage 
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
     (LLVMDisposeMessage cptr))))))

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

(define safe:LLVMModuleCreator
 (let ()
   (define-llvm-safe LLVMDisposeModule (_fun safe:LLVMModuleRef -> _void))
   (_fun _string (context : safe:LLVMContextRef)
     -> (ptr : _pointer)
     -> (let ((mod (safe:llvm-module-ref ptr context)))
          (will-register llvm-will-executor mod safe:LLVMDisposeModule)
          mod))))

(define safe:LLVMContextCreator
 (let ()
   (define-llvm-safe LLVMContextDispose (_fun safe:LLVMContextRef -> _void))
   (_fun -> (v : safe:LLVMContextRef)
         -> (begin
             (will-register llvm-will-executor v safe:LLVMContextDispose)
             v))))

(define unsafe:LLVMModuleDescriptionMaker (_fun unsafe:LLVMModuleRef -> _malloc-string))
(define safe:LLVMModuleDescriptionMaker (_fun safe:LLVMModuleRef -> _malloc-string))



;/*===-- Error handling ----------------------------------------------------===*/

(define-llvm LLVMDisposeMessage (_fun _pointer -> _void))



(define-llvm-racket LLVMGetTypeDescription (_fun unsafe:LLVMTypeRef -> _malloc-string))
(define-llvm-racket LLVMGetValueDescription (_fun unsafe:LLVMValueRef -> _malloc-string))

(define-llvm-racket-safe LLVMGetTypeDescription (_fun safe:LLVMTypeRef -> _malloc-string))
(define-llvm-racket-safe LLVMGetValueDescription (_fun safe:LLVMValueRef -> _malloc-string))


