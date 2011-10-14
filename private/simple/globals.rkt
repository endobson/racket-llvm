#lang racket/base

(require "../ffi/safe.rkt" "../safe/structs.rkt" "util.rkt" "types.rkt"
 "parameters.rkt"  "convertible.rkt" "types-values.rkt")
(require (for-syntax racket/base) racket/contract unstable/contract)


(provide
 (contract-out 

  (llvm:global-variable? predicate/c)
  (llvm:global? predicate/c)


  (llvm-get-named-global (->* (string?) (#:module llvm-module-ref?) llvm-value-ref?))
  (llvm-add-global
   (->* (llvm-type-ref? string?) (#:module llvm-module-ref?) llvm-value-ref?))
  ;TODO look into changing the name of this
  (llvm-global-string-ptr
    (->* (string?) (#:builder llvm-builder-ref? #:name string?) llvm-value-ref?))



  (llvm:get-visibility  (-> llvm:global? visibility/c))
  (llvm:set-visibility! (-> llvm:global? visibility/c void?))

  (llvm:get-linkage  (-> llvm:global? linkage/c))
  (llvm:set-linkage! (-> llvm:global? linkage/c void?))

  (llvm:get-alignment  (-> llvm:global? alignment/c))
  (llvm:set-alignment! (-> llvm:global? alignment/c void?))

  (llvm:get-section  (-> llvm:global? string?))
  (llvm:set-section! (-> llvm:global? string? void?))
  
  (llvm:get-initializer  (-> llvm:global-variable? llvm-value-ref?))
  (llvm:set-initializer! set-initializer/c)

  (llvm:is-thread-local? (-> llvm:global-variable? boolean?))
  (llvm:set-thread-local! (-> llvm:global-variable? boolean? void?))

  (llvm:is-global-constant? (-> llvm:global-variable? boolean?))
  (llvm:set-global-constant! (-> llvm:global-variable? boolean? void?))))

  
;TODO implement
(define (llvm:global? v) #t)
(define (llvm:global-variable? v) #t)


(define (llvm-get-named-global name #:module (module (current-module)))
 (LLVMGetNamedGlobal module name))


(define (llvm-add-global type name #:module (module (current-module)))
 (LLVMAddGlobal module type name))

(define (llvm-global-string-ptr string #:builder (builder (current-builder)) #:name (name ""))
 (LLVMBuildGlobalStringPtr builder string name))






(define set-initializer/c
  (->i ((global-var llvm:global-variable?)
        (value llvm-value/c))
       #:pre/name (global-var value)
        "Matching types"
        (equal? (llvm-get-element-type (llvm-type-of global-var))
                (value->llvm-type value))
        (result void?)))
(define alignment/c
  (or/c 0 power-of-two?))
        


(define-syntax (define-converter stx)
  (syntax-case stx ()
   ((_ name ((sym enum) ...))
    (let ()
     (define (get-id str)
      (datum->syntax #'name
        (string->symbol (format str (syntax-e #'name)))))
 
     (with-syntax ((->enum (get-id "~a->enum"))
                   (enum-> (get-id "enum->~a"))
                   (contract (get-id "~a/c")))
       #'(begin
           (define contract (or/c 'sym ...))
           (define (->enum v)
             (case v
               ((sym) 'enum) ...))
           (define (enum-> v)
             (case v
               ((enum) 'sym) ...))))))))

(define-converter visibility
 ((default LLVMDefaultVisibility)
  (hidden LLVMHiddenVisibility)
  (protected LLVMProtectedVisibility)))


(define-converter linkage
 ((external LLVMExternalLinkage)
  (available-externally LLVMAvailableExternallyLinkage)
  (link-once-any LLVMLinkOnceAnyLinkage)
  (link-once-odr LLVMLinkOnceODRLinkage)
  (weak-any LLVMWeakAnyLinkage)
  (weak-odr LLVMWeakODRLinkage)
  (appending LLVMAppendingLinkage)
  (internal LLVMInternalLinkage)
  (private LLVMPrivateLinkage)
  (dll-import LLVMDLLImportLinkage)
  (dll-export LLVMDLLExportLinkage)
  (external-weak LLVMExternalWeakLinkage)
  (common LLVMCommonLinkage)
  (linker-private LLVMLinkerPrivateLinkage)
  (linker-private-weak LLVMLinkerPrivateWeakLinkage)
  (linker-private-weak-default-auto LLVMLinkerPrivateWeakDefAutoLinkage)))




(define (llvm:get-section global)
  (LLVMGetSection global))
(define (llvm:set-section! global section)
  (LLVMSetSection global section))

(define (llvm:get-alignment global)
  (LLVMGetAlignment global))
(define (llvm:set-alignment! global alignment)
  (LLVMSetAlignment global alignment))

(define (llvm:get-visibility global)
  (enum->visibility (LLVMGetVisibility global)))
(define (llvm:set-visibility! global visibility)
  (LLVMSetVisibility global (visibility->enum visibility)))

(define (llvm:get-linkage global)
  (enum->linkage (LLVMGetLinkage global)))
(define (llvm:set-linkage! global linkage)
  (LLVMSetLinkage global (linkage->enum linkage)))


(define (llvm:get-initializer global-var)
  (LLVMGetInitializer global-var))
(define (llvm:set-initializer! global-var value)
  (LLVMSetInitializer global-var (value->llvm value)))

(define (llvm:is-thread-local? global-val)
  (LLVMIsThreadLocal global-val))
(define (llvm:set-thread-local! global-val bool)
  (LLVMSetThreadLocal global-val bool))

(define (llvm:is-global-constant? global-val)
  (LLVMIsGlobalConstant global-val))
(define (llvm:set-global-constant! global-val bool)
  (LLVMSetGlobalConstant global-val bool))


