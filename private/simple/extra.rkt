#lang racket/base

(require "base.rkt" "memory.rkt" "globals.rkt" racket/list)
(require "../ffi/safe.rkt")
(require (for-syntax racket/base syntax/parse racket/syntax racket/list syntax/kerncase))


(provide llvm-if llvm-for llvm-when
         llvm-define-mutable llvm-define-reference llvm-declare-function
         llvm-define-function llvm-define-module
         llvm-implement-function)

(define (llvm-get-insert-block #:builder (builder (current-builder)))
 (LLVMGetInsertBlock builder))

(define-syntax (define-basic-block stx)
 (syntax-case stx ()
  ((_ id ...)
   (with-syntax ((((id name) ...)
    (for/list ((id (syntax->list #'(id ...))))
     (list id (symbol->string (syntax-e id))))))
    #`(begin
       (define id (llvm-add-block #:name name)) ...)))))

(define (llvm-add-block
           #:context (context (current-context))
           #:builder (builder (current-builder))
           #:name    (name ""))
 (LLVMAppendBasicBlockInContext context (builder->function builder) name))


(define (llvm-get-param index #:function (function (builder->function (current-builder))))
 (LLVMGetParam function index))

(define (llvm-add-block-to-function function
           #:context (context (current-context))
           #:name    (name ""))
 (LLVMAppendBasicBlockInContext context function name))


(define (llvm-add-function type name #:module (module (current-module)))
 (LLVMAddFunction module name type))

(define (builder->function builder)
  (LLVMGetBasicBlockParent (LLVMGetInsertBlock builder)))

(define (llvm-br block #:builder (builder (current-builder)))
 (LLVMBuildBr builder block))

(define (llvm-set-position block #:builder (builder (current-builder)))
 (LLVMPositionBuilderAtEnd builder block))


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
 

(define (llvm-cond-br cond true-block false-block #:builder (builder (current-builder)))
 (LLVMBuildCondBr builder (boolean->llvm cond) true-block false-block))

(define (llvm-get-terminator (block (llvm-get-insert-block)))
  (LLVMGetBasicBlockTerminator block))

(define (llvm-function-type return-type #:varargs (varargs #f) . args)
 (LLVMFunctionType return-type args varargs))


(define (llvm-create-context)
 (LLVMContextCreate))


(define (llvm-create-module (name "") #:context (context (current-context)))
 (LLVMModuleCreateWithNameInContext name context))


(define (llvm-int32-type #:context (context (current-context)))
 (LLVMInt32TypeInContext context))

(define (llvm-double-type #:context (context (current-context)))
 (LLVMDoubleTypeInContext context))


(define (llvm-create-builder #:context (context (current-context)))
 (LLVMCreateBuilderInContext context))




(define-syntax (llvm-when stx)
 (syntax-parse stx
  ((_ cond-expr:expr then-expr:expr)
   #'(let ()
       (define-basic-block then-block end-block)
       (define br-val cond-expr)
       (llvm-cond-br br-val then-block end-block)

       (llvm-set-position then-block)
       then-expr
       (unless (llvm-get-terminator)
         (llvm-br end-block))

       (llvm-set-position end-block)
       (void)))))



(define-syntax (llvm-if stx)
 (syntax-parse stx
  ((_ cond-expr:expr then-expr:expr else-expr:expr)
   #'(let ()
       (define-basic-block then-block else-block merge-block)
       (define br-val cond-expr)
       (llvm-cond-br br-val then-block else-block)

       (llvm-set-position then-block)
       (define then-val then-expr)
       (define then-end-block (llvm-get-insert-block))
       (llvm-br merge-block)
       
       (llvm-set-position else-block)
       (define else-val else-expr)
       (define else-end-block (llvm-get-insert-block))
       (llvm-br merge-block)

       (llvm-set-position merge-block)
       (define merge-val (llvm-phi (value->llvm-type else-val)))
       (llvm-add-incoming merge-val
            (cons then-val then-end-block)
            (cons else-val else-end-block))
       merge-val))))

(define-syntax (llvm-for stx)
  (syntax-parse stx
   ((_ var:id init:expr test:expr inc:expr bodies:expr ...)
    #'(let ()
        (define-basic-block start-block body-block finish-block)

        (define init-val init)
        (define entry-block (llvm-get-insert-block))
        (llvm-br start-block)
        
        (llvm-set-position start-block)
        (define var (llvm-phi (value->llvm-type init-val)))
        (define test-val test)
        (llvm-cond-br test-val body-block finish-block)

        (llvm-set-position body-block)
        (let ()
          bodies ...)

        (define inc-val inc)
        (define body-end-block (llvm-get-insert-block))

        (llvm-br start-block)

        (llvm-add-incoming var
            (cons init-val entry-block)
            (cons inc-val body-end-block))
        (llvm-set-position finish-block)
        (void)))))

(define-syntax (llvm-define-mutable stx)
 (syntax-parse stx
  ((_ var:id init:expr)
   #'(begin
      (define initial-value init)
      (define ptr (llvm-alloca (value->llvm-type initial-value)))
      (llvm-store initial-value ptr)
      (llvm-define-reference var ptr)))))

(define-syntax (llvm-define-reference stx)
 (syntax-parse stx
  ((_ var:id init:expr)
   #'(begin
      (define inner init)
      (define-syntax var
       (make-set!-transformer (lambda (stx)
        (syntax-parse stx #:literals (set!)
         ((set! _ body:expr) 
          #'(llvm-store body inner))
         ((_ args (... ...)) (datum->syntax stx (cons #'(llvm-load inner) #'(args (... ...)))))
         (_:id #'(llvm-load inner))))))))))


(define-syntax (llvm-declare-function stx)
 (define-splicing-syntax-class visibility
  (pattern (~seq) #:attr val #f)
  (pattern (~seq #:visibility val:expr)))
 (define-splicing-syntax-class linkage
  (pattern (~seq) #:attr val #f)
  (pattern (~seq #:linkage val:expr)))
 (syntax-parse stx
  ((_ name:id type:expr visibility:visibility linkage:linkage)
   #`(begin
      (define name (llvm-add-function type (symbol->string 'name)))
      #,(if (attribute visibility.val)
          #'(llvm:set-visibility! name visibility.val)
          #'(begin))
      #,(if (attribute linkage.val)
          #'(llvm:set-linkage! name linkage.val)
          #'(begin))))))

(define-syntax (llvm-implement-function stx)
 (syntax-parse stx
  ((_ name:id (args:id ...) bodies:expr ...)
   (with-syntax (((indicies ...)
                  (for/list ((arg (syntax->list #'(args ...)))
                             (i (in-naturals)))
                    i)))
   #'(let ()
      (define entry-block (llvm-add-block-to-function name #:name "entry")) 
      (llvm-set-position entry-block)
      (define args (llvm-get-param indicies)) ...
      bodies ...
      (void))))))

    
(define-syntax (llvm-define-function stx)
 (define-splicing-syntax-class visibility
  (pattern (~seq))
  (pattern (~seq #:visibility val:expr)))
 (define-splicing-syntax-class linkage
  (pattern (~seq))
  (pattern (~seq #:linkage val:expr)))

 (syntax-parse stx
  ((_ name:id ((arg:id arg-type:expr) ... (~datum ->) return-type:expr)
      (~and
        (~seq keywords ...)
        (~seq visibility:visibility
              linkage:linkage))
      bodies:expr ...)
   (with-syntax (((arg-type-val ...) (generate-temporaries #'(arg-type ...))))
    (define/with-syntax function-implementation
      (syntax-property 
       #'(llvm-implement-function name (arg ...) bodies ...)
       'llvm-implement-function #t))

    #`(begin
       (define arg-type-val arg-type) ...
       (define return-type-val return-type)
       (llvm-declare-function name
         (llvm-function-type return-type-val arg-type-val ...)
         keywords ...)
       function-implementation)))))

(define-syntax (handle-bodies stx)
 (syntax-case stx ()
  ((_ (last-bodies ...) body bodies ...)
   (let () 
     (define new-body (local-expand #'body (syntax-local-context) #f))
     (if (syntax-property new-body 'llvm-implement-function)
         #`(handle-bodies (#,new-body last-bodies ...) bodies ...)
         (syntax-case new-body (begin)
          ((begin form ...)
            #'(handle-bodies (last-bodies ...) form ... bodies ...))
          (form 
            #'(begin form
               (handle-bodies (last-bodies ...) bodies ...)))))))
  ((_ (last-bodies ...)) #'(begin last-bodies ...))))

(define-syntax (llvm-define-module stx)
 (define-splicing-syntax-class context
  (pattern (~seq)))
 (define-splicing-syntax-class exports
  (pattern (~seq) #:attr (export 1) empty)
  (pattern (~seq #:exports (export:id ...))))
 (define-splicing-syntax-class int-type
  (pattern (~seq)))
 (define-splicing-syntax-class float-type
  (pattern (~seq)))


 (syntax-parse stx
  ((_ module-name:id context:context exports:exports int-type:int-type float-type:float-type
      bodies:expr ...)
   (define/with-syntax (renamed-exports ...) (generate-temporaries #'(exports.export ...)))
   #'(begin
       (define ctx (llvm-create-context))
       (define exports.export #f) ...
       (define-syntax renamed-exports (make-rename-transformer #'exports.export)) ...
       (define module-name (llvm-create-module (symbol->string 'module-name) #:context ctx))
       (parameterize ((current-context ctx)
                      (current-module module-name)
                      (current-integer-type (llvm-int32-type #:context ctx))
                      (current-float-type (llvm-double-type #:context ctx))
                      (current-builder (llvm-create-builder #:context ctx)))
         (handle-bodies () bodies ...)
         (set! renamed-exports exports.export) ...
         (void))))))






