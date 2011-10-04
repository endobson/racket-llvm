#lang racket/base

(require "base.rkt" "memory.rkt" racket/list)
(require "../ffi/safe.rkt")
(require (for-syntax racket/base syntax/parse))


(provide llvm-if llvm-for llvm-when llvm-define-mutable llvm-define-reference)

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




    



