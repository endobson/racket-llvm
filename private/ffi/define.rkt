#lang racket

(require typed/racket
  syntax/parse/define
  racket/stxparam
  racket/splicing

  (for-syntax "../llvm-util-exptime.rkt" syntax/parse)
  (only-in ffi/unsafe get-ffi-obj)
  "lib.rkt")


(provide
  define-llvm
  define-llvm-multiple
  define-llvm-safe
  define-llvm-racket-safe
  define-llvm-multiple-safe
  define-llvm-unsafe
  define-llvm-racket-unsafe
  define-llvm-multiple-unsafe
  define-llvm-safety-parameter
  with-llvm-safety)



(define-syntax (define-ffi-definer stx)
 (syntax-case stx ()
  ((_ name lib)
   #'(define-syntax (name stx)
       (syntax-case stx ()
        ((_ id type #:c-id c-id)
         #'(define id (get-ffi-obj 'c-id lib type)))
        ((_ id type)
         #'(define id (get-ffi-obj 'id lib type))))))))



(define-syntax-parameter llvm-safety #f)

(define-syntax define-llvm-safety-parameter
  (syntax-parser
   ((_ name:id body:expr)
    #'(define-llvm-safety-parameter #:safe body #:unsafe body))
   ((_ name:id (~seq (~or (~once (~seq #:safe safe-expr:expr))
                          (~once (~seq #:unsafe unsafe-expr:expr))) ...))
    #'(begin
        (define safe-id (with-llvm-safety #:safe safe-expr))
        (define unsafe-id (with-llvm-safety #:unsafe unsafe-expr))
        (define-syntax (name stx)
          (let ()
            (define (get-id)
              (let ((val (syntax-parameter-value #'llvm-safety)))
              (case val
                ((#f) (raise-syntax-error 'llvm-safety "llvm-safety has not been set in this scope" stx))
                ((unsafe) #'unsafe-id)
                ((safe) #'safe-id)
                (else (error 'llvm-safety "Bad value for llvm-safety: ~s" val)))))
            (...
              (syntax-parse stx
                ((_ a ...) #'(#,(get-id) a ...))
                (_ (get-id))))))))))
(define-syntax with-llvm-safety
  (syntax-parser
   ((_ #:both body:expr ...)
     #'(values (with-llvm-safety #:safe body ...) (with-llvm-safety #:unsafe body ...)))
   ((_ (~or (~and #:safe (~bind (safety #''safe)))
            (~and #:unsafe (~bind (safety #''unsafe))))
       body:expr ...)
    #'(syntax-parameterize ((llvm-safety safety))
        body ...))))


(define-ffi-definer define-llvm-raw llvm-lib)
(define-ffi-definer define-llvm-racket-raw llvm-racket-lib)


(define-syntaxes
  (define-llvm-unsafe
   define-llvm-racket-unsafe
   define-llvm-safe
   define-llvm-racket-safe)
 (let ((definer
        (lambda (define-llvm safety)
         (define safety-symbol (if safety 'safe 'unsafe))
         (syntax-parser 
           ((_ name:id type:expr)
            (with-syntax ((id (id-prefix safety-symbol (id-prefix ': #'name))))
              #`(splicing-syntax-parameterize ((llvm-safety '#,safety-symbol))
                  (#,define-llvm id type #:c-id name))))))))
   (values
     (definer #'define-llvm-raw #f)
     (definer #'define-llvm-racket-raw #f)
     (definer #'define-llvm-raw #t)
     (definer #'define-llvm-racket-raw #t))))



(begin-for-syntax

  (define (add-safe-prefix id) (id-prefix 'safe: id))
  (define (add-unsafe-prefix id) (id-prefix 'unsafe: id))
  (define (null-provide id) #'(begin))
  (define (simple-provide id) #`(provide #,id))
  (define ((contracted-provide contract) id)
    #`(provide (contract-out (#,id #,contract))))


  (define-splicing-syntax-class (inner-provide-clause keyword)
    #:attributes (provide)
    (pattern (~seq) #:attr provide simple-provide)
    (pattern (~seq key)
             #:when (equal? (syntax-e (attribute key)) keyword)
             #:attr provide simple-provide)
    (pattern (~seq (key #:no))
             #:when (equal? (syntax-e (attribute key)) keyword)
             #:attr provide null-provide)
    (pattern (~seq (key contract:expr))
             #:when (equal? (syntax-e (attribute key)) keyword)
             #:attr provide (contracted-provide #'contract)))

  (define-syntax-class provide-clause
    #:attributes (safe unsafe)
    (pattern
      (#:provide
        (~seq (~or (~once (~var safe-inner (inner-provide-clause '#:safe)))
                   (~once (~var unsafe-inner (inner-provide-clause '#:unsafe)))) ...))
      #:attr safe (attribute safe-inner.provide)
      #:attr unsafe (attribute unsafe-inner.provide)))

  (define-splicing-syntax-class ctype-clause
    #:attributes (safe unsafe)
    (pattern (~seq #:both type:expr) #:attr safe #'type #:attr unsafe #'type)
    (pattern (~seq (~or (~once (~seq #:safe safe:expr)) (~once (~seq #:unsafe unsafe:expr))) ...))))



(define-syntax define-llvm
  (syntax-parser
   ((_ name:id
       types:ctype-clause
       (~optional provide:provide-clause
         #:defaults ((provide.safe simple-provide) (provide.unsafe simple-provide))))
    (define safe-name (add-safe-prefix #'name))
    (define unsafe-name (add-unsafe-prefix #'name))
    #`(begin
       (define-llvm-safe name types.safe)
       (define-llvm-unsafe name types.unsafe)
       #,((attribute provide.safe) safe-name)
       #,((attribute provide.unsafe) unsafe-name)))))

(define-syntax define-llvm-multiple
  (syntax-parser
   ((_ (name:id ...) . rest)
    #'(begin
        (define-llvm name . rest) ...))))
       



(define-syntax (define-llvm-multiple-unsafe stx)
 (syntax-parse stx
  ((_ (name:id ...) type:expr) 
    #'(begin (define-llvm-unsafe name type) ...))))

(define-syntax (define-llvm-multiple-safe stx)
 (syntax-parse stx
  ((_ (name:id ...) type:expr) 
    #'(begin (define-llvm-safe name type) ...))))



