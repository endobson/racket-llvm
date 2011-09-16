#lang racket

(require typed/racket

  (for-syntax "../llvm-util-exptime.rkt" syntax/parse)
  (only-in ffi/unsafe get-ffi-obj)
  "lib.rkt"
  racket/splicing
  racket/stxparam)


(provide
  define-llvm-multiple
  define-llvm-safe
  define-llvm-racket-safe
  define-llvm-both
  (rename-out
   (define-llvm-unsafe define-llvm)
   (define-llvm-racket-unsafe define-llvm-racket))
  llvm-safety
  llvm-unsafe-context
  define-ffi-definer)


(define-syntax-parameter llvm-safety #f)


(define-syntax (llvm-unsafe-context stx)
 (syntax-case stx ()
  ((_ . args)
   #'(splicing-syntax-parameterize ((llvm-safety 'unsafe))
      . args))))


(define-syntax (define-ffi-definer stx)
 (syntax-case stx ()
  ((_ name lib)
   #'(define-syntax (name stx)
       (syntax-case stx ()
        ((_ id type #:c-id c-id)
         #'(define id (get-ffi-obj 'c-id lib type)))
        ((_ id type)
         #'(define id (get-ffi-obj 'id lib type))))))))


(define-syntax (define-llvm-both stx)
 (syntax-case stx ()
  ((_ . args)
   (begin
    #'(splicing-syntax-parameterize ((llvm-safety 'unsafe))
       (define-llvm . args))
    #'(splicing-syntax-parameterize ((llvm-safety 'safe))
       (define-llvm . args))))))


(define-ffi-definer define-llvm llvm-lib)
(define-ffi-definer define-llvm-racket llvm-racket-lib)

   


(define-syntax (define-llvm-unsafe stx)
 (syntax-case stx ()
  ((_ . args)
  #'(splicing-syntax-parameterize ((llvm-safety 'unsafe))
     (define-llvm . args)))))

(define-syntax (define-llvm-racket-unsafe stx)
 (syntax-case stx ()
  ((_ . args)
 #'(splicing-syntax-parameterize ((llvm-safety 'unsafe))
    (define-llvm-racket . args)))))





(define-syntaxes (define-llvm-safe define-llvm-racket-safe)
 (let ((definer
        (lambda (define-llvm)
         (syntax-parser 
           ((_ name:id type:expr)
            #`(#,define-llvm
               #,(id-prefix 'safe: #'name)
               type
               #:c-id name))))))
   (values
     (definer #'define-llvm)
     (definer #'define-llvm-racket))))









(define-syntax (define-llvm-multiple stx)
 (syntax-case stx ()
  ((_ (name ...) type) 
    #'(begin (define-llvm-unsafe name type) ...))))



