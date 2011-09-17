#lang racket

(require typed/racket

  (for-syntax "../llvm-util-exptime.rkt" syntax/parse)
  (only-in ffi/unsafe get-ffi-obj)
  "lib.rkt")


(provide
  define-llvm-safe
  define-llvm-racket-safe
  define-llvm-multiple-safe
  define-llvm-unsafe
  define-llvm-racket-unsafe
  define-llvm-multiple-unsafe)



(define-syntax (define-ffi-definer stx)
 (syntax-case stx ()
  ((_ name lib)
   #'(define-syntax (name stx)
       (syntax-case stx ()
        ((_ id type #:c-id c-id)
         #'(define id (get-ffi-obj 'c-id lib type)))
        ((_ id type)
         #'(define id (get-ffi-obj 'id lib type))))))))



(define-ffi-definer define-llvm-raw llvm-lib)
(define-ffi-definer define-llvm-racket-raw llvm-racket-lib)

   

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
     (definer #'define-llvm-raw)
     (definer #'define-llvm-racket-raw))))


(define-syntaxes (define-llvm-unsafe define-llvm-racket-unsafe)
 (let ((definer
        (lambda (define-llvm)
         (syntax-parser 
           ((_ name:id type:expr)
            #`(#,define-llvm
               #,(id-prefix 'unsafe: #'name)
               type
               #:c-id name))))))
   (values
     (definer #'define-llvm-raw)
     (definer #'define-llvm-racket-raw))))





(define-syntax (define-llvm-multiple-unsafe stx)
 (syntax-case stx ()
  ((_ (name ...) type) 
    #'(begin (define-llvm-unsafe name type) ...))))

(define-syntax (define-llvm-multiple-safe stx)
 (syntax-case stx ()
  ((_ (name ...) type) 
    #'(begin (define-llvm-safe name type) ...))))



