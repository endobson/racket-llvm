#lang racket/base

(require
  racket/port
  racket/stxparam
  racket/splicing
  ffi/unsafe
  ffi/unsafe/define
  srfi/13
  racket/runtime-path)
(require (for-syntax racket/base syntax/parse))

(provide
  llvm-lib
  (for-syntax id-prefix)
  llvm-safety
  llvm-unsafe-context
  define-llvm-racket-safe
  define-llvm-safe
  define-llvm-multiple
  define-llvm-both
  (rename-out
    (define-llvm-racket-unsafe define-llvm-racket)
    (define-llvm-unsafe define-llvm)))

(define llvm-version-string
 (let-values (((process out in err) (subprocess #f #f #f "/usr/bin/env" "llvm-config" "--version")))
  (begin0
   (string-trim-both (port->string out))
   (close-output-port in)
   (close-input-port err)
   (close-input-port out)
   (subprocess-wait process)
   (unless (= (subprocess-status process) 0) (error 'llvm-config "Returned non zero exit code")))))

(define llvm-lib-path
 (let-values (((process out in err) (subprocess #f #f #f "/usr/bin/env" "llvm-config" "--libdir")))
  (begin0
   (string-trim-both (port->string out))
   (close-output-port in)
   (close-input-port err)
   (close-input-port out)
   (subprocess-wait process)
   (unless (= (subprocess-status process) 0) (error 'llvm-config "Returned non zero exit code")))))



(define-runtime-path llvm-racket-lib-path (string-append "llvm-racket" (bytes->string/utf-8 (system-type 'so-suffix))))

(define llvm-lib (ffi-lib (build-path llvm-lib-path (string-append "libLLVM-" llvm-version-string))))
(define llvm-racket-lib (ffi-lib (path-replace-suffix llvm-racket-lib-path "")))

(define-syntax-parameter llvm-safety #f)

(define-ffi-definer define-llvm llvm-lib)
(define-ffi-definer define-llvm-racket llvm-racket-lib)

(define-syntax (llvm-unsafe-context stx)
 (syntax-case stx ()
  ((_ . args)
   #'(splicing-syntax-parameterize ((llvm-safety 'unsafe))
      . args))))
   

(define-syntax (define-llvm-both stx)
 (syntax-case stx ()
  ((_ . args)
   (begin
    #'(splicing-syntax-parameterize ((llvm-safety 'unsafe))
       (define-llvm . args))
    #'(splicing-syntax-parameterize ((llvm-safety 'safe))
       (define-llvm . args))))))

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



(define-for-syntax (id-prefix sym id)
 (let* ((id-sym (syntax-e id))
        (new-id-sym (string->symbol
                      (string-append (symbol->string sym)
                                     (symbol->string id-sym)))))
   (datum->syntax id new-id-sym id id)))


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

