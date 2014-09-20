#lang racket/base

(require
  (for-syntax racket/base syntax/parse)
  racket/string
  racket/port
  racket/promise
  racket/contract)


(provide
  (contract-out
    (llvm-include-dir (-> string?))
    (llvm-lib-dir (-> string?))
    (llvm-version (-> string?))
    (llvm-cpp-flags (-> (listof string?)))))

(define-syntax (define/promise stx)
  (syntax-parse stx
    ((_ name:id body:expr)
     #'(begin
         (define p (delay body))
         (define (name) (force p))))))

(define (llvm-config/list flag)
 (define (remove-blanks lst)
  (filter (lambda (x) (not (equal? x ""))) lst))
 (remove-blanks (regexp-split " " (llvm-config flag))))

(define (llvm-config flag)
 (let-values (((process out in err) (subprocess #f #f #f "/usr/bin/env" "llvm-config" flag)))
  (close-output-port in)
  (begin0
   (string-trim (port->string out))
   (close-input-port err)
   (close-input-port out)
   (subprocess-wait process)
   (unless (= (subprocess-status process) 0)
     (error 'llvm-config "Returned non zero exit code for flags: ~a" flag)))))



(define/promise llvm-include-dir (llvm-config "--includedir"))
(define/promise llvm-lib-dir (llvm-config "--libdir"))
(define/promise llvm-version (llvm-config "--version"))
(define/promise llvm-cpp-flags (llvm-config/list "--cppflags"))


