#lang racket/base

(require "rename.rkt")
(require "simple/all.rkt")
(require "simple/parameters.rkt")
(require (for-syntax racket/base syntax/parse))
(require (for-meta 2 racket/base))
(require racket/list)


(provide ll)


(define-syntax (current-integer-type-macro stx)
  (syntax-case stx ()
    ((_ args ...) #'((current-integer-type) args ...))
    (_ #'(current-integer-type))))

(define-syntax (boolean-type-macro stx)
  (syntax-case stx ()
    ((_ args ...) #'((llvm-int1-type) args ...))
    (_ #'(llvm-int1-type))))

(define-syntax (void-type-macro stx)
  (syntax-case stx ()
    ((_ args ...) #'((llvm-void-type) args ...))
    (_ #'(llvm-void-type))))

(define-syntax (i8-type-macro stx)
  (syntax-case stx ()
    ((_ args ...) #'((llvm-int8-type) args ...))
    (_ #'(llvm-int8-type))))




(define-syntax (current-float-type-macro stx)
  (syntax-case stx ()
    ((_ args ...) #'((current-float-type) args ...))
    (_ #'(current-float-type))))

(define (reversed-llvm-function-type #:varargs (varargs #f) arg . args)
  (let-values (((arg args) 
                (let ((rev (reverse (cons arg args))))
                  (values (first rev) (reverse (rest rev))))))
    (llvm-function-type* arg #:varargs varargs args)))



(begin-for-syntax
  (define-splicing-syntax-class possible-context
    (pattern (~seq) #:attr ctx #f)
    (pattern (~seq #:context ctx:id))))


(define-syntax (ll stx)
  (syntax-parse stx
    ((ll c:possible-context body ...)
     (define ctx (or (attribute c.ctx) #'ll))
     (define-syntax (local-introduce stx)
       (syntax-case stx ()
        ((_ ctx (args ...) body)
         #'(with-syntax ((args (datum->syntax ctx (syntax-e #'args))) ...) body))))
     (local-introduce ctx
        (-> > >= < <= = /= return store load alloca + - * / 
         for when gep gep0 >> << >>> zext sext call
         or and xor sqrt loop
         bool i8 int float vec arr ptr void unsyntax)
     #'(local-rename
         ((-> reversed-llvm-function-type)
          (int current-integer-type-macro)
          (bool boolean-type-macro)
          (i8 i8-type-macro)
          (vec llvm-vector-type)
          (alloca llvm-alloca)
          (store llvm-store)
          (load llvm-load)
          (return llvm-ret)
          (for llvm-for)
          (loop llvm-loop)
          (when llvm-when)
          (gep llvm-gep)
          (gep0 llvm-gep0)
          (call llvm-call)
          (sqrt llvm:sqrt)
          (zext llvm-zext)
          (sext llvm-sext)
          (<< llvm-shl)
          (>> llvm-ashr)
          (>>> llvm-lshr)
          (< llvm-<)
          (<= llvm-<=)
          (> llvm->)
          (>= llvm->=)
          (/= llvm-/=)
          (= llvm-=)
          (+ llvm-+)
          (- llvm--)
          (* llvm-*)
          (/ llvm-/)
          (or llvm-or)
          (and llvm-and)
          (xor llvm-xor)
          (arr llvm-array-type)
          (ptr llvm-pointer-type)
          (void void-type-macro)
          (float current-float-type-macro))
          #:escaper unsyntax
         (begin
           body ...))))))
