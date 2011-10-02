#lang racket

(require (for-syntax racket/syntax))


(define-for-syntax marker (make-syntax-introducer))
(define-syntax (mark stx)
  (syntax-case stx ()
    ((_ arg ...) (marker #'(begin arg ...)))))

         
(define foo 'a)
(mark
 (define foo 'b)
 foo
 (mark foo))
foo
(mark foo)
(mark (mark foo))