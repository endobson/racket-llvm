#lang racket

(require "simple.rkt")

(define-syntax foo
  (make-set!-transformer
    (lambda (stx)
      (syntax-case stx ()
        (stuff
          (let ()
            (displayln (syntax->datum #'stuff))
            #'3))))))

(set! foo 1)
foo
(set! foo 1 2)
