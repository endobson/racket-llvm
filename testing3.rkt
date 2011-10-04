#lang racket


(require (for-syntax syntax/parse))

(define-syntax (foo stx)
  (syntax-parse stx 
    (_ 
      (define x #'3)
      x)))

(foo)
