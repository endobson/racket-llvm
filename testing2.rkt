#lang racket
(require "testing.rkt")

(define a 'aa)
(define b 'bb)
(define c 'cc)
(define x 'xx)
(define z 'zz)
(define (unsyntax args) (error 'top-level-unsyntax))
        
(bar a x (bar b z (list a b c #,a #,#,a)));#,a #,b #,#,a #,#,b)))

