#lang racket
(require "testing.rkt")

(define a 'aa)
(define b 'bb)
(define c 'cc)
(define x 'xx)
(define z 'zz)
(define (unsyntax args) (error 'top-level-unsyntax))
        
(bar ((a z)) (bar ((a x) (b c)) (bar ((b z)) (list a b #,a #,b #,#,a #,#,b #,#,#,#,#,a))))

