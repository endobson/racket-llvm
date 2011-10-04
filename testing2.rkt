#lang racket
(require "private/rename.rkt")


(define-syntax (make-bind-to-5 stx)
  (syntax-case stx ()
   ((_ id later)
    #'(define-syntax (later stx)
        (syntax-case stx ()
         ((_ body) 
           (with-syntax ((new-id (syntax-local-introduce #'id)))
            (printf "~a ~a~n"
              (free-identifier=? #'body #'new-id)
              (bound-identifier=? #'body #'new-id))
            #'(let ((new-id 5))
                body))))))))





(let  ()
  (make-bind-to-5 five later)
  (let ()
    (define five 4)
    (later five)))

(let  ()
  (make-bind-to-5 five later)
  (let ()
    (define five 4)
    (#%expression 
     (later five))))


(let  ()
  (make-bind-to-5 five later)
  (letrec ((five 4))
    (later five)))


(let ()
  (make-bind-to-5 five later)
  (let ((five 4))
    (later five)))

