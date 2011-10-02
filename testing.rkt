#lang racket

(require (for-syntax racket/syntax))
(require (for-meta 2 racket/base))
(require racket/stxparam)
(require (for-syntax macro-debugger/util/stxobj))

(define-syntax (debug stx)
  (displayln stx))



(define-syntax (bar stx)
  (define (define-new-unsyntax bound-name overwrite-orig overwrite-unsyntax new-unsyntax-binding new-orig-binding)
    (define body 
      (with-syntax ((overwrite-unsyntax overwrite-unsyntax)
                    (overwrite-orig overwrite-orig)
                    (new-unsyntax-binding new-unsyntax-binding)
                    (new-orig-binding new-orig-binding))
       (with-syntax ((binding-unsyntax
                      (list (quote-syntax unsyntax) #'(syntax-local-introduce #'overwrite-unsyntax)))
                     (binding-orig
                      (list (quote-syntax unsyntax) #'(syntax-local-introduce #'overwrite-orig))))
         #'(syntax-case stx ()
           ((_ arg) 
            (let ()
            (printf "Unsyntax called with ~a~n" #'arg)
            (printf "Have ~a~n" #`binding-orig)
            (printf "marks ~a ~a~n" (get-marks #`binding-orig) (get-marks #'arg))
            (printf "free-id=? ~a~n" (and (identifier? #'arg) (free-identifier=? #`binding-orig #'arg)))
            #`(let ()
                ;(define-syntax true-binding-orig (make-rename-transformer #'new-orig-binding))
                (define-syntax (binding-unsyntax stx)
                  (displayln "got ~a" stx)
                  (error 'end))
                (define-syntax true-binding-unsyntax (make-rename-transformer #'new-unsyntax-binding))
                ;(define-syntax true-binding-unsyntax (make-rename-transformer #'new-unsyntax-binding))
                ;(define-syntax (binding-unsyntax stx)
                ;  #'(error 'binding--unsyntax)) 
                (define-syntax binding-orig 
                  (make-rename-transformer #'new-orig-binding))
                arg)))))))
    #`(define-syntax (#,bound-name stx)
        #,body))
  (syntax-case stx ()
    ((_ bind-id binding-id body)
     (let ()
      (define sli syntax-local-introduce)
      (define base-marker (make-syntax-introducer))
      (define marker (compose base-marker sli))
      (define original-unsyntax (datum->syntax stx 'unsyntax))
      (printf "Base marks ~a~n" (get-marks stx))
      (printf "Marker marks ~a~n" (get-marks (marker stx)))

      (with-syntax ((new-id (marker #'bind-id))
                    (original-embeded-unsyntax original-unsyntax)
                    (new-unsyntax (marker original-unsyntax)))
        (define new-unsyntax-def 
          (define-new-unsyntax (syntax-local-introduce (marker original-unsyntax))
                                  (sli #'new-id)
                                  (sli #'new-unsyntax)
                                  original-unsyntax
                                  #'bind-id))
       #`(let ()
           (define-syntax #,(syntax-local-introduce #'new-id) (make-rename-transformer #'binding-id))
           #,new-unsyntax-def
           #,(sli (marker #'body))))))))

(provide bar)

