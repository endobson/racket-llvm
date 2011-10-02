#lang racket

(require (for-syntax racket/syntax))
(require (for-meta 2 racket/base))
(require racket/stxparam)
(require (for-syntax macro-debugger/util/stxobj))

(define-syntax (debug stx)
  (displayln stx))



(define-syntax (bar stx)
  (define (define-new-unsyntax bound-name overwrite-orig overwrite-unsyntax new-unsyntax-binding new-orig-binding)
    (printf "new-unsyntax-binding marks ~a~n" (get-marks new-unsyntax-binding))
    (define body 
      (with-syntax ((overwrite-unsyntax overwrite-unsyntax)
                    (overwrite-orig overwrite-orig)
                    (new-unsyntax-binding new-unsyntax-binding)
                    (new-orig-binding new-orig-binding))
         #'(syntax-case stx ()
           ((_ arg) 
            (let ()
              (define sli syntax-local-introduce)
              (define base-marker (make-syntax-introducer))
            (with-syntax ((overwrite-unsyntax2 (sli (base-marker #'overwrite-unsyntax)))
                          (overwrite-orig2 (sli (base-marker #'overwrite-orig)))
                          (arg2 (sli (base-marker (sli  #'arg))))
                          (new-unsyntax-binding2 (syntax-local-introduce #'new-unsyntax-binding))
                          (new-orig-binding2 (syntax-local-introduce #'new-orig-binding)))
            (printf "Unsyntax called with ~a~n" #'arg)
            (printf "Have ~a~n" #'overwrite-orig2)
            (printf "marks ~a ~a~n" (get-marks #'overwrite-orig2) (get-marks #'arg2))
            (printf "free-id=? ~a~n" (and (identifier? #'arg) (free-identifier=? #'overwrite-orig2 #'arg2)))
            (printf "binding-unsyntax marks ~a~n" (get-marks #`binding-unsyntax))

            (printf "new-unsyntax-binding ~a~n" #'new-unsyntax-binding)
            (printf "new-unsyntax-binding marks ~a~n" (get-marks #'new-unsyntax-binding))
            (printf "new-unsyntax-binding2 marks ~a~n" (get-marks #'new-unsyntax-binding2))
            #'(let ()
                ;(define-syntax true-binding-orig (make-rename-transformer #'new-orig-binding))
                (define-syntax overwrite-unsyntax2 
                  (make-rename-transformer #'new-unsyntax-binding2))
                ;(define-syntax true-binding-unsyntax (make-rename-transformer #'new-unsyntax-binding))
                ;(define-syntax (binding-unsyntax stx)
                ;  #'(error 'binding--unsyntax)) 
                (define-syntax overwrite-orig2 
                  (make-rename-transformer #'new-orig-binding2))
                arg2)))))))
    #`(define-syntax (#,bound-name stx)
        #,body))
  (syntax-case stx ()
    ((_ bind-id binding-id body)
     (let ()
      (define gen-unsyntax (generate-temporary 'gen-unsyntax))
      (define sli syntax-local-introduce)
      (define base-marker (make-syntax-introducer))
      (define marker (compose base-marker sli))
      (define original-unsyntax (datum->syntax stx 'unsyntax))
      (printf "Base marks ~a~n" (get-marks stx))
      (printf "Marker marks ~a~n" (get-marks (marker stx)))
      (printf "gen-unsyntax marks ~a~n" (get-marks gen-unsyntax))
      (printf "original-unsyntax marks ~a~n" (get-marks original-unsyntax))

      (with-syntax ((new-id (marker #'bind-id))
                    (original-embeded-unsyntax original-unsyntax)
                    (new-unsyntax (marker original-unsyntax)))
        (define new-unsyntax-def 
          (define-new-unsyntax (syntax-local-introduce (marker original-unsyntax))
                                  (sli #'new-id)
                                  (sli #'new-unsyntax)
                                  gen-unsyntax
                                  #'bind-id))
       #`(let ()
           (define-syntax #,(syntax-local-introduce #'new-id) (make-rename-transformer #'binding-id))
           (define-syntax gen-unsyntax (make-rename-transformer #'#,original-unsyntax))
           #,new-unsyntax-def
           #,(sli (marker #'body))))))))

(provide bar)

