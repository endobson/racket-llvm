#lang racket


(require (for-syntax syntax/parse))
(define-syntax (foo stx)
  (syntax-parse stx 
    ((_ ((orig-binding:id new-binding:id) ...) escaper:id body:expr)
     (with-syntax (((saved-orig-binding ...) (generate-temporaries #'(orig-binding ...)))
                   ((orig-binding2 ...) (generate-temporaries #'(orig-binding ...)))
                   ((saved-orig-binding2 ...) (generate-temporaries #'(orig-binding ...))))
       #'(let ()
           (define-syntax saved-orig-binding (make-rename-transformer #'orig-binding)) ...
           (define-syntax saved-escaper (make-rename-transformer #'escaper))
           (let ()
             (define-syntax orig-binding (make-rename-transformer #'new-binding)) ...
             (define-syntax (escaper stx)
               (syntax-case stx ()
                 ((_ new-body)
                    (with-syntax ((orig-binding2 (syntax-local-introduce #'orig-binding)) ...
                                  (escaper2 (syntax-local-introduce #'escaper))
                                  (saved-escaper2 (syntax-local-introduce #'saved-escaper))
                                  (saved-orig-binding2 (syntax-local-introduce #'saved-orig-binding)) ...)
                    #'(let ()
                        (define-syntax orig-binding2 (make-rename-transformer #'saved-orig-binding2)) ...
                        (define-syntax escaper2 (make-rename-transformer #'saved-escaper2))
                        new-body)))))
             body))))))

(define-syntax (bar stx)
  (syntax-parse stx 
    ((_ ((orig-binding:id new-binding:id) ...) body:expr)
     #`(foo ((orig-binding new-binding) ...) #,(datum->syntax stx 'unsyntax) body))))



(provide bar)
(provide foo)

