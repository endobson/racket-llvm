#lang racket


(require (for-syntax syntax/parse))
(require (for-syntax macro-debugger/util/stxobj))

(provide foo)
(define-syntax (foo stx)
  (syntax-parse stx 
    ((_ name:id escaper:id body:expr)
     #'(let ()
         (define-syntax saved-escaper (make-rename-transformer #'escaper))
         (define-syntax (escaper-impl stx)
             (syntax-case stx ()
               ((_ new-body)
                (with-syntax ((escaper2 (datum->syntax #'new-body (syntax-e #'escaper))))
                  #'(let ()
                      (define-syntax escaper2 (make-rename-transformer #'saved-escaper))
                        new-body)))))
           (let ()
             (define-syntax escaper (make-rename-transformer #'escaper-impl))
             body)))))

