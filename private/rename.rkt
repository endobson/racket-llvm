#lang racket/base


(require racket/splicing)
(require (for-syntax syntax/parse racket/base racket/syntax))
(provide local-rename)

(begin-for-syntax
  (define-splicing-syntax-class escape
    (pattern (~seq) #:attr esc (generate-temporary 'escaper))
    (pattern (~seq #:escaper esc:id))))

(define-syntax (local-rename stx)
  (syntax-parse stx 
    ((_ ((orig-binding:id new-binding:id) ...) escaper:escape body:expr)
     (with-syntax (((saved-orig-binding ...) (generate-temporaries #'(orig-binding ...)))
                   ((orig-binding2 ...) (generate-temporaries #'(orig-binding ...)))
                   ((saved-orig-binding2 ...) (generate-temporaries #'(orig-binding ...))))
     #'(splicing-let-syntax
         ((saved-orig-binding (make-rename-transformer #'orig-binding)) ...
          (saved-escaper (make-rename-transformer #'escaper.esc))
          (escaper-impl (lambda (stx)
                          (syntax-case stx ()
                            ((head new-body)
                             (with-syntax ((escaper2 (datum->syntax #'head (syntax-e #'escaper.esc)))
                                           (orig-binding2 (datum->syntax #'head (syntax-e #'orig-binding))) ...
                                           (saved-orig-binding2 (syntax-local-introduce #'saved-orig-binding)) ...)
                               #'(splicing-let-syntax ((orig-binding2 (make-rename-transformer #'saved-orig-binding2)) ...
                                              (escaper2 (make-rename-transformer #'saved-escaper)))
                                       new-body)))))))
           (splicing-let-syntax ((orig-binding (make-rename-transformer #'new-binding)) ...
                                 (escaper.esc (make-rename-transformer #'escaper-impl)))
               body))))))


