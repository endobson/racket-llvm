#lang typed/racket/base

(provide id-prefix)

(: id-prefix (Symbol Identifier -> Identifier))
(define (id-prefix sym id)
 (let* ((id-sym (syntax-e id))
        (new-id-sym (string->symbol
                      (string-append (symbol->string sym)
                                     (symbol->string id-sym)))))
   (datum->syntax id new-id-sym id id)))
