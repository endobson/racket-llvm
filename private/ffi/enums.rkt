#lang racket/base

(require
  (for-syntax
    racket/base
    racket/list
    racket/string
    racket/syntax
    racket/match
    racket/string
    syntax/parse
    "../llvm-headers.rkt")
  racket/contract
  (only-in ffi/unsafe _enum))

(define-for-syntax (generate-enum-code2 nm enum-values)
  (define/with-syntax name nm)
  (define/with-syntax name? (format-id nm "~a?" nm))

  (define enum-list
    (append*
      (for/list ([(name value) (in-hash enum-values)])
        `(,name = ,value))))
  (define quoted-enum-symbols
    (for/list ((symbol (hash-keys enum-values)))
      #`'#,symbol))
  #`(begin
     (provide name name?)
     (define name (_enum '#,enum-list))
     (define name? (or/c #,@quoted-enum-symbols))))

(begin-for-syntax
  (define (namespace-get-namespaces ns names)
    (for/fold ([ns ns]) ([name names])
       (namespace-get-namespace ns name)))
  (define-syntax-class namespaced-id
    #:attributes (name namespaces)
    (pattern name:id
      #:attr namespaces '())
    (pattern (nss:id ... name:id)
      #:attr namespaces (syntax->datum #'(nss ...))))
  (define-splicing-syntax-class declaration
    #:attributes (generator)
    (pattern (~seq :namespaced-id
                   (~optional (~seq #:name alt-name))
                   (~optional (~seq #:rename rename) #:defaults ([rename #'values])))
      #:with external-name (or (attribute alt-name) #'name)
      #:with external-predicate (format-id #'external-name "~a?" #'external-name)
      #:attr generator
        (λ (ns)
          (generate-enum-code2
            #'external-name
            (namespace-get-typedef (namespace-get-namespaces ns (attribute namespaces))
                                   (syntax-e #'name)))))
    (pattern (~seq #:enum :namespaced-id
                   (~optional (~seq #:name alt-name))
                   (~optional (~seq #:rename rename) #:defaults ([rename #'values]))
                   )
      #:with external-name (or (attribute alt-name) #'name)
      #:with external-predicate (format-id #'external-name "~a?" #'external-name)
      #:attr generator
        (λ (ns)
          (generate-enum-code2
            #'external-name
            (namespace-get-enum (namespace-get-namespaces ns (attribute namespaces))
                                (syntax-e #'name)))))))

(define-syntax define-enums
  (syntax-parser
    [(_ header-file:str #:lang lang:id decl:declaration ...)
     (define ns (read-header (syntax-e #'header-file) #:lang (syntax-e #'lang)))
     #`(begin
         #,@(for/list ([gen (in-list (attribute decl.generator))])
              (gen ns)))]))
(define-for-syntax  (underscore->hypen symbol)
  (string->symbol (string-replace (symbol->string symbol) "_" "-")))

(define-enums "llvm-c/Core.h" #:lang c
  LLVMAttribute
  LLVMOpcode
  LLVMTypeKind
  LLVMLinkage
  LLVMVisibility
  LLVMCallConv
  LLVMIntPredicate
  LLVMRealPredicate
  LLVMLandingPadClauseTy)

(define-enums "llvm/IR/Intrinsics.h" #:lang c++
  #:enum (llvm Intrinsic ID) #:name LLVMIntrinsic #:rename underscore->hyphen)

