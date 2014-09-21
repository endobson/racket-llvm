#lang racket/base

(require
  (for-syntax
    racket/base
    racket/list
    racket/syntax
    racket/match
    racket/string
    syntax/parse
    "../llvm-headers.rkt")
  (rename-in racket/contract
    (-> c:->))
  ffi/unsafe)

(define-for-syntax declarations (read-header "llvm-c/Core.h"))

(define-for-syntax (enum-list->symbols enum-list)
  (match enum-list
    ((list-rest (? symbol? name) '= (? number? val)  rest)
     (cons name (enum-list->symbols rest)))
    ((cons (? symbol? name) rest)
     (cons name (enum-list->symbols rest)))
    ((list) null)))


(define-for-syntax (generate-enum-code nm enum-list)
  (define/with-syntax name nm)
  (define/with-syntax name? (format-id nm "~a?" nm))

  (define quoted-enum-symbols
    (for/list ((symbol (enum-list->symbols enum-list)))
      #`'#,symbol))
  #`(begin
     (provide name name?)
     (define name (_enum '#,enum-list))
     (define name? (or/c #,@quoted-enum-symbols))))

(define-for-syntax (generate-enum-code2 nm enum-values)
  (define/with-syntax name nm)
  (define/with-syntax name? (format-id nm "~a?" nm))


  (define enum-list
    (append*
      (for/list ([(name value) (in-hash enum-values)])
        `(,name = ,value))))
  (define quoted-enum-symbols
    (for/list ((symbol (hash-values enum-values)))
      #`'#,symbol))
  #`(begin
     (provide name name?)
     (define name (_enum '#,enum-list))
     (define name? (or/c #,@quoted-enum-symbols))))



(define-syntax define-enum
  (syntax-parser
    ((_ name:id)
     (generate-enum-code2
       #'name
       (extract-enum-values (syntax->datum #'name) declarations)))))

(define-syntax define-enums
  (syntax-parser
    ((_ names:id ...)
     #'(begin (define-enum names) ...))))

(define-enums
  LLVMAttribute
  LLVMOpcode
  LLVMTypeKind
  LLVMLinkage
  LLVMVisibility
  LLVMCallConv
  LLVMIntPredicate
  LLVMRealPredicate
  LLVMLandingPadClauseTy)

(define-syntax generate-intrinsics
  (syntax-parser
    ((_ name:id)
     (define intrinsic-names
       (cons 'not-intrinsic
         (map (compose string->symbol
                       (lambda (x) (regexp-replace "_" x "-"))
                       string-trim)
           (string-split
             (read-genfile "llvm/IR/Intrinsics.gen" "GET_INTRINSIC_ENUM_VALUES")
             ","))))
     (generate-enum-code #'name intrinsic-names))))
(generate-intrinsics LLVMIntrinsic)


