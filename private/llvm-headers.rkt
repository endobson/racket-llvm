#lang racket/base

(require
  (for-syntax racket/base)
  (rename-in racket/contract
    (-> c:->))
  racket/port
  racket/match
  racket/list
  racket/string
  ffi/unsafe
  "clang.rkt"
  "llvm-config.rkt")

(provide 
  (contract-out
    (read-header (c:-> path-string? #:lang (or/c 'c 'c++) any/c))
    (namespace-get-enum (c:-> namespace? symbol? (or/c (hash/c symbol? integer?) #f)))
    (namespace-get-typedef (c:-> namespace? symbol? (or/c (hash/c symbol? integer?) #f)))
    (namespace-get-namespace (c:-> namespace? symbol? (or/c namespace? #f)))))

;; Represents the enums, typedefs, and subname spaces of a namespace.
;; enums: (hash/c symbol? integer?)
;; typedefs: (hash/c symbol? integer?)
;; namespaces (hash/c symbol? namespace?)
(struct namespace [enums typedefs namespaces] #:transparent)

(define (read-header name #:lang lang)
  (define filename (string-append (llvm-include-dir) "/" name))
  (define idx (create-index #:display-diagnostics #t))
  (define tu (create-translation-unit-from-source-file idx filename
                                                       (list* "-x" (symbol->string lang) (llvm-cpp-flags))))

  ;; cursor -> (hash/c symbol? integer?)
  (define (read-enum-decl cursor)
    (make-immutable-hash
      (cursor-map cursor
        (λ (child)
          (cons (string->symbol (cursor-spelling child)) (enum-constant-decl-value child))))))

  ;; cursor -> namespace
  (define (visit-cursor c)
    (define entries
      (cursor-map c
        (λ (cursor)
           (cond
             [(namespace-decl-cursor? cursor)
              (list 'namespace
                    (string->symbol (cursor-spelling cursor))
                    (visit-cursor cursor))]
             [(typedef-decl-cursor? cursor)
              (define enum (cursor-find cursor read-enum-decl))
              (and enum (list 'typedef (string->symbol (cursor-spelling cursor)) enum))]
             [(enum-decl-cursor? cursor)
              (define name (cursor-spelling cursor))
              (and (not (equal? "" name))
                   (list 'enum (string->symbol name) (read-enum-decl cursor)))]
             [else #f]))))
    (namespace
      (for/hash ([entry (in-list entries)]
                 #:when (and entry (equal? (first entry) 'enum)))
        (values (second entry) (third entry)))
      (for/hash ([entry (in-list entries)]
                 #:when (and entry (equal? (first entry) 'typedef)))
        (values (second entry) (third entry)))
      (for/hash ([entry (in-list entries)]
                 #:when (and entry (equal? (first entry) 'namespace)))
        (values (second entry) (third entry)))))

 (visit-cursor (translation-unit-cursor tu)))

(define (namespace-get-typedef namespace name)
  (for/first ([(n v) (in-hash (namespace-typedefs namespace))]
              #:when (equal? name n))
    v))

(define (namespace-get-enum namespace name)
  (for/first ([(n v) (in-hash (namespace-enums namespace))]
              #:when (equal? name n))
    v))

(define (namespace-get-namespace namespace name)
  (for/first ([(n v) (in-hash (namespace-namespaces namespace))]
              #:when (equal? name n))
    v))
