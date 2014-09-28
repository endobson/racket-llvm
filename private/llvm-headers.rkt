#lang racket/base

(require
  (for-syntax racket/base)
  (except-in (planet dherman/c:4:0) struct)
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
    (read-genfile (c:-> path-string? string? string?))
    (read-header (c:-> path-string? (listof enum-decl?)))
    (extract-enum-values (c:-> symbol? (listof enum-decl?) (hash/c symbol? integer?)))))

(define (read-genfile name def)
  (define (munge contents)
    (string-join
      (filter
        (lambda (str) (> (string-length str) 0))
        (map
          (lambda (line)
            (string-trim (regexp-replace "//.*" line "")))
          (string-split contents "\n")))
      "\n"))


  (define filename (string-append (llvm-include-dir) "/" name))


  (define command `("/usr/bin/env" "cpp" ,(format "-D~a" def) "-E" "-P" ,@(llvm-cpp-flags) ,filename))
  (let-values (((process out in err) (apply subprocess #f #f #f command)))
   (close-output-port in)
   (begin0
    (munge (string-trim (port->string out)))
    (let ((err-string (string-trim (port->string err))))
      (close-input-port err)
      (close-input-port out)
      (subprocess-wait process)
      (unless (= (subprocess-status process) 0)
        (error 'cpp "Returned non zero exit code for file: ~a\n~a" filename err-string))))))

;; Kind is either 'enum or 'typedef
;; Name is a symbol and the name of the enum
;; Values is a dict from symbols to integers
(struct enum-decl [kind name values])

(define (read-header name)
  (define filename (string-append (llvm-include-dir) "/" name))
  (define idx (create-index #:display-diagnostics #t))
  (define tu (create-translation-unit-from-source-file idx filename (llvm-cpp-flags)))

  (define (enum-decl->_enum cursor)
    (make-immutable-hash
      (cursor-map cursor
        (λ (child)
          (cons (string->symbol (cursor-spelling child)) (enum-constant-decl-value child))))))

  (define result
    (filter values
      (cursor-map (translation-unit-cursor tu)
        (λ (cursor)
           (cond
             [(typedef-decl-cursor? cursor)
              (define enum (cursor-find cursor enum-decl->_enum))
              (and enum (enum-decl 'typedef (string->symbol (cursor-spelling cursor)) enum))]
             [(enum-decl-cursor? cursor)
              (define name (cursor-spelling cursor))
              (and (not (equal? "" name))
                   (enum-decl 'enum (string->symbol name) (enum-decl->_enum cursor)))]
             [else #f])))))
  result
  )


(define (find-typedef name declarations)
  (for/first ([decl declarations]
           #:when (equal? (enum-decl-kind decl) 'typedef)
           #:when (equal? (enum-decl-name decl) name))
    decl))


(define (extract-enum-values name declarations)
  (enum-decl-values (find-typedef name declarations)))
