#lang racket/base

(require
  (for-syntax racket/base)
  (planet dherman/c:4:0)
  (rename-in racket/contract
    (-> c:->))
  racket/port
  racket/match
  racket/list
  racket/string
  ffi/unsafe
  "llvm-config.rkt")

(provide 
  (contract-out
    (read-genfile (c:-> path-string? string? string?))
    (read-header (c:-> path-string? (listof decl?)))
    (extract-enum-list (c:-> symbol? (listof decl?) (listof (or/c symbol? number?))))))

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


  (define filename (string-append (include-dir) "/" name))
  (define command `("/usr/bin/env" "cpp" ,(format "-D~a" def) "-E" "-P" ,@(cpp-flags) ,filename))
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



(define (read-header name)
  (define (munge header-string)
    (string-join
      (filter
        (lambda (line)
          (not (regexp-match? "__attribute__" line)))
        (string-split header-string "\n"))
      "\n"))

  (define filename (string-append (include-dir) "/" name))
  (define command `("/usr/bin/env" "clang" "-U__GNUC__" "-E" "-P" ,@(cpp-flags) ,filename))
  (let-values (((process out in err) (apply subprocess #f #f #f command)))
   (close-output-port in)
   (begin0
    (parse-program (munge (string-trim (port->string out)))
      #:typedef '(uint16_t uint32_t uint64_t))
    (let ((err-string (string-trim (port->string err))))
      (close-input-port err)
      (close-input-port out)
      (subprocess-wait process)
      (unless (= (subprocess-status process) 0)
        (error 'clang "Returned non zero exit code for file: ~a\n~a" filename err-string))))))


(define (find-typedef name declarations)
  (for/or ((a-typedef (filter decl:typedef? declarations)))
    (match a-typedef
      ((decl:typedef _ type (list (decl:declarator _ (id:var _ (== name)) _ _))) type)
      (_ #f))))


(define (eval-expr expr)
 (match expr
   ((expr:binop _ l (id:op _ op-name) r)
    (define op
      (match op-name
       ('<< arithmetic-shift)))
    (op (eval-expr l) (eval-expr r)))
   ((expr:int _ v '()) v)))


(define-match-expander bind
   (λ (stx)
     (syntax-case stx ()
       [(_ pat expr) #'(app (λ (x) expr) pat)]
       [(_ pat) #'(app (λ (x) #f) pat)])))

(define (parse-enum enum)
  (match enum
   ((type:enum _ tag (list (or (cons (id:var _ variant-names) vals)
                               (and (id:var _ variant-names)
                                    (bind vals))) ...))
    (append*
      (for/list ((name variant-names) (val vals))
        (if val
            `(,name = ,(eval-expr val))
            `(,name)))))))


(define (extract-enum-list name declarations)
  (let ((def (find-typedef name declarations)))
    (if def
        (parse-enum def)
        (error 'extract-enum-list "No enum definition found for ~a" name))))
