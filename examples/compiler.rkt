#lang racket

;Definitions
;Our language will be simple with two first class data types:
;Booleans and Integers (32Bit)
;There will also be first order functions
;
;The following expressions will be supported
;#t, #f, N where N is a numeric literal
;(if B T F)
;(binop X Y), where binop ∈ {+,-,x,/,%}
;(compare X Y), where compare ∈ {<,>,<=,>=,=}
;(let ((x expr) ...) body)
;(call fun arg-expr ...)
;
;A function definition will look like
;(define (fun-name arg ...) body)
;
;A program is something that looks like
;(program main-function-name
; function-definition ...)
;
; This implementation is not meant to be perfect and error check everything.
; It requires that the program be well-typed, for the obvious definition of
; well-typed.

(require "../private/simple/all.rkt")

; Data for an expression
(struct num-literal (value))
(struct bool-literal (value))
(struct identifier (symbol))
(struct conditional (test true false))
(struct binary-op (op left right))
(struct compare-op (op left right))
(struct binding (name expr))
(struct bind (bindings body))
(struct call (function args))

; Data for a funciton definition
(struct function-definition (name args body))

; Data for a program
(struct program (main functions))

(define (parse sexp)
  (define (binary-op-symbol? x)
    (member x '(+ - * / %)))
  (define (compare-op-symbol? x)
    (member x '(< > <= >= =)))
  (define (parse-program sexp)
    (match sexp
      (`(program ,(? symbol? main) ,funs ...)
       (program main (map parse-function funs)))))
  (define (parse-function sexp)
    (match sexp
      (`(define (,(? symbol? name) ,(? symbol? args) ...) ,body)
        (function-definition name args (parse-expression body)))))
  (define (parse-expression sexp)
    (match sexp
      ((? exact-integer? n) (num-literal n))
      ((? boolean? v) (bool-literal v))
      ((? symbol? v) (identifier v))
      (`(if ,c ,t ,f) (conditional (parse-expression c)
                                   (parse-expression t)
                                   (parse-expression f)))
      (`(,(? binary-op-symbol? op) ,left ,right)
        (binary-op op (parse-expression left) (parse-expression right)))
      (`(,(? compare-op-symbol? op) ,left ,right)
        (compare-op op (parse-expression left) (parse-expression right)))
      (`(let ((,(? symbol? vars) ,exprs) ...) ,body)
        (bind (for/list ((v vars) (e exprs)) (binding v (parse-expression e)))
              (parse-expression body)))
      (`(call ,(? symbol? name) ,args ...)
        (call name (map parse-expression args)))))

  (parse-program sexp))

(define (compile program)
  (define context (llvm-create-context))
  (define module (llvm-create-module "module" #:context context))

  (enter-module/32 context module
    (define function-map
      (for/hash ((function (program-functions program)))
        (let ((name (function-definition-name function)))
          (values name
            (llvm-add-function
              (llvm-function-type* (llvm-int32-type)
                (for/list ((arg (function-definition-args function)))
                  (llvm-int32-type)))
              (symbol->string (function-definition-name function)))))))

    (define (compile-function function)
      (match function
        ((function-definition name args body)
         (define llvm-function (hash-ref function-map name))
         (llvm-set-position
           (llvm-add-block-to-function llvm-function #:name "entry"))
         (define base-env
           (for/hash ((arg args) (i (in-naturals)))
             (values arg (llvm-get-param i))))
         (llvm-ret (compile-expression body base-env)))))

    (define (convert-binary-op op)
      (case op
        ((+) llvm-+)
        ((-) llvm--)
        ((*) llvm-*)
        ((/) llvm-/)
        ((%) llvm-i%)))
    (define (convert-compare-op op)
      (case op
        ((<) llvm-<)
        ((>) llvm->)
        ((<=) llvm-<=)
        ((>=) llvm->=)
        ((=) llvm-=)))

    (define (compile-expression expr env)
      (match expr
       ((num-literal n) (llvm-int n))
       ((bool-literal b) (llvm-int (if b 1 0)))
       ((identifier s) (hash-ref env s))
       ((binary-op op left right)
        ((convert-binary-op op)
         (compile-expression left env)
         (compile-expression right env)))
       ((compare-op op left right)
        ((convert-compare-op op)
         (compile-expression left env)
         (compile-expression right env)))
       ((conditional c t f)
        (llvm-if (llvm-/= 0 (compile-expression c env))
                 (compile-expression t env)
                 (compile-expression f env)))
       ((bind bindings body)
        (let ((new-env (for/fold ((new-env env)) ((binding bindings))
                         (hash-set new-env (binding-name binding)
                                   (compile-expression (binding-expr binding))))))
          (compile-expression body new-env)))
       ((call name args)
        (define function (hash-ref function-map name))
        (define evaled-args
          (for/list ((arg args)) (compile-expression arg env)))
        (llvm-call* function evaled-args))))

    (for ((function (program-functions program)))
      (compile-function function))
    module))



(define program1
  '(program main
     (define (main) 2)))
(define program2
  '(program main
     (define (main) (if #t 1 2))))
(define program3
  '(program main
     (define (main) (+ (call sub #t) (call sub #f)))
     (define (sub v) (if v 2 (* 3 (call sub #t))))))

(define program4
  '(program main
     (define (main) (call fact 5))
     (define (fact x)
       (if (= x 0) 1 (* x (call fact (- x 1)))))))

(compile (parse program1))
(compile (parse program2))
(compile (parse program3))
(compile (parse program4))



