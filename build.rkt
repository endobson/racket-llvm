#lang racket
(require srfi/13)

(define (build)
  (define launcher "/usr/bin/env")
  (define compiler '("g++"))
  (define os (system-type 'os))
  (define shared-library-flags
    (case os
     ((unix) '("-shared"))
     ((macosx) '("-dynamiclib" "-undefined" "suppress" "-flat_namespace"))))
  (define architecture-flags
    (case os
      ((unix) '("-m32"))
      ((macosx) empty)))
  (define output-redirection-flags
    `("-o" 
      ,(case os
        ((unix) "llvm-racket.so")
        ((macosx) "llvm-racket.dylib"))))
  (define cxx-flags (llvm-config "--cxxflags"))
  (define ld-flags (llvm-config "--ldflags"))

  (define source-file '("llvm-racket.cpp"))
  (define arguments
    (append
      compiler
      shared-library-flags
      architecture-flags
      output-redirection-flags
      cxx-flags
      ld-flags
      source-file))

  (let-values (((process out in err)
               (apply subprocess #f #f (current-error-port) launcher arguments)))
    (close-output-port in)
    (close-input-port out)
    (subprocess-wait process)
   (unless (= (subprocess-status process) 0) (error 'g++ "Returned non zero exit code"))))

(define (llvm-config flags)
 (define (remove-blanks lst)
  (filter (lambda (x) (not (equal? x ""))) lst))
 (remove-blanks
  (regexp-split " "
   (let-values (((process out in err) (subprocess #f #f #f "/usr/bin/env" "llvm-config" flags)))
    (begin0
     (string-trim-both (port->string out))
     (close-output-port in)
     (close-input-port err)
     (close-input-port out)
     (subprocess-wait process)
     (unless (= (subprocess-status process) 0) (error 'llvm-config "Returned non zero exit code for flags: ~a" flags)))))))



 

(build)
;rsync -r . ~/proj/racket/planet/llvm/1.0



