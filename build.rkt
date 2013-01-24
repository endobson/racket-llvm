#lang racket
(require srfi/13)

(define (build)
  (define launcher "/usr/bin/env")
  (define compiler '("clang"))
  (define os (system-type 'os))
  (match-define (list version) (llvm-config "--version"))

  (define shared-library-flags
    (case os
     ((unix) '("-shared"))
     ((macosx) `("-dynamiclib" "-lstdc++"))))

  (define llvm-library-flags
    (list (format "-lLLVM-~a" version) ))

  (define architecture-flags
    (case os
      ((unix) empty)
      ((macosx) empty)))
  (define output-redirection-flags
    `("-o" 
      ,(string-append "llvm-racket"
                      (bytes->string/utf-8 (system-type 'so-suffix)))))

  (define cxx-flags (llvm-config "--cxxflags"))
  (define ld-flags (llvm-config "--ldflags"))

  (define source-file '("llvm-racket.cpp"))
  (define arguments
    (append
      compiler
      shared-library-flags
      llvm-library-flags
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
   (unless (= (subprocess-status process) 0) (error 'c-compiler "Returned non zero exit code"))))

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



