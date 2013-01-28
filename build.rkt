#lang racket
(require srfi/13
         dynext/compile
         dynext/link)

(define (build)
  (define cxx-flags (llvm-config "--cxxflags"))
  (define ld-flags (llvm-config "--ldflags"))
  (define include-dirs (llvm-config "--includedir"))

  (define output-so-name
    (string-append "llvm-racket"
                   (bytes->string/utf-8 (system-type 'so-suffix))))

  ;; Switch the compiler to clang if we can find it.
  (parameterize ([current-extension-compiler 
                  (cond [(find-executable-path "clang")
                         => values]
                        [else
                         (current-extension-compiler)])]

                 ;; Change the compiler flags:
                 [current-extension-compiler-flags 
                  (append cxx-flags (current-extension-compiler-flags))]

                 ;; As well as the linker flags:
                 [current-extension-linker-flags 
                  (append ld-flags (current-extension-linker-flags))])


    ;; Finally, build:
    (compile-extension #t 
                       "llvm-racket.cpp"
                       "llvm-racket.o"
                       include-dirs)
    ;; ... and link:
    (link-extension #t
                    (list "llvm-racket.o")
                    output-so-name)
    
    ;; cleanup:
    (delete-file "llvm-racket.o")
    
    (void)))


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



