#lang typed/racket/base

(require
  racket/port) 


(require/typed ffi/unsafe
  (opaque FFI-Lib ffi-lib?)
  (ffi-lib (Path -> FFI-Lib)))
               

(require/typed "llvm-ffi-paths.rkt"
 (llvm-racket-lib-path Path))
(require/typed srfi/13
 (string-trim-both (String -> String)))


(provide
  llvm-lib
  llvm-racket-lib)

(define llvm-version-string
 (let-values (((process out in err) (subprocess #f #f #f "/usr/bin/env" "llvm-config" "--version")))
  (begin0
   (string-trim-both (port->string out))
   (close-output-port in)
   (close-input-port err)
   (close-input-port out)
   (subprocess-wait process)
   (unless (equal? (subprocess-status process) 0) (error 'llvm-config "Returned non zero exit code")))))

(define llvm-lib-path
 (let-values (((process out in err) (subprocess #f #f #f "/usr/bin/env" "llvm-config" "--libdir")))
  (begin0
   (string-trim-both (port->string out))
   (close-output-port in)
   (close-input-port err)
   (close-input-port out)
   (subprocess-wait process)
   (unless (equal? (subprocess-status process) 0) (error 'llvm-config "Returned non zero exit code")))))




(define llvm-lib (ffi-lib (build-path llvm-lib-path (string-append "libLLVM-" llvm-version-string))))
(define llvm-racket-lib (ffi-lib (path-replace-suffix llvm-racket-lib-path "")))



