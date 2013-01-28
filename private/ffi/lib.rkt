#lang typed/racket/base

(require
  racket/port) 


(require/typed ffi/unsafe
  (#:opaque FFI-Lib ffi-lib?)
  (#:opaque CType ctype?)
  ;very specific type for this use case
  (get-ffi-obj (Symbol FFI-Lib CType -> (-> Any)))
  (_cprocedure ((Listof CType) CType -> CType))
  (_void CType)
  (ffi-lib (Path [#:global? Boolean] -> FFI-Lib)))

(require (only-in ffi/unsafe _fun (-> ffi:->)))               

(require/typed "paths.rkt"
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



(define llvm-lib
  (let ((lib-name (string-append "libLLVM-" llvm-version-string)))
    (ffi-lib
      (case (system-type 'os)
        ((macosx) (build-path llvm-lib-path lib-name))
        ((unix) (string->path lib-name))
        ((windows) (string->path lib-name)))
      #:global? #t)))

(define llvm-racket-lib (ffi-lib (path-replace-suffix llvm-racket-lib-path "")))

(define cthunk
 (_cprocedure null _void))

((get-ffi-obj 'LLVMInitializeRacket llvm-racket-lib cthunk))
;((get-ffi-obj 'LLVMInitializeX86TargetInfo llvm-lib cthunk))
;((get-ffi-obj 'LLVMInitializeX86Target llvm-lib cthunk))




