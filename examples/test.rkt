#lang racket/base

(require ffi/unsafe)
(require "../simple.rkt")
(require "../safe.rkt")
(require "../private/simple/base.rkt")
(require "../private/ffi/lib.rkt")
(require "../private/ffi/ctypes.rkt")

(define context (llvm-create-context))
(current-context context)
(define module (llvm-create-module "mandlebrot"))
(current-module module)
(current-integer-type (llvm-int32-type))
(current-float-type (llvm-double-type))
(current-builder (llvm-create-builder))

(define int-type (llvm-int-type))
(define bool-type (llvm-int1-type))
(define float-type (current-float-type))



(define mandlebrot-type
  (llvm-function-type
    float-type))

(define test-proc
 (_cprocedure (list safe:LLVMTypeRef _double*) _void))

((get-ffi-obj 'LLVMTest llvm-racket-lib test-proc)
   float-type
   0.0)

(define mandlebrot (llvm-add-function mandlebrot-type "mandlebrot"))

(let ()
  (define entry-block (llvm-add-block-to-function mandlebrot #:name "entry")) 
  (llvm-set-position entry-block)
  (llvm-ret 2.0))
  

(let ((err (llvm-verify-module module)))
  (void
   (and err
        (error 'mandlebrot "Bad module: ~a, module" err))))

;(display (llvm-module-description module))



