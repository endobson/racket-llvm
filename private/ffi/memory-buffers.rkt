#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/*===-- Memory buffers ----------------------------------------------------===*/
;
(define-llvm-unsafe LLVMCreateMemoryBufferWithContentsOfFile
   (_fun (path) ::
          (path : _string)
          (buffer : (_ptr o LLVMMemoryBufferRef))
          (message : (_ptr io LLVMMessage) = #f)
          ->
          (ans : LLVMBool)
          ->
          (if ans message buffer)))


(define-llvm-unsafe LLVMCreateMemoryBufferWithSTDIN
   (_fun () ::
          (buffer : (_ptr o LLVMMemoryBufferRef))
          (message : (_ptr io LLVMMessage) = #f)
          ->
          (ans : LLVMBool)
          ->
          (if ans message buffer)))

(define-llvm-unsafe LLVMDisposeMemoryBuffer
 (_fun LLVMMemoryBufferRef -> _void))



