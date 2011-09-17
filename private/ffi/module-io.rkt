#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/*===-- Operations on modules ---------------------------------------------===*/

;/** Writes a module to the specified path. Returns 0 on success. */ 
(define-llvm-unsafe LLVMWriteBitcodeToFile (_fun LLVMModuleRef _string -> _int))

;/** Writes a module to an open file descriptor. Returns 0 on success. */
(define-llvm-unsafe LLVMWriteBitcodeToFD (_fun LLVMModuleRef _int _bool _bool -> _int))

;/* Builds a module from the bitcode in the specified memory buffer, returning a
;   reference to the module via the OutModule parameter. Returns 0 on success.
;   Optionally returns a human-readable error message via OutMessage. */ 
(define-llvm-unsafe LLVMParseBitcode
  (_fun (buffer) ::
        (buffer : LLVMMemoryBufferRef)
        (module : (_ptr o LLVMModuleRef))
        (message : (_ptr io LLVMMessage) = #f)
        ->
        (err : LLVMBool)
        ->
        (if err message module)))
            
(define-llvm-unsafe LLVMParseBitcodeInContext
  (_fun (context buffer) ::
        (context : LLVMContextRef)
        (buffer : LLVMMemoryBufferRef)
        (module : (_ptr o LLVMModuleRef))
        (message : (_ptr io LLVMMessage) = #f)
        ->
        (err : LLVMBool)
        ->
        (if err message module)))

(define-llvm-unsafe LLVMGetBitcodeModule
  (_fun (buffer) ::
        (buffer : LLVMMemoryBufferRef)
        (module : (_ptr o LLVMModuleRef))
        (message : (_ptr io LLVMMessage) = #f)
        ->
        (err : LLVMBool)
        ->
        (if err message module)))
            
(define-llvm-unsafe LLVMGetBitcodeModuleInContext
  (_fun (context buffer) ::
        (context : LLVMContextRef)
        (buffer : LLVMMemoryBufferRef)
        (module : (_ptr o LLVMModuleRef))
        (message : (_ptr io LLVMMessage) = #f)
        ->
        (err : LLVMBool)
        ->
        (if err message module)))

