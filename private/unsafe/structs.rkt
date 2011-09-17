#lang racket/base
(require "../ffi/ctypes.rkt")
(provide (rename-out
  (unsafe:llvm-value-ref? llvm-value-ref?)
  (unsafe:llvm-type-ref? llvm-type-ref?)
  (unsafe:llvm-module-ref? llvm-module-ref?)
  (unsafe:llvm-context-ref? llvm-context-ref?)
  (unsafe:llvm-basic-block-ref? llvm-basic-block-ref?)
  (unsafe:llvm-builder-ref? llvm-builder-ref?)))


