#lang racket/base

(require racket/runtime-path)
(require (for-syntax racket/base))
(provide llvm-racket-lib-path)

(define-runtime-path llvm-racket-lib-path (string-append "llvm-racket" (bytes->string/utf-8 (system-type 'so-suffix))))
