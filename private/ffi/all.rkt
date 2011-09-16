#lang racket/base


(define-syntax-rule (reprovide path ...)
 (begin
   (require path ...)
   (provide (all-from-out path ...))))

(reprovide
  "arithmetic.rkt"
  "basic-blocks.rkt"
  "builder.rkt"
  "cast.rkt"
  "constants.rkt"
  "functions.rkt"
  "globals.rkt"
  "instructions.rkt"
  "memory-buffers.rkt"
  "memory.rkt"
  "misc-instructions.rkt"
  "misc-operations.rkt"
  "module-io.rkt"
  "modules.rkt"
  "passes.rkt"
  "racket-ext.rkt"
  "runtime.rkt"
  "terminators.rkt"
  "types.rkt")
 
