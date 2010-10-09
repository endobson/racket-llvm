#lang racket
(require ffi/unsafe)
(displayln 'a)
(define llvm-lib (ffi-lib "libLLVM-2.7"))
(displayln 'b)

(define LLVMInt1Type (get-ffi-obj 'LLVMInt1Type llvm-lib (_fun  -> _pointer )))
(define LLVMInt8Type (get-ffi-obj 'LLVMInt8Type  llvm-lib (_fun  -> _pointer)))
(define LLVMInt16Type (get-ffi-obj 'LLVMInt16Type llvm-lib (_fun  -> _pointer)))
(define LLVMInt32Type (get-ffi-obj 'LLVMInt32Type llvm-lib (_fun  -> _pointer)))
(define LLVMInt64Type (get-ffi-obj 'LLVMInt64Type llvm-lib (_fun  -> _pointer)))
;(get-ffi-obj 'LLVMIntType 'LLVMTypeRef llvm-lib (unsigned NumBits))
(define LLVMGetIntTypeWidth (get-ffi-obj  'LLVMGetIntTypeWidth llvm-lib (_fun  _pointer -> _uint32)))

(LLVMInt64Type)
(LLVMGetIntTypeWidth (LLVMInt64Type))
(LLVMGetIntTypeWidth (LLVMInt32Type))

