#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Operations on scalar constants */
(define-llvm-unsafe LLVMConstInt (_fun LLVMTypeRef _long LLVMBool -> LLVMValueRef))
(define-llvm-unsafe LLVMConstIntOfArbitraryPrecision
 (_fun (type words) ::
       (type : LLVMTypeRef)
       (_uint = (length words))
       (words : (_list i _uint64))
       -> LLVMTypeRef))


(define-llvm-safe LLVMConstInt
 (_fun (ty : safe:LLVMTypeRef) _long LLVMBool ->
       (ptr : _pointer) ->
       (safe:llvm-value-ref ptr (safe:llvm-type-ref-context ty))))

;/* Operations on constants of any type */
(define-llvm-multiple-unsafe
 (LLVMConstNull      ; /* all zeroes */
  LLVMConstAllOnes   ; /* only for int/vector */
  LLVMGetUndef
  LLVMConstPointerNull)
 (_fun LLVMTypeRef -> LLVMValueRef))


(define-llvm-multiple-safe
 (LLVMConstNull      ; /* all zeroes */
  LLVMConstAllOnes   ; /* only for int/vector */
  LLVMGetUndef
  LLVMConstPointerNull)
 (_fun (ty : safe:LLVMTypeRef) ->
       (ptr : _pointer) ->
       (safe:llvm-value-ref ptr (safe:llvm-type-ref-context ty))))



(define-llvm-unsafe LLVMConstIntOfString
 (_fun LLVMTypeRef _string _uint8 -> LLVMValueRef))
(define-llvm-unsafe LLVMConstIntOfStringAndSize
 (_fun LLVMTypeRef _string _uint _uint8 -> LLVMValueRef))

(define-llvm-unsafe LLVMConstReal (_fun LLVMTypeRef _double* -> LLVMValueRef))
(define-llvm-unsafe LLVMConstRealOfString
 (_fun LLVMTypeRef _string -> LLVMValueRef))
(define-llvm-unsafe LLVMConstRealOfStringAndSize
 (_fun LLVMTypeRef _string _uint -> LLVMValueRef))

(define-llvm-unsafe LLVMConstIntGetZExtValue (_fun LLVMValueRef -> _ulong))
(define-llvm-unsafe LLVMConstIntGetSExtValue (_fun LLVMValueRef -> _long))

;/* Operations on composite constants */
(define-llvm-unsafe LLVMConstStringInContext
 (_fun (context str dnt) ::
       (context : LLVMContextRef)
       (str : _string)
       (_uint = (string-length str))
       (dnt : LLVMBool)
       -> LLVMValueRef))


(define-llvm-safe LLVMConstStringInContext
 (_fun (context str dnt) ::
       (context : safe:LLVMContextRef)
       (str : _string)
       (_uint = (string-length str))
       (dnt : LLVMBool) ->
       (ptr : _pointer) ->
       (safe:llvm-value-ref ptr context)))

       

(define-llvm-unsafe LLVMConstStructInContext
 (_fun (context fields packed) ::
       (context : LLVMContextRef)
       (fields : (_list i LLVMValueRef))
       (_uint = (length fields))
       (packed : LLVMBool)
       -> LLVMValueRef))


(define-llvm-safe LLVMConstStructInContext
 (_fun (context fields packed) ::
       (context : safe:LLVMContextRef)
       (fields : (_list i safe:LLVMValueRef))
       (_uint = (length fields))
       (packed : LLVMBool) ->
       (ptr : _pointer) ->
       (safe:llvm-value-ref ptr context)))

     

(define-llvm-unsafe LLVMConstString
 (_fun (str dnt) ::
       (str : _string)
       (_uint = (string-length str))
       (dnt : LLVMBool)
       -> LLVMValueRef))



(define-llvm-unsafe LLVMConstStruct
 (_fun (fields packed) ::
       (fields : (_list i LLVMValueRef))
       (_uint = (length fields))
       (packed : LLVMBool)
       -> LLVMValueRef))

(define-llvm-unsafe LLVMConstNamedStruct
 (_fun (type fields) ::
       (type : LLVMTypeRef)
       (fields : (_list i LLVMValueRef))
       (_uint = (length fields))
       -> LLVMValueRef))


(define-llvm-safe LLVMConstNamedStruct
 (_fun (type fields) ::
       (type : safe:LLVMTypeRef)
       (fields : (_list i safe:LLVMValueRef))
       (_uint = (length fields)) ->
       (ptr : _pointer) ->
       (safe:llvm-value-ref ptr (safe:llvm-type-ref-context type))))



(define-llvm-unsafe LLVMConstArray
 (_fun (type elements) ::
       (type : LLVMTypeRef)
       (elements : (_list i LLVMValueRef))
       (_uint = (length elements))
       -> LLVMValueRef))

(define-llvm-safe LLVMConstArray
 (_fun (type elements) ::
       (type : safe:LLVMTypeRef)
       (elements : (_list i safe:LLVMValueRef))
       (_uint = (length elements)) ->
       (ptr : _pointer) ->
       (safe:llvm-value-ref ptr (safe:llvm-type-ref-context type))))





(define-llvm-unsafe LLVMConstVector
 (_fun (elements) ::
       (elements : (_list i LLVMValueRef))
       (_uint = (length elements))
       -> LLVMValueRef))


(define-llvm-unsafe LLVMGetConstOpcode (_fun LLVMValueRef -> LLVMOpcode))
(define-llvm-multiple-unsafe
 (LLVMAlignOf LLVMSizeOf) (_fun LLVMTypeRef -> LLVMValueRef))

(define-llvm-multiple-unsafe 
 (LLVMConstNeg
  LLVMConstNSWNeg
  LLVMConstNUWNeg
  LLVMConstFNeg
  LLVMConstNot)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple-unsafe 
 (LLVMConstAdd
  LLVMConstNSWAdd
  LLVMConstNUWAdd
  LLVMConstFAdd
  LLVMConstSub
  LLVMConstNSWSub
  LLVMConstNUWSub
  LLVMConstFSub
  LLVMConstMul
  LLVMConstNSWMul
  LLVMConstNUWMul
  LLVMConstFMul
  LLVMConstUDiv
  LLVMConstSDiv
  LLVMConstExactSDiv
  LLVMConstFDiv
  LLVMConstURem
  LLVMConstSRem
  LLVMConstFRem
  LLVMConstAnd
  LLVMConstOr
  LLVMConstXor)
 (_fun LLVMValueRef LLVMValueRef  -> LLVMValueRef))


(define-llvm-unsafe LLVMConstICmp
 (_fun LLVMIntPredicate LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm-unsafe LLVMConstFCmp
 (_fun LLVMRealPredicate LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm-multiple-unsafe
 (LLVMConstShl LLVMConstLShr LLVMConstAShr)
 (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm-multiple-unsafe (LLVMConstGEP LLVMConstInBoundsGEP)
 (_fun (ptr indices) ::
       (ptr : LLVMValueRef)
       (indices : (_list i LLVMValueRef))
       (_uint = (length indices))
       -> LLVMValueRef))
        

(define-llvm-multiple-unsafe
 (LLVMConstTrunc
  LLVMConstSExt
  LLVMConstZExt
  LLVMConstFPTrunc
  LLVMConstFPExt
  LLVMConstUIToFP
  LLVMConstSIToFP
  LLVMConstFPToUI
  LLVMConstFPToSI
  LLVMConstPtrToInt
  LLVMConstIntToPtr
  LLVMConstBitCast
  LLVMConstZExtOrBitCast
  LLVMConstSExtOrBitCast
  LLVMConstTruncOrBitCast
  LLVMConstPointerCast)
 (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))


(define-llvm-unsafe LLVMConstIntCast
 (_fun LLVMValueRef LLVMTypeRef LLVMBool -> LLVMValueRef))

(define-llvm-unsafe LLVMConstFPCast
 (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))

(define-llvm-multiple-unsafe
 (LLVMConstSelect
  LLVMConstInsertElement
  LLVMConstShuffleVector)
 (_fun LLVMValueRef LLVMValueRef LLVMValueRef -> LLVMValueRef))

(define-llvm-unsafe LLVMConstExtractElement
 (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))

(define-llvm-unsafe LLVMConstExtractValue
 (_fun (agg indices) ::
       (agg : LLVMValueRef)
       (indices : (_list i _uint))
       (_uint = (length indices))
       ->
       LLVMValueRef))


(define-llvm-unsafe LLVMConstInsertValue
 (_fun (agg elem indices) ::
       (agg : LLVMValueRef)
       (elem : LLVMValueRef)
       (indices : (_list i _uint))
       (_uint = (length indices))
       ->
       LLVMValueRef))

(define-llvm-unsafe LLVMConstInlineAsm (_fun LLVMTypeRef _string _string LLVMBool LLVMBool -> LLVMValueRef))
