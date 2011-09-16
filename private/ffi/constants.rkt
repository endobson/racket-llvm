#lang racket

(require
  "enums.rkt"
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

;/* Operations on scalar constants */
(define-llvm LLVMConstInt (_fun LLVMTypeRef _long LLVMBool -> LLVMValueRef))
(define-llvm LLVMConstIntOfArbitraryPrecision
 (_fun (type words) ::
       (type : LLVMTypeRef)
       (_uint = (length words))
       (words : (_list i _uint64))
       -> LLVMTypeRef))


;/* Operations on constants of any type */
(define-llvm-multiple
 (LLVMConstNull      ; /* all zeroes */
  LLVMConstAllOnes   ; /* only for int/vector */
  LLVMGetUndef
  LLVMConstPointerNull)
 (_fun LLVMTypeRef -> LLVMValueRef))


(define-llvm LLVMConstIntOfString
 (_fun LLVMTypeRef _string _uint8 -> LLVMValueRef))
(define-llvm LLVMConstIntOfStringAndSize
 (_fun LLVMTypeRef _string _uint _uint8 -> LLVMValueRef))

(define-llvm LLVMConstReal (_fun LLVMTypeRef _double* -> LLVMValueRef))
(define-llvm LLVMConstRealOfString
 (_fun LLVMTypeRef _string -> LLVMValueRef))
(define-llvm LLVMConstRealOfStringAndSize
 (_fun LLVMTypeRef _string _uint -> LLVMValueRef))

(define-llvm LLVMConstIntGetZExtValue (_fun LLVMValueRef -> _ulong))
(define-llvm LLVMConstIntGetSExtValue (_fun LLVMValueRef -> _long))

;/* Operations on composite constants */
(define-llvm LLVMConstStringInContext
 (_fun (context str dnt) ::
       (context : LLVMContextRef)
       (str : _string)
       (_uint = (string-length str))
       (dnt : LLVMBool)
       -> LLVMValueRef))
       

(define-llvm LLVMConstStructInContext
 (_fun (context fields packed) ::
       (context : LLVMContextRef)
       (fields : (_list i LLVMValueRef))
       (_uint = (length fields))
       (packed : LLVMBool)
       -> LLVMValueRef))
     

(define-llvm LLVMConstString
 (_fun (str dnt) ::
       (str : _string)
       (_uint = (string-length str))
       (dnt : LLVMBool)
       -> LLVMValueRef))



(define-llvm LLVMConstStruct
 (_fun (fields packed) ::
       (fields : (_list i LLVMValueRef))
       (_uint = (length fields))
       (packed : LLVMBool)
       -> LLVMValueRef))

(define-llvm LLVMConstNamedStruct
 (_fun (type fields) ::
       (type : LLVMTypeRef)
       (fields : (_list i LLVMValueRef))
       (_uint = (length fields))
       -> LLVMValueRef))


(define-llvm LLVMConstArray
 (_fun (type elements) ::
       (type : LLVMTypeRef)
       (elements : (_list i LLVMValueRef))
       (_uint = (length elements))
       -> LLVMValueRef))


(define-llvm LLVMConstVector
 (_fun (elements) ::
       (elements : (_list i LLVMValueRef))
       (_uint = (length elements))
       -> LLVMValueRef))


(define-llvm LLVMGetConstOpcode (_fun LLVMValueRef -> LLVMOpcode))
(define-llvm-multiple
 (LLVMAlignOf LLVMSizeOf) (_fun LLVMTypeRef -> LLVMValueRef))

(define-llvm-multiple 
 (LLVMConstNeg
  LLVMConstNSWNeg
  LLVMConstNUWNeg
  LLVMConstFNeg
  LLVMConstNot)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple 
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


(define-llvm LLVMConstICmp
 (_fun LLVMIntPredicate LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMConstFCmp
 (_fun LLVMRealPredicate LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm-multiple
 (LLVMConstShl LLVMConstLShr LLVMConstAShr)
 (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))
(define-llvm-multiple (LLVMConstGEP LLVMConstInBoundsGEP)
 (_fun (ptr indices) ::
       (ptr : LLVMValueRef)
       (indices : (_list i LLVMValueRef))
       (_uint = (length indices))
       -> LLVMValueRef))
        

(define-llvm-multiple
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


(define-llvm LLVMConstIntCast
 (_fun LLVMValueRef LLVMTypeRef LLVMBool -> LLVMValueRef))

(define-llvm LLVMConstFPCast
 (_fun LLVMValueRef LLVMTypeRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMConstSelect
  LLVMConstInsertElement
  LLVMConstShuffleVector)
 (_fun LLVMValueRef LLVMValueRef LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMConstExtractElement
 (_fun LLVMValueRef LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMConstExtractValue
 (_fun (agg indices) ::
       (agg : LLVMValueRef)
       (indices : (_list i _uint))
       (_uint = (length indices))
       ->
       LLVMValueRef))


(define-llvm LLVMConstInsertValue
 (_fun (agg elem indices) ::
       (agg : LLVMValueRef)
       (elem : LLVMValueRef)
       (indices : (_list i _uint))
       (_uint = (length indices))
       ->
       LLVMValueRef))

(define-llvm LLVMConstInlineAsm (_fun LLVMTypeRef _string _string LLVMBool LLVMBool -> LLVMValueRef))
