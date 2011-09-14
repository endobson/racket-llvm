#lang racket


(provide (all-defined-out))
(require "llvm-define.rkt"
         "llvm-ctypes.rkt"
         "llvm-enums.rkt"
         ffi/unsafe)

;/*===-- Types -------------------------------------------------------------===*/

;/* LLVM types conform to the following hierarchy:
; * 
; *   types:
; *     integer type
; *     real type
; *     function type
; *     sequence types:
; *       array type
; *       pointer type
; *       vector type
; *     void type
; *     label type
; *     opaque type
; */

;/** See llvm::LLVMTypeKind::getTypeID. */
(define-llvm-both LLVMGetTypeKind (_fun LLVMTypeRef -> LLVMTypeKind))

;/** See llvm::LLVMType::getContext. */
(define-llvm LLVMGetTypeContext (_fun LLVMTypeRef -> LLVMContextRef))

;/* Operations on integer types */

(define-llvm-multiple
 (LLVMInt1TypeInContext
  LLVMInt8TypeInContext
  LLVMInt16TypeInContext
  LLVMInt32TypeInContext
  LLVMInt64TypeInContext) (_fun LLVMContextRef -> LLVMTypeRef))

(define-llvm LLVMIntTypeInContext (_fun LLVMContextRef _uint -> LLVMTypeRef))

(define-llvm-multiple
 (LLVMInt1Type
  LLVMInt8Type
  LLVMInt16Type
  LLVMInt32Type
  LLVMInt64Type) (_fun -> LLVMTypeRef))
(define-llvm LLVMIntType (_fun _uint -> LLVMTypeRef))

(define-llvm LLVMGetIntTypeWidth (_fun LLVMTypeRef -> _uint))

;/* Operations on real types */
(define-llvm-multiple
 (LLVMFloatTypeInContext
  LLVMDoubleTypeInContext
  LLVMX86FP80TypeInContext
  LLVMFP128TypeInContext
  LLVMPPCFP128TypeInContext) (_fun LLVMContextRef -> LLVMTypeRef))

(define-llvm-multiple
 (LLVMFloatType
  LLVMDoubleType
  LLVMX86FP80Type
  LLVMFP128Type
  LLVMPPCFP128Type) (_fun -> LLVMTypeRef))

;/* Operations on function types */
(define-llvm LLVMFunctionType
  (_fun (ret-type arg-types varargs) ::
        (ret-type : LLVMTypeRef)
        (arg-types : (_list i LLVMTypeRef))
        (_uint = (length arg-types))
        (varargs : LLVMBool)
        -> LLVMTypeRef))


(define-llvm LLVMIsFunctionVarArg (_fun LLVMTypeRef -> LLVMBool))
(define-llvm LLVMGetReturnType (_fun LLVMTypeRef -> LLVMTypeRef))
(define-llvm LLVMCountParamTypes (_fun LLVMTypeRef -> _uint))
(define-llvm LLVMGetParamTypes (_fun LLVMTypeRef _pointer -> _void))

;/* Operations on struct types */
(define-llvm LLVMStructTypeInContext
  (_fun (context types packed) ::
        (context : LLVMContextRef)
        (types : (_list i LLVMTypeRef))
        (_uint = (length types))
        (packed : LLVMBool)
        -> LLVMTypeRef))

(define-llvm LLVMStructType
  (_fun (types packed) ::
        (types : (_list i LLVMTypeRef))
        (_uint = (length types))
        (packed : LLVMBool)
        -> LLVMTypeRef))


(define-llvm LLVMStructCreateNamed (_fun LLVMContextRef _string -> LLVMTypeRef))

(define-llvm LLVMStructSetBody
 (_fun (type types packed) ::
       (type : LLVMTypeRef)
       (types : (_list i LLVMTypeRef))
       (_uint = (length types))
       (packed : LLVMBool)
       -> _void))


(define-llvm LLVMCountStructElementTypes (_fun LLVMTypeRef -> _uint))

(define-llvm LLVMGetStructElementTypes
  (_fun (type) ::
        (type : LLVMTypeRef)
        (types : (_list o LLVMTypeRef (LLVMCountStructElementTypes type)))
        -> _void
        -> types))
(define-llvm LLVMIsPackedStruct (_fun LLVMTypeRef -> LLVMBool))
(define-llvm LLVMIsOpaqueStruct (_fun LLVMTypeRef -> LLVMBool))

(define-llvm LLVMGetTypeByName (_fun LLVMModuleRef _string -> LLVMTypeRef))


;/* Operations on array, pointer, and vector types (sequence types) */
(define-llvm-multiple
 (LLVMArrayType
  LLVMPointerType
  LLVMVectorType) (_fun LLVMTypeRef _uint -> LLVMTypeRef))

(define-llvm LLVMGetElementType (_fun LLVMTypeRef -> LLVMTypeRef))

(define-llvm-multiple
 (LLVMGetArrayLength
  LLVMGetPointerAddressSpace
  LLVMGetVectorSize) (_fun LLVMTypeRef -> _uint))


;/* Operations on other types */

(define-llvm-multiple
 (LLVMVoidTypeInContext
  LLVMLabelTypeInContext
  LLVMX86MMXTypeInContext) (_fun LLVMContextRef -> LLVMTypeRef))

(define-llvm-multiple
 (LLVMVoidType
  LLVMLabelType
  LLVMX86MMXType) (_fun -> LLVMTypeRef))

