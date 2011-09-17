#lang racket


(provide (all-defined-out))
(require "define.rkt"
         "ctypes.rkt"
         "enums.rkt"
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
(define-llvm-unsafe LLVMGetTypeKind (_fun LLVMTypeRef -> LLVMTypeKind))

;/** See llvm::LLVMType::getContext. */
(define-llvm-unsafe LLVMGetTypeContext (_fun LLVMTypeRef -> LLVMContextRef))

;/* Operations on integer types */

(define-llvm-multiple-unsafe
 (LLVMInt1TypeInContext
  LLVMInt8TypeInContext
  LLVMInt16TypeInContext
  LLVMInt32TypeInContext
  LLVMInt64TypeInContext) (_fun LLVMContextRef -> LLVMTypeRef))

(define-llvm-unsafe LLVMIntTypeInContext (_fun LLVMContextRef _uint -> LLVMTypeRef))

(define-llvm-multiple-unsafe
 (LLVMInt1Type
  LLVMInt8Type
  LLVMInt16Type
  LLVMInt32Type
  LLVMInt64Type) (_fun -> LLVMTypeRef))
(define-llvm-unsafe LLVMIntType (_fun _uint -> LLVMTypeRef))

(define-llvm-unsafe LLVMGetIntTypeWidth (_fun LLVMTypeRef -> _uint))

;/* Operations on real types */
(define-llvm-multiple-unsafe
 (LLVMFloatTypeInContext
  LLVMDoubleTypeInContext
  LLVMX86FP80TypeInContext
  LLVMFP128TypeInContext
  LLVMPPCFP128TypeInContext) (_fun LLVMContextRef -> LLVMTypeRef))

(define-llvm-multiple-unsafe
 (LLVMFloatType
  LLVMDoubleType
  LLVMX86FP80Type
  LLVMFP128Type
  LLVMPPCFP128Type) (_fun -> LLVMTypeRef))

;/* Operations on function types */
(define-llvm-unsafe LLVMFunctionType
  (_fun (ret-type arg-types varargs) ::
        (ret-type : LLVMTypeRef)
        (arg-types : (_list i LLVMTypeRef))
        (_uint = (length arg-types))
        (varargs : LLVMBool)
        -> LLVMTypeRef))


(define-llvm-unsafe LLVMIsFunctionVarArg (_fun LLVMTypeRef -> LLVMBool))
(define-llvm-unsafe LLVMGetReturnType (_fun LLVMTypeRef -> LLVMTypeRef))
(define-llvm-unsafe LLVMCountParamTypes (_fun LLVMTypeRef -> _uint))
(define-llvm-unsafe LLVMGetParamTypes (_fun LLVMTypeRef _pointer -> _void))

;/* Operations on struct types */
(define-llvm-unsafe LLVMStructTypeInContext
  (_fun (context types packed) ::
        (context : LLVMContextRef)
        (types : (_list i LLVMTypeRef))
        (_uint = (length types))
        (packed : LLVMBool)
        -> LLVMTypeRef))

(define-llvm-unsafe LLVMStructType
  (_fun (types packed) ::
        (types : (_list i LLVMTypeRef))
        (_uint = (length types))
        (packed : LLVMBool)
        -> LLVMTypeRef))


(define-llvm-unsafe LLVMStructCreateNamed (_fun LLVMContextRef _string -> LLVMTypeRef))

(define-llvm-unsafe LLVMStructSetBody
 (_fun (type types packed) ::
       (type : LLVMTypeRef)
       (types : (_list i LLVMTypeRef))
       (_uint = (length types))
       (packed : LLVMBool)
       -> _void))


(define-llvm-unsafe LLVMCountStructElementTypes (_fun LLVMTypeRef -> _uint))

(define-llvm-unsafe LLVMGetStructElementTypes
  (_fun (type) ::
        (type : LLVMTypeRef)
        (types : (_list o LLVMTypeRef (unsafe:LLVMCountStructElementTypes type)))
        -> _void
        -> types))
(define-llvm-unsafe LLVMIsPackedStruct (_fun LLVMTypeRef -> LLVMBool))
(define-llvm-unsafe LLVMIsOpaqueStruct (_fun LLVMTypeRef -> LLVMBool))

(define-llvm-unsafe LLVMGetTypeByName (_fun LLVMModuleRef _string -> LLVMTypeRef))


;/* Operations on array, pointer, and vector types (sequence types) */
(define-llvm-multiple-unsafe
 (LLVMArrayType
  LLVMPointerType
  LLVMVectorType) (_fun LLVMTypeRef _uint -> LLVMTypeRef))

(define-llvm-unsafe LLVMGetElementType (_fun LLVMTypeRef -> LLVMTypeRef))

(define-llvm-multiple-unsafe
 (LLVMGetArrayLength
  LLVMGetPointerAddressSpace
  LLVMGetVectorSize) (_fun LLVMTypeRef -> _uint))


;/* Operations on other types */

(define-llvm-multiple-unsafe
 (LLVMVoidTypeInContext
  LLVMLabelTypeInContext
  LLVMX86MMXTypeInContext) (_fun LLVMContextRef -> LLVMTypeRef))

(define-llvm-multiple-unsafe
 (LLVMVoidType
  LLVMLabelType
  LLVMX86MMXType) (_fun -> LLVMTypeRef))

