#lang racket
(require srfi/13)
(require ffi/unsafe)
(require ffi/unsafe/define)

(provide (all-defined-out))

(define llvm-version-string
 (let-values (((process out in err) (subprocess #f #f #f "/usr/bin/env" "llvm-config" "--version")))
  (begin0
   (string-trim-both (port->string out))
   (close-output-port in)
   (close-input-port err)
   (close-input-port out)
   (subprocess-wait process)
   (unless (= (subprocess-status process) 0) (error 'llvm-config "Returned non zero exit code")))))

(define llvm-lib-path
 (let-values (((process out in err) (subprocess #f #f #f "/usr/bin/env" "llvm-config" "--libdir")))
  (begin0
   (string-trim-both (port->string out))
   (close-output-port in)
   (close-input-port err)
   (close-input-port out)
   (subprocess-wait process)
   (unless (= (subprocess-status process) 0) (error 'llvm-config "Returned non zero exit code")))))



(define llvm-lib (ffi-lib (build-path llvm-lib-path (string-append "libLLVM-" llvm-version-string))))

(define-ffi-definer define-llvm llvm-lib)

(define-syntax (define-llvm-multiple stx)
 (syntax-case stx ()
  ((_ (name ...) type) 
    #'(begin (define-llvm name type) ...))))

(define LLVMBool _bool)

(struct llvm-context-ref (pointer))
(struct llvm-module-ref (pointer))
(struct llvm-type-ref (pointer))
(struct llvm-type-handle-ref (pointer))
(struct llvm-value-ref (pointer))
(struct llvm-basic-block-ref (pointer))
(struct llvm-builder-ref (pointer))
(struct llvm-module-provider-ref (pointer))
(struct llvm-memory-buffer-ref (pointer))
(struct llvm-pass-manager-ref (pointer))
(struct llvm-use-ref (pointer))

(define LLVMContextRef
 (make-ctype _pointer llvm-context-ref-pointer llvm-context-ref))
(define LLVMModuleRef
 (make-ctype _pointer llvm-module-ref-pointer llvm-module-ref))
(define LLVMTypeRef
 (make-ctype _pointer llvm-type-ref-pointer llvm-type-ref))
(define LLVMTypeHandleRef
 (make-ctype _pointer llvm-type-handle-ref-pointer llvm-type-handle-ref))
(define LLVMValueRef
 (make-ctype _pointer llvm-value-ref-pointer llvm-value-ref))
(define LLVMBasicBlockRef
 (make-ctype _pointer llvm-basic-block-ref-pointer llvm-basic-block-ref))
(define LLVMBuilderRef
 (make-ctype _pointer llvm-builder-ref-pointer llvm-builder-ref))
(define LLVMModuleProviderRef
 (make-ctype _pointer llvm-module-provider-ref-pointer llvm-module-provider-ref))
(define LLVMMemoryBufferRef
 (make-ctype _pointer llvm-memory-buffer-ref-pointer llvm-memory-buffer-ref))
(define LLVMPassManagerRef
 (make-ctype _pointer llvm-pass-manager-ref-pointer llvm-pass-manager-ref))
(define LLVMUseRef
 (make-ctype _pointer llvm-use-ref-pointer llvm-use-ref))

(define (make-byte-string ptr)
 (let loop ((i 0))
  (cond
   ((zero? (ptr-ref ptr _byte i))
    (make-sized-byte-string ptr i))
   (else (loop (add1 i))))))

(define LLVMMessage 
 (make-ctype
  _pointer
  (lambda (scheme)
   (when scheme
    (error 'LLVMMessage "Cannot Convert non null pointers to C"))
   #f)
  (lambda (cptr)
   (and cptr
    (begin0
     (bytes->string/utf-8 (make-byte-string cptr))
     (LLVMDisposeMessage cptr))))))





(define << arithmetic-shift)
(define LLVMAttribute (_enum `(
    LLVMZExtAttribute            = ,(<< 1 0)
    LLVMSExtAttribute            = ,(<< 1 1)
    LLVMNoReturnAttribute        = ,(<< 1 2)
    LLVMInRegAttribute           = ,(<< 1 3)
    LLVMStructRetAttribute       = ,(<< 1 4)
    LLVMNoUnwindAttribute        = ,(<< 1 5)
    LLVMNoAliasAttribute         = ,(<< 1 6)
    LLVMByValAttribute           = ,(<< 1 7)
    LLVMNestAttribute            = ,(<< 1 8)
    LLVMReadNoneAttribute        = ,(<< 1 9)
    LLVMReadOnlyAttribute        = ,(<< 1 10)
    LLVMNoInlineAttribute        = ,(<< 1 11)
    LLVMAlwaysInlineAttribute    = ,(<< 1 12)
    LLVMOptimizeForSizeAttribute = ,(<< 1 13)
    LLVMStackProtectAttribute    = ,(<< 1 14)
    LLVMStackProtectReqAttribute = ,(<< 1 15)
    LLVMAlignment                = ,(<< 31 16)
    LLVMNoCaptureAttribute       = ,(<< 1 21)
    LLVMNoRedZoneAttribute       = ,(<< 1 22)
    LLVMNoImplicitFloatAttribute = ,(<< 1 23)
    LLVMNakedAttribute           = ,(<< 1 24)
    LLVMInlineHintAttribute      = ,(<< 1 25)
    LLVMStackAlignment           = ,(<< 7 26))))


(define LLVMOpcode (_enum `(
  ;/* Terminator Instructions */
  LLVMRet            = ,1
  LLVMBr             = ,2
  LLVMSwitch         = ,3
  LLVMIndirectBr     = ,4
  LLVMInvoke         = ,5
  LLVMUnwind         = ,6
  LLVMUnreachable    = ,7

  ;/* Standard Binary Operators */
  LLVMAdd            = ,8
  LLVMFAdd           = ,9
  LLVMSub            = ,10
  LLVMFSub           = ,11
  LLVMMul            = ,12
  LLVMFMul           = ,13
  LLVMUDiv           = ,14
  LLVMSDiv           = ,15
  LLVMFDiv           = ,16
  LLVMURem           = ,17
  LLVMSRem           = ,18
  LLVMFRem           = ,19

  ;/* Logical Operators */
  LLVMShl            = ,20
  LLVMLShr           = ,21
  LLVMAShr           = ,22
  LLVMAnd            = ,23
  LLVMOr             = ,24
  LLVMXor            = ,25

  ;/* Memory Operators */
  LLVMAlloca         = ,26
  LLVMLoad           = ,27
  LLVMStore          = ,28
  LLVMGetElementPtr  = ,29

  ;/* Cast Operators */
  LLVMTrunc          = ,30
  LLVMZExt           = ,31
  LLVMSExt           = ,32
  LLVMFPToUI         = ,33
  LLVMFPToSI         = ,34
  LLVMUIToFP         = ,35
  LLVMSIToFP         = ,36
  LLVMFPTrunc        = ,37
  LLVMFPExt          = ,38
  LLVMPtrToInt       = ,39
  LLVMIntToPtr       = ,40
  LLVMBitCast        = ,41

  ;/* Other Operators */
  LLVMICmp           = ,42
  LLVMFCmp           = ,43
  LLVMPHI            = ,44
  LLVMCall           = ,45
  LLVMSelect         = ,46
  ;/* UserOp1 */
  ;/* UserOp2 */
  LLVMVAArg          = ,49
  LLVMExtractElement = ,50
  LLVMInsertElement  = ,51
  LLVMShuffleVector  = ,52
  LLVMExtractValue   = ,53
  LLVMInsertValue    = ,54)))

(define LLVMTypeKind (_enum '(
  LLVMVoidTypeKind        ;/**< type with no size */
  LLVMFloatTypeKind       ;/**< 32 bit floating point type */
  LLVMDoubleTypeKind      ;/**< 64 bit floating point type */
  LLVMX86_FP80TypeKind    ;/**< 80 bit floating point type (X87) */
  LLVMFP128TypeKind       ;/**< 128 bit floating point type (112-bit mantissa)*/
  LLVMPPC_FP128TypeKind   ;/**< 128 bit floating point type (two 64-bits) */
  LLVMLabelTypeKind       ;/**< Labels */
  LLVMIntegerTypeKind     ;/**< Arbitrary bit width integers */
  LLVMFunctionTypeKind    ;/**< Functions */
  LLVMStructTypeKind      ;/**< Structures */
  LLVMArrayTypeKind       ;/**< Arrays */
  LLVMPointerTypeKind     ;/**< Pointers */
  LLVMOpaqueTypeKind      ;/**< Opaque: type with unknown structure */
  LLVMVectorTypeKind      ;/**< SIMD 'packed' format, or other vector type */
  LLVMMetadataTypeKind)))     ;/**< Metadata */

(define LLVMLinkage (_enum '(
  LLVMExternalLinkage    ;/**< Externally visible function */
  LLVMAvailableExternallyLinkage
  LLVMLinkOnceAnyLinkage ;/**< Keep one copy of function when linking (inline)*/
  LLVMLinkOnceODRLinkage ;/**< Same, but only replaced by something equivalent. */
  LLVMWeakAnyLinkage     ;/**< Keep one copy of function when linking (weak) */
  LLVMWeakODRLinkage     ;/**< Same, but only replaced by something equivalent. */
  LLVMAppendingLinkage   ;/**< Special purpose, only applies to global arrays */
  LLVMInternalLinkage    ;/**< Rename collisions when linking (static functions) */
  LLVMPrivateLinkage     ;/**< Like Internal, but omit from symbol table */
  LLVMDLLImportLinkage   ;/**< Function to be imported from DLL */
  LLVMDLLExportLinkage   ;/**< Function to be accessible from DLL */
  LLVMExternalWeakLinkage;/**< ExternalWeak linkage description */
  LLVMGhostLinkage       ;/**< Obsolete */
  LLVMCommonLinkage      ;/**< Tentative definitions */
  LLVMLinkerPrivateLinkage ;/**< Like Private, but linker removes. */
  LLVMLinkerPrivateWeakLinkage ;/**< Like LinkerPrivate, but is weak. */
  LLVMLinkerPrivateWeakDefAutoLinkage))) ;/**< Like LinkerPrivateWeak, but possibly hidden. */

(define LLVMVisibility (_enum '(
  LLVMDefaultVisibility      ;/**< The GV is visible */
  LLVMHiddenVisibility       ;/**< The GV is hidden */
  LLVMProtectedVisibility))) ;/**< The GV is protected */

(define LLVMCallConv (_enum `(
  LLVMCCallConv           = ,0
  LLVMFastCallConv        = ,8
  LLVMColdCallConv        = ,9
  LLVMX86StdcallCallConv  = ,64
  LLVMX86FastcallCallConv = ,65)))

(define LLVMIntPredicate (_enum `(
  LLVMIntEQ = ,32  ;/**< equal */
  LLVMIntNE        ;/**< not equal */
  LLVMIntUGT       ;/**< unsigned greater than */
  LLVMIntUGE       ;/**< unsigned greater or equal */
  LLVMIntULT       ;/**< unsigned less than */
  LLVMIntULE       ;/**< unsigned less or equal */
  LLVMIntSGT       ;/**< signed greater than */
  LLVMIntSGE       ;/**< signed greater or equal */
  LLVMIntSLT       ;/**< signed less than */
  LLVMIntSLE)))     ;/**< signed less or equal */

(define LLVMRealPredicate (_enum '(
  LLVMRealPredicateFalse ;/**< Always false (always folded) */
  LLVMRealOEQ            ;/**< True if ordered and equal */
  LLVMRealOGT            ;/**< True if ordered and greater than */
  LLVMRealOGE            ;/**< True if ordered and greater than or equal */
  LLVMRealOLT            ;/**< True if ordered and less than */
  LLVMRealOLE            ;/**< True if ordered and less than or equal */
  LLVMRealONE            ;/**< True if ordered and operands are unequal */
  LLVMRealORD            ;/**< True if ordered (no nans) */
  LLVMRealUNO            ;/**< True if unordered: isnan(X) | isnan(Y) */
  LLVMRealUEQ            ;/**< True if unordered or equal */
  LLVMRealUGT            ;/**< True if unordered or greater than */
  LLVMRealUGE            ;/**< True if unordered, greater than, or equal */
  LLVMRealULT            ;/**< True if unordered or less than */
  LLVMRealULE            ;/**< True if unordered, less than, or equal */
  LLVMRealUNE            ;/**< True if unordered or not equal */
  LLVMRealPredicateTrue)))  ;/**< Always true (always folded) */

;/*===-- Error handling ----------------------------------------------------===*/

(define-llvm LLVMDisposeMessage (_fun _pointer -> _void))

;/*===-- Contexts ----------------------------------------------------------===*/


;/* Create and destroy contexts. */
(define-llvm LLVMContextCreate (_fun -> LLVMContextRef))
(define-llvm LLVMGetGlobalContext (_fun -> LLVMContextRef))
(define-llvm LLVMContextDispose (_fun LLVMContextRef -> _void))

(define-llvm LLVMGetMDKindIDInContext (_fun LLVMContextRef _string _uint -> _uint))

(define-llvm LLVMGetMDKindID (_fun _string _uint -> _uint))



;/*===-- Modules -----------------------------------------------------------===*/

;/* Create and destroy modules. */ 
;/** See llvm::Module::Module. */
(define-llvm LLVMModuleCreateWithName (_fun _string -> LLVMModuleRef))
(define-llvm LLVMModuleCreateWithNameInContext (_fun _string LLVMContextRef -> LLVMModuleRef))

;/** See llvm::Module::~Module. */
(define-llvm LLVMDisposeModule (_fun LLVMModuleRef -> _void))


;/** Data layout. See Module::getDataLayout. */
(define-llvm LLVMGetDataLayout (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetDataLayout (_fun LLVMModuleRef _string -> _void))

;/** Target triple. See Module::getTargetTriple. */
(define-llvm LLVMGetTarget (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetTarget (_fun LLVMModuleRef _string -> _void))

;/** See Module::addTypeName. */
(define-llvm LLVMAddTypeName (_fun LLVMModuleRef _string LLVMTypeRef -> LLVMBool))
(define-llvm LLVMDeleteTypeName (_fun LLVMModuleRef _string -> _void))
(define-llvm LLVMGetTypeByName (_fun LLVMModuleRef _string -> LLVMTypeRef))

;/** See Module::dump. */
(define-llvm LLVMDumpModule (_fun LLVMModuleRef -> _void))

;/** See Module::setModuleInlineAsm. */
;Not in 2.7
;(define-llvm LLVMSetModuleInlineAsm (_fun LLVMModuleRef _string -> _void))

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
(define-llvm LLVMGetTypeKind (_fun LLVMTypeRef -> LLVMTypeKind))

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

(define-llvm LLVMCountStructElementTypes (_fun LLVMTypeRef -> _uint))
;void LLVMGetStructElementTypes(LLVMTypeRef StructTy, LLVMTypeRef *Dest);
(define-llvm LLVMIsPackedStruct (_fun LLVMTypeRef -> LLVMBool))

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
  LLVMOpaqueTypeInContext) (_fun LLVMContextRef -> LLVMTypeRef))

(define-llvm-multiple
 (LLVMVoidType
  LLVMLabelType
  LLVMOpaqueType) (_fun -> LLVMTypeRef))

;/* Operations on type handles */

(define-llvm LLVMCreateTypeHandle (_fun LLVMTypeRef -> LLVMTypeHandleRef))
(define-llvm LLVMRefineType (_fun LLVMTypeRef LLVMTypeRef -> _void))
(define-llvm LLVMResolveTypeHandle (_fun LLVMTypeHandleRef -> LLVMTypeRef))
(define-llvm LLVMDisposeTypeHandle (_fun LLVMTypeHandleRef -> _void))



;/*===-- Values ------------------------------------------------------------===*/

;/* The bulk of LLVM's object model consists of values, which comprise a very
; * rich type hierarchy.
; */

#|
#define LLVM_FOR_EACH_VALUE_SUBCLASS(macro) \
  macro(Argument)                           \
  macro(BasicBlock)                         \
  macro(InlineAsm)                          \
  macro(User)                               \
    macro(Constant)                         \
      macro(ConstantAggregateZero)          \
      macro(ConstantArray)                  \
      macro(ConstantExpr)                   \
      macro(ConstantFP)                     \
      macro(ConstantInt)                    \
      macro(ConstantPointerNull)            \
      macro(ConstantStruct)                 \
      macro(ConstantVector)                 \
      macro(GlobalValue)                    \
        macro(Function)                     \
        macro(GlobalAlias)                  \
        macro(GlobalVariable)               \
      macro(UndefValue)                     \
    macro(Instruction)                      \
      macro(BinaryOperator)                 \
      macro(CallInst)                       \
        macro(IntrinsicInst)                \
          macro(DbgInfoIntrinsic)           \
            macro(DbgDeclareInst)           \
          macro(EHSelectorInst)             \
          macro(MemIntrinsic)               \
            macro(MemCpyInst)               \
            macro(MemMoveInst)              \
            macro(MemSetInst)               \
      macro(CmpInst)                        \
      macro(FCmpInst)                       \
      macro(ICmpInst)                       \
      macro(ExtractElementInst)             \
      macro(GetElementPtrInst)              \
      macro(InsertElementInst)              \
      macro(InsertValueInst)                \
      macro(PHINode)                        \
      macro(SelectInst)                     \
      macro(ShuffleVectorInst)              \
      macro(StoreInst)                      \
      macro(TerminatorInst)                 \
        macro(BranchInst)                   \
        macro(InvokeInst)                   \
        macro(ReturnInst)                   \
        macro(SwitchInst)                   \
        macro(UnreachableInst)              \
        macro(UnwindInst)                   \
    macro(UnaryInstruction)                 \
      macro(AllocaInst)                     \
      macro(CastInst)                       \
        macro(BitCastInst)                  \
        macro(FPExtInst)                    \
        macro(FPToSIInst)                   \
        macro(FPToUIInst)                   \
        macro(FPTruncInst)                  \
        macro(IntToPtrInst)                 \
        macro(PtrToIntInst)                 \
        macro(SExtInst)                     \
        macro(SIToFPInst)                   \
        macro(TruncInst)                    \
        macro(UIToFPInst)                   \
        macro(ZExtInst)                     \
      macro(ExtractValueInst)               \
      macro(LoadInst)                       \
      macro(VAArgInst)
|#
;/* Operations on all values */
(define-llvm LLVMTypeOf (_fun LLVMValueRef -> LLVMTypeRef))
(define-llvm LLVMGetValueName (_fun LLVMValueRef -> _string))

(define-llvm LLVMSetValueName (_fun LLVMValueRef _string -> _void))

(define-llvm LLVMDumpValue (_fun LLVMValueRef -> _void))
(define-llvm LLVMReplaceAllUsesWith (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm LLVMHasMetadata (_fun LLVMValueRef -> _int))
(define-llvm LLVMGetMetadata (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMSetMetadata (_fun LLVMValueRef _uint LLVMValueRef -> _void))


;/* Conversion functions. Return the input value if it is an instance of the
;   specified class, otherwise NULL. See llvm::dyn_cast_or_null<>. */
#|
#define LLVM_DECLARE_VALUE_CAST(name) \
  LLVMValueRef LLVMIsA##name(LLVMValueRef Val);
LLVM_FOR_EACH_VALUE_SUBCLASS(LLVM_DECLARE_VALUE_CAST)
|#

;/* Operations on Uses */
(define-llvm LLVMGetFirstUse (_fun LLVMValueRef -> LLVMUseRef))
(define-llvm LLVMGetNextUse (_fun LLVMUseRef -> LLVMUseRef))
(define-llvm-multiple
 (LLVMGetUser LLVMGetUsedValue)
 (_fun LLVMUseRef -> LLVMValueRef))

;/* Operations on Users */
(define-llvm LLVMGetOperand (_fun LLVMValueRef _uint -> LLVMValueRef))
;Not in 2.7
;(define-llvm LLVMSetOperand (_fun LLVMValueRef _uint LLVMValueRef -> _void))
;(define-llvm LLVMGetNumOperands (_fun LLVMValueRef -> _int))

;/* Operations on constants of any type */
(define-llvm-multiple
 (LLVMConstNull      ; /* all zeroes */
  LLVMConstAllOnes   ; /* only for int/vector */
  LLVMGetUndef
  LLVMConstPointerNull)
 (_fun LLVMTypeRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMIsConstant
  LLVMIsNull
  LLVMIsUndef)
 (_fun LLVMValueRef -> LLVMBool))


;/* Operations on metadata */
(define-llvm LLVMMDStringInContext
 (_fun LLVMContextRef _string _uint -> LLVMValueRef))
(define-llvm LLVMMDString
 (_fun _string _uint -> LLVMValueRef))

(define-llvm LLVMMDNodeInContext
 (_fun LLVMContextRef LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMMDNode
 (_fun LLVMValueRef _uint -> LLVMValueRef))

;/* Operations on scalar constants */
(define-llvm LLVMConstInt (_fun LLVMTypeRef _ulong LLVMBool -> LLVMValueRef))
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
#|

/* Operations on composite constants */
|#
(define-llvm LLVMConstStringInContext
 (_fun (context str dnt) ::
       (context : LLVMContextRef)
       (str : _string)
       (_uint = (string-length str))
       (dnt : LLVMBool)
       -> LLVMValueRef))
       

#|
LLVMValueRef LLVMConstStructInContext(LLVMContextRef C, 
                                      LLVMValueRef *ConstantVals,
                                      unsigned Count, LLVMBool Packed);

LLVMValueRef LLVMConstString(const char *Str, unsigned Length,
                             LLVMBool DontNullTerminate);
LLVMValueRef LLVMConstArray(LLVMTypeRef ElementTy,
                            LLVMValueRef *ConstantVals, unsigned Length);
LLVMValueRef LLVMConstStruct(LLVMValueRef *ConstantVals, unsigned Count,
                             LLVMBool Packed);
LLVMValueRef LLVMConstVector(LLVMValueRef *ScalarConstantVals, unsigned Size);

/* Constant expressions */
LLVMOpcode LLVMGetConstOpcode(LLVMValueRef ConstantVal);
LLVMValueRef LLVMAlignOf(LLVMTypeRef Ty);
LLVMValueRef LLVMSizeOf(LLVMTypeRef Ty);
LLVMValueRef LLVMConstNeg(LLVMValueRef ConstantVal);
LLVMValueRef LLVMConstNSWNeg(LLVMValueRef ConstantVal);
LLVMValueRef LLVMConstNUWNeg(LLVMValueRef ConstantVal);
LLVMValueRef LLVMConstFNeg(LLVMValueRef ConstantVal);
LLVMValueRef LLVMConstNot(LLVMValueRef ConstantVal);
LLVMValueRef LLVMConstAdd(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstNSWAdd(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstNUWAdd(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstFAdd(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstSub(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstNSWSub(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstNUWSub(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstFSub(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstMul(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstNSWMul(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstNUWMul(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstFMul(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstUDiv(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstSDiv(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstExactSDiv(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstFDiv(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstURem(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstSRem(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstFRem(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstAnd(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstOr(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstXor(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstICmp(LLVMIntPredicate Predicate,
                           LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstFCmp(LLVMRealPredicate Predicate,
                           LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstShl(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstLShr(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstAShr(LLVMValueRef LHSConstant, LLVMValueRef RHSConstant);
LLVMValueRef LLVMConstGEP(LLVMValueRef ConstantVal,
                          LLVMValueRef *ConstantIndices, unsigned NumIndices);
LLVMValueRef LLVMConstInBoundsGEP(LLVMValueRef ConstantVal,
                                  LLVMValueRef *ConstantIndices,
                                  unsigned NumIndices);
LLVMValueRef LLVMConstTrunc(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstSExt(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstZExt(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstFPTrunc(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstFPExt(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstUIToFP(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstSIToFP(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstFPToUI(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstFPToSI(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstPtrToInt(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstIntToPtr(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstBitCast(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstZExtOrBitCast(LLVMValueRef ConstantVal,
                                    LLVMTypeRef ToType);
LLVMValueRef LLVMConstSExtOrBitCast(LLVMValueRef ConstantVal,
                                    LLVMTypeRef ToType);
LLVMValueRef LLVMConstTruncOrBitCast(LLVMValueRef ConstantVal,
                                     LLVMTypeRef ToType);
LLVMValueRef LLVMConstPointerCast(LLVMValueRef ConstantVal,
                                  LLVMTypeRef ToType);
LLVMValueRef LLVMConstIntCast(LLVMValueRef ConstantVal, LLVMTypeRef ToType,
                              LLVMBool isSigned);
LLVMValueRef LLVMConstFPCast(LLVMValueRef ConstantVal, LLVMTypeRef ToType);
LLVMValueRef LLVMConstSelect(LLVMValueRef ConstantCondition,
                             LLVMValueRef ConstantIfTrue,
                             LLVMValueRef ConstantIfFalse);
LLVMValueRef LLVMConstExtractElement(LLVMValueRef VectorConstant,
                                     LLVMValueRef IndexConstant);
LLVMValueRef LLVMConstInsertElement(LLVMValueRef VectorConstant,
                                    LLVMValueRef ElementValueConstant,
                                    LLVMValueRef IndexConstant);
LLVMValueRef LLVMConstShuffleVector(LLVMValueRef VectorAConstant,
                                    LLVMValueRef VectorBConstant,
                                    LLVMValueRef MaskConstant);
LLVMValueRef LLVMConstExtractValue(LLVMValueRef AggConstant, unsigned *IdxList,
                                   unsigned NumIdx);
LLVMValueRef LLVMConstInsertValue(LLVMValueRef AggConstant,
                                  LLVMValueRef ElementValueConstant,
                                  unsigned *IdxList, unsigned NumIdx);
LLVMValueRef LLVMConstInlineAsm(LLVMTypeRef Ty,
                                const char *AsmString, const char *Constraints,
                                LLVMBool HasSideEffects, LLVMBool IsAlignStack);
LLVMValueRef LLVMBlockAddress(LLVMValueRef F, LLVMBasicBlockRef BB);

/* Operations on global variables, functions, and aliases (globals) */
|#
(define-llvm LLVMGetGlobalParent (_fun LLVMValueRef -> LLVMModuleRef))
#|
LLVMBool LLVMIsDeclaration(LLVMValueRef Global);
LLVMLinkage LLVMGetLinkage(LLVMValueRef Global);
void LLVMSetLinkage(LLVMValueRef Global, LLVMLinkage Linkage);
const char *LLVMGetSection(LLVMValueRef Global);
void LLVMSetSection(LLVMValueRef Global, const char *Section);
LLVMVisibility LLVMGetVisibility(LLVMValueRef Global);
void LLVMSetVisibility(LLVMValueRef Global, LLVMVisibility Viz);
unsigned LLVMGetAlignment(LLVMValueRef Global);
void LLVMSetAlignment(LLVMValueRef Global, unsigned Bytes);

/* Operations on global variables */
LLVMValueRef LLVMAddGlobal(LLVMModuleRef M, LLVMTypeRef Ty, const char *Name);
LLVMValueRef LLVMAddGlobalInAddressSpace(LLVMModuleRef M, LLVMTypeRef Ty,
                                         const char *Name,
                                         unsigned AddressSpace);
LLVMValueRef LLVMGetNamedGlobal(LLVMModuleRef M, const char *Name);
LLVMValueRef LLVMGetFirstGlobal(LLVMModuleRef M);
LLVMValueRef LLVMGetLastGlobal(LLVMModuleRef M);
LLVMValueRef LLVMGetNextGlobal(LLVMValueRef GlobalVar);
LLVMValueRef LLVMGetPreviousGlobal(LLVMValueRef GlobalVar);
void LLVMDeleteGlobal(LLVMValueRef GlobalVar);
LLVMValueRef LLVMGetInitializer(LLVMValueRef GlobalVar);
void LLVMSetInitializer(LLVMValueRef GlobalVar, LLVMValueRef ConstantVal);
LLVMBool LLVMIsThreadLocal(LLVMValueRef GlobalVar);
void LLVMSetThreadLocal(LLVMValueRef GlobalVar, LLVMBool IsThreadLocal);
LLVMBool LLVMIsGlobalConstant(LLVMValueRef GlobalVar);
void LLVMSetGlobalConstant(LLVMValueRef GlobalVar, LLVMBool IsConstant);

/* Operations on aliases */
LLVMValueRef LLVMAddAlias(LLVMModuleRef M, LLVMTypeRef Ty, LLVMValueRef Aliasee,
                          const char *Name);

/* Operations on functions */
|#
(define-llvm LLVMAddFunction (_fun LLVMModuleRef _string LLVMTypeRef -> LLVMValueRef))

(define-llvm LLVMGetNamedFunction (_fun LLVMModuleRef _string -> LLVMValueRef))

#|
LLVMValueRef LLVMGetFirstFunction(LLVMModuleRef M);
LLVMValueRef LLVMGetLastFunction(LLVMModuleRef M);
LLVMValueRef LLVMGetNextFunction(LLVMValueRef Fn);
LLVMValueRef LLVMGetPreviousFunction(LLVMValueRef Fn);
void LLVMDeleteFunction(LLVMValueRef Fn);
unsigned LLVMGetIntrinsicID(LLVMValueRef Fn);
unsigned LLVMGetFunctionCallConv(LLVMValueRef Fn);
void LLVMSetFunctionCallConv(LLVMValueRef Fn, unsigned CC);
const char *LLVMGetGC(LLVMValueRef Fn);
|#
(define-llvm LLVMSetGC (_fun LLVMValueRef _string -> _void))
#|
void LLVMAddFunctionAttr(LLVMValueRef Fn, LLVMAttribute PA);
LLVMAttribute LLVMGetFunctionAttr(LLVMValueRef Fn);
void LLVMRemoveFunctionAttr(LLVMValueRef Fn, LLVMAttribute PA);

/* Operations on parameters */
unsigned LLVMCountParams(LLVMValueRef Fn);
void LLVMGetParams(LLVMValueRef Fn, LLVMValueRef *Params);
|#
(define-llvm LLVMGetParam (_fun LLVMValueRef _uint -> LLVMValueRef))
#|
LLVMValueRef LLVMGetParamParent(LLVMValueRef Inst);
LLVMValueRef LLVMGetFirstParam(LLVMValueRef Fn);
LLVMValueRef LLVMGetLastParam(LLVMValueRef Fn);
LLVMValueRef LLVMGetNextParam(LLVMValueRef Arg);
LLVMValueRef LLVMGetPreviousParam(LLVMValueRef Arg);
void LLVMAddAttribute(LLVMValueRef Arg, LLVMAttribute PA);
void LLVMRemoveAttribute(LLVMValueRef Arg, LLVMAttribute PA);
LLVMAttribute LLVMGetAttribute(LLVMValueRef Arg);
void LLVMSetParamAlignment(LLVMValueRef Arg, unsigned align);

/* Operations on basic blocks */
LLVMValueRef LLVMBasicBlockAsValue(LLVMBasicBlockRef BB);
LLVMBool LLVMValueIsBasicBlock(LLVMValueRef Val);
LLVMBasicBlockRef LLVMValueAsBasicBlock(LLVMValueRef Val);
|#
(define-llvm LLVMGetBasicBlockParent (_fun LLVMBasicBlockRef -> LLVMValueRef))
#|
unsigned LLVMCountBasicBlocks(LLVMValueRef Fn);
void LLVMGetBasicBlocks(LLVMValueRef Fn, LLVMBasicBlockRef *BasicBlocks);
LLVMBasicBlockRef LLVMGetFirstBasicBlock(LLVMValueRef Fn);
LLVMBasicBlockRef LLVMGetLastBasicBlock(LLVMValueRef Fn);
LLVMBasicBlockRef LLVMGetNextBasicBlock(LLVMBasicBlockRef BB);
LLVMBasicBlockRef LLVMGetPreviousBasicBlock(LLVMBasicBlockRef BB);
LLVMBasicBlockRef LLVMGetEntryBasicBlock(LLVMValueRef Fn);

|#
(define-llvm LLVMAppendBasicBlockInContext (_fun LLVMContextRef LLVMValueRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMInsertBasicBlockInContext (_fun LLVMContextRef LLVMBasicBlockRef _string -> LLVMBasicBlockRef))
#|

LLVMBasicBlockRef LLVMAppendBasicBlock(LLVMValueRef Fn, const char *Name);
LLVMBasicBlockRef LLVMInsertBasicBlock(LLVMBasicBlockRef InsertBeforeBB,
                                       const char *Name);
void LLVMDeleteBasicBlock(LLVMBasicBlockRef BB);

void LLVMMoveBasicBlockBefore(LLVMBasicBlockRef BB, LLVMBasicBlockRef MovePos);
void LLVMMoveBasicBlockAfter(LLVMBasicBlockRef BB, LLVMBasicBlockRef MovePos);

/* Operations on instructions */
LLVMBasicBlockRef LLVMGetInstructionParent(LLVMValueRef Inst);
LLVMValueRef LLVMGetFirstInstruction(LLVMBasicBlockRef BB);
LLVMValueRef LLVMGetLastInstruction(LLVMBasicBlockRef BB);
LLVMValueRef LLVMGetNextInstruction(LLVMValueRef Inst);
LLVMValueRef LLVMGetPreviousInstruction(LLVMValueRef Inst);

/* Operations on call sites */
void LLVMSetInstructionCallConv(LLVMValueRef Instr, unsigned CC);
unsigned LLVMGetInstructionCallConv(LLVMValueRef Instr);
void LLVMAddInstrAttribute(LLVMValueRef Instr, unsigned index, LLVMAttribute);
void LLVMRemoveInstrAttribute(LLVMValueRef Instr, unsigned index, 
                              LLVMAttribute);
void LLVMSetInstrParamAlignment(LLVMValueRef Instr, unsigned index, 
                                unsigned align);

/* Operations on call instructions (only) */
LLVMBool LLVMIsTailCall(LLVMValueRef CallInst);
void LLVMSetTailCall(LLVMValueRef CallInst, LLVMBool IsTailCall);

/* Operations on phi nodes */
void LLVMAddIncoming(LLVMValueRef PhiNode, LLVMValueRef *IncomingValues,
                     LLVMBasicBlockRef *IncomingBlocks, unsigned Count);
unsigned LLVMCountIncoming(LLVMValueRef PhiNode);
LLVMValueRef LLVMGetIncomingValue(LLVMValueRef PhiNode, unsigned Index);
LLVMBasicBlockRef LLVMGetIncomingBlock(LLVMValueRef PhiNode, unsigned Index);

/*===-- Instruction builders ----------------------------------------------===*/

/* An instruction builder represents a point within a basic block, and is the
 * exclusive means of building instructions using the C interface.
 */

|#
(define-llvm LLVMCreateBuilderInContext (_fun LLVMContextRef -> LLVMBuilderRef))
(define-llvm LLVMCreateBuilder (_fun -> LLVMBuilderRef))
#|
void LLVMPositionBuilder(LLVMBuilderRef Builder, LLVMBasicBlockRef Block,
                         LLVMValueRef Instr);
void LLVMPositionBuilderBefore(LLVMBuilderRef Builder, LLVMValueRef Instr);
|#
(define-llvm LLVMPositionBuilderAtEnd (_fun LLVMBuilderRef LLVMBasicBlockRef -> _void))

(define-llvm LLVMGetInsertBlock (_fun LLVMBuilderRef -> LLVMBasicBlockRef))
#|
void LLVMClearInsertionPosition(LLVMBuilderRef Builder);
void LLVMInsertIntoBuilder(LLVMBuilderRef Builder, LLVMValueRef Instr);
void LLVMInsertIntoBuilderWithName(LLVMBuilderRef Builder, LLVMValueRef Instr,
                                   const char *Name);
|#
(define-llvm LLVMDisposeBuilder (_fun LLVMBuilderRef -> _void))
#|

/* Metadata */
void LLVMSetCurrentDebugLocation(LLVMBuilderRef Builder, LLVMValueRef L);
LLVMValueRef LLVMGetCurrentDebugLocation(LLVMBuilderRef Builder);
void LLVMSetInstDebugLocation(LLVMBuilderRef Builder, LLVMValueRef Inst);

/* Terminators */
|#
(define-llvm LLVMBuildRetVoid (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm LLVMBuildRet (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))
#|
LLVMValueRef LLVMBuildAggregateRet(LLVMBuilderRef, LLVMValueRef *RetVals,
                                   unsigned N);
LLVMValueRef LLVMBuildBr(LLVMBuilderRef, LLVMBasicBlockRef Dest);

|#
(define-llvm LLVMBuildCondBr
 (_fun LLVMBuilderRef
       LLVMValueRef
       LLVMBasicBlockRef
       LLVMBasicBlockRef -> LLVMValueRef))
#|
LLVMValueRef LLVMBuildSwitch(LLVMBuilderRef, LLVMValueRef V,
                             LLVMBasicBlockRef Else, unsigned NumCases);
LLVMValueRef LLVMBuildIndirectBr(LLVMBuilderRef B, LLVMValueRef Addr,
                                 unsigned NumDests);
LLVMValueRef LLVMBuildInvoke(LLVMBuilderRef, LLVMValueRef Fn,
                             LLVMValueRef *Args, unsigned NumArgs,
                             LLVMBasicBlockRef Then, LLVMBasicBlockRef Catch,
                             const char *Name);
LLVMValueRef LLVMBuildUnwind(LLVMBuilderRef);
LLVMValueRef LLVMBuildUnreachable(LLVMBuilderRef);

/* Add a case to the switch instruction */
void LLVMAddCase(LLVMValueRef Switch, LLVMValueRef OnVal,
                 LLVMBasicBlockRef Dest);

/* Add a destination to the indirectbr instruction */
void LLVMAddDestination(LLVMValueRef IndirectBr, LLVMBasicBlockRef Dest);

/* Arithmetic */
|#
(define-llvm-multiple
  (LLVMBuildAdd
   LLVMBuildNSWAdd
   LLVMBuildNUWAdd
   LLVMBuildFAdd
   LLVMBuildSub
   LLVMBuildNSWSub
   LLVMBuildNUWSub
   LLVMBuildFSub
   LLVMBuildMul
   LLVMBuildNSWMul
   LLVMBuildNUWMul
   LLVMBuildFMul
   LLVMBuildUDiv
   LLVMBuildSDiv
   LLVMBuildExactSDiv
   LLVMBuildFDiv
   LLVMBuildURem
   LLVMBuildSRem
   LLVMBuildFRem
   LLVMBuildShl
   LLVMBuildLShr
   LLVMBuildAShr
   LLVMBuildAnd
   LLVMBuildOr
   LLVMBuildXor)
  (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
#|
LLVMValueRef LLVMBuildBinOp(LLVMBuilderRef B, LLVMOpcode Op,
                            LLVMValueRef LHS, LLVMValueRef RHS,
                            const char *Name);
LLVMValueRef LLVMBuildNeg(LLVMBuilderRef, LLVMValueRef V, const char *Name);
LLVMValueRef LLVMBuildNSWNeg(LLVMBuilderRef B, LLVMValueRef V,
                             const char *Name);
LLVMValueRef LLVMBuildNUWNeg(LLVMBuilderRef B, LLVMValueRef V,
                             const char *Name);
LLVMValueRef LLVMBuildFNeg(LLVMBuilderRef, LLVMValueRef V, const char *Name);
LLVMValueRef LLVMBuildNot(LLVMBuilderRef, LLVMValueRef V, const char *Name);

/* Memory */
|#
(define-llvm LLVMBuildMalloc (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))
#|
LLVMValueRef LLVMBuildArrayMalloc(LLVMBuilderRef, LLVMTypeRef Ty,
                                  LLVMValueRef Val, const char *Name);
LLVMValueRef LLVMBuildAlloca(LLVMBuilderRef, LLVMTypeRef Ty, const char *Name);
LLVMValueRef LLVMBuildArrayAlloca(LLVMBuilderRef, LLVMTypeRef Ty,
                                  LLVMValueRef Val, const char *Name);
LLVMValueRef LLVMBuildFree(LLVMBuilderRef, LLVMValueRef PointerVal);
|#
(define-llvm LLVMBuildLoad
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildStore
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMBuildGEP
 (_fun (builder ptr indices name) ::
       (builder : LLVMBuilderRef)
       (ptr : LLVMValueRef)
       (indices : (_list i LLVMValueRef))
       (_uint = (length indices))
       (name : _string)
       -> LLVMValueRef))

#|
LLVMValueRef LLVMBuildInBoundsGEP(LLVMBuilderRef B, LLVMValueRef Pointer,
                                  LLVMValueRef *Indices, unsigned NumIndices,
                                  const char *Name);
LLVMValueRef LLVMBuildStructGEP(LLVMBuilderRef B, LLVMValueRef Pointer,
                                unsigned Idx, const char *Name);
|#

(define-llvm-multiple
 (LLVMBuildGlobalString LLVMBuildGlobalStringPtr)
 (_fun LLVMBuilderRef _string _string -> LLVMValueRef))
#|

/* Casts */
LLVMValueRef LLVMBuildTrunc(LLVMBuilderRef, LLVMValueRef Val,
                            LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildZExt(LLVMBuilderRef, LLVMValueRef Val,
                           LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildSExt(LLVMBuilderRef, LLVMValueRef Val,
                           LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildFPToUI(LLVMBuilderRef, LLVMValueRef Val,
                             LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildFPToSI(LLVMBuilderRef, LLVMValueRef Val,
                             LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildUIToFP(LLVMBuilderRef, LLVMValueRef Val,
                             LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildSIToFP(LLVMBuilderRef, LLVMValueRef Val,
                             LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildFPTrunc(LLVMBuilderRef, LLVMValueRef Val,
                              LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildFPExt(LLVMBuilderRef, LLVMValueRef Val,
                            LLVMTypeRef DestTy, const char *Name);
|#
(define-llvm-multiple
 (LLVMBuildPtrToInt LLVMBuildIntToPtr LLVMBuildBitCast)
 (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))
#|
LLVMValueRef LLVMBuildZExtOrBitCast(LLVMBuilderRef, LLVMValueRef Val,
                                    LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildSExtOrBitCast(LLVMBuilderRef, LLVMValueRef Val,
                                    LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildTruncOrBitCast(LLVMBuilderRef, LLVMValueRef Val,
                                     LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildCast(LLVMBuilderRef B, LLVMOpcode Op, LLVMValueRef Val,
                           LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildPointerCast(LLVMBuilderRef, LLVMValueRef Val,
                                  LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildIntCast(LLVMBuilderRef, LLVMValueRef Val, /*Signed cast!*/
                              LLVMTypeRef DestTy, const char *Name);
LLVMValueRef LLVMBuildFPCast(LLVMBuilderRef, LLVMValueRef Val,
                             LLVMTypeRef DestTy, const char *Name);
|#

;/* Comparisons */
(define-llvm LLVMBuildICmp
  (_fun LLVMBuilderRef
        LLVMIntPredicate
        LLVMValueRef
        LLVMValueRef
        _string -> LLVMValueRef))

#|
LLVMValueRef LLVMBuildFCmp(LLVMBuilderRef, LLVMRealPredicate Op,
                           LLVMValueRef LHS, LLVMValueRef RHS,
                           const char *Name);

/* Miscellaneous instructions */
LLVMValueRef LLVMBuildPhi(LLVMBuilderRef, LLVMTypeRef Ty, const char *Name);
|#
(define-llvm LLVMBuildCall
 (_fun (builder fun args name) ::
       (builder : LLVMBuilderRef)
       (fun : LLVMValueRef)
       (args : (_list i LLVMValueRef))
       (_uint = (length args))
       (name : _string)
       -> LLVMValueRef))
#|
LLVMValueRef LLVMBuildSelect(LLVMBuilderRef, LLVMValueRef If,
                             LLVMValueRef Then, LLVMValueRef Else,
                             const char *Name);
LLVMValueRef LLVMBuildVAArg(LLVMBuilderRef, LLVMValueRef List, LLVMTypeRef Ty,
                            const char *Name);
LLVMValueRef LLVMBuildExtractElement(LLVMBuilderRef, LLVMValueRef VecVal,
                                     LLVMValueRef Index, const char *Name);
LLVMValueRef LLVMBuildInsertElement(LLVMBuilderRef, LLVMValueRef VecVal,
                                    LLVMValueRef EltVal, LLVMValueRef Index,
                                    const char *Name);
LLVMValueRef LLVMBuildShuffleVector(LLVMBuilderRef, LLVMValueRef V1,
                                    LLVMValueRef V2, LLVMValueRef Mask,
                                    const char *Name);
LLVMValueRef LLVMBuildExtractValue(LLVMBuilderRef, LLVMValueRef AggVal,
                                   unsigned Index, const char *Name);
LLVMValueRef LLVMBuildInsertValue(LLVMBuilderRef, LLVMValueRef AggVal,
                                  LLVMValueRef EltVal, unsigned Index,
                                  const char *Name);

LLVMValueRef LLVMBuildIsNull(LLVMBuilderRef, LLVMValueRef Val,
                             const char *Name);
LLVMValueRef LLVMBuildIsNotNull(LLVMBuilderRef, LLVMValueRef Val,
                                const char *Name);
LLVMValueRef LLVMBuildPtrDiff(LLVMBuilderRef, LLVMValueRef LHS,
                              LLVMValueRef RHS, const char *Name);


/*===-- Module providers --------------------------------------------------===*/

/* Changes the type of M so it can be passed to FunctionPassManagers and the
 * JIT.  They take ModuleProviders for historical reasons.
 */
LLVMModuleProviderRef
LLVMCreateModuleProviderForExistingModule(LLVMModuleRef M);

/* Destroys the module M.
 */
void LLVMDisposeModuleProvider(LLVMModuleProviderRef M);


/*===-- Memory buffers ----------------------------------------------------===*/

LLVMBool LLVMCreateMemoryBufferWithContentsOfFile(const char *Path,
                                                  LLVMMemoryBufferRef *OutMemBuf,
                                                  char **OutMessage);
LLVMBool LLVMCreateMemoryBufferWithSTDIN(LLVMMemoryBufferRef *OutMemBuf,
                                         char **OutMessage);
void LLVMDisposeMemoryBuffer(LLVMMemoryBufferRef MemBuf);


|#
;/*===-- Pass Managers -----------------------------------------------------===*/

;/** Constructs a new whole-module pass pipeline. This type of pipeline is
;    suitable for link-time optimization and whole-module transformations.
;    See llvm::PassManager::PassManager. */
(define-llvm LLVMCreatePassManager (_fun -> LLVMPassManagerRef))

;/** Constructs a new function-by-function pass pipeline over the module
;    provider. It does not take ownership of the module provider. This type of
;    pipeline is suitable for code generation and JIT compilation tasks.
;    See llvm::FunctionPassManager::FunctionPassManager. */
(define-llvm LLVMCreateFunctionPassManagerForModule (_fun LLVMModuleRef -> LLVMPassManagerRef))


;/** Initializes, executes on the provided module, and finalizes all of the
;    passes scheduled in the pass manager. Returns 1 if any of the passes
;    modified the module, 0 otherwise. See llvm::PassManager::run(Module&). */
(define-llvm LLVMRunPassManager (_fun LLVMPassManagerRef LLVMModuleRef -> LLVMBool))

#|
/** Initializes all of the function passes scheduled in the function pass
    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
    See llvm::FunctionPassManager::doInitialization. */
LLVMBool LLVMInitializeFunctionPassManager(LLVMPassManagerRef FPM);

/** Executes all of the function passes scheduled in the function pass manager
    on the provided function. Returns 1 if any of the passes modified the
    function, false otherwise.
    See llvm::FunctionPassManager::run(Function&). */
LLVMBool LLVMRunFunctionPassManager(LLVMPassManagerRef FPM, LLVMValueRef F);

/** Finalizes all of the function passes scheduled in in the function pass
    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
    See llvm::FunctionPassManager::doFinalization. */
LLVMBool LLVMFinalizeFunctionPassManager(LLVMPassManagerRef FPM);

|#

;/** Frees the memory of a pass pipeline. For function pipelines, does not free
;    the module provider.
;    See llvm::PassManagerBase::~PassManagerBase. */
(define-llvm LLVMDisposePassManager (_fun LLVMPassManagerRef -> _void))


;Analysis
(define LLVMVerifierFailureAction (_enum '(
  LLVMAbortProcessAction    ;/* verifier will print to stderr and abort() */
  LLVMPrintMessageAction    ;/* verifier will print to stderr and return 1 */
  LLVMReturnStatusAction))) ;/* verifier will just return 1 */


;/* Verifies that a module is valid, taking the specified action if not.
;   Optionally returns a human-readable description of any invalid constructs.
;   OutMessage must be disposed with LLVMDisposeMessage. */

(define-llvm LLVMVerifyModule
   (_fun (module action) ::
          (module : LLVMModuleRef)
          (action : LLVMVerifierFailureAction)
          (message : (_ptr io LLVMMessage) = #f)
          ->
          (ans : LLVMBool)
          ->
          (and ans message)))

;/* Verifies that a single function is valid, taking the specified action. Useful
;   for debugging. */
(define-llvm LLVMVerifyFunction (_fun LLVMValueRef LLVMVerifierFailureAction -> LLVMBool))

;/* Open up a ghostview window that displays the CFG of the current function.
;   Useful for debugging. */

(define-llvm LLVMViewFunctionCFG (_fun LLVMValueRef -> _void))
(define-llvm LLVMViewFunctionCFGOnly (_fun LLVMValueRef -> _void))

;/*===-- Operations on modules ---------------------------------------------===*/

;/** Writes a module to the specified path. Returns 0 on success. */ 
(define-llvm LLVMWriteBitcodeToFile (_fun LLVMModuleRef _string -> _int))

;/** Writes a module to an open file descriptor. Returns 0 on success. */
(define-llvm LLVMWriteBitcodeToFD (_fun LLVMModuleRef _int _bool _bool -> _int))

;/* Builds a module from the bitcode in the specified memory buffer, returning a
;   reference to the module via the OutModule parameter. Returns 0 on success.
;   Optionally returns a human-readable error message via OutMessage. */ 
(define-llvm LLVMParseBitcode
  (_fun (buffer) ::
        (buffer : LLVMMemoryBufferRef)
        (module : (_ptr o LLVMModuleRef))
        (message : (_ptr io LLVMMessage) = #f)
        ->
        (err : LLVMBool)
        ->
        (if err message module)))
            
(define-llvm LLVMParseBitcodeInContext
  (_fun (context buffer) ::
        (context : LLVMContextRef)
        (buffer : LLVMMemoryBufferRef)
        (module : (_ptr o LLVMModuleRef))
        (message : (_ptr io LLVMMessage) = #f)
        ->
        (err : LLVMBool)
        ->
        (if err message module)))

(define-llvm LLVMGetBitcodeModule
  (_fun (buffer) ::
        (buffer : LLVMMemoryBufferRef)
        (module : (_ptr o LLVMModuleRef))
        (message : (_ptr io LLVMMessage) = #f)
        ->
        (err : LLVMBool)
        ->
        (if err message module)))
            
(define-llvm LLVMGetBitcodeModuleInContext
  (_fun (context buffer) ::
        (context : LLVMContextRef)
        (buffer : LLVMMemoryBufferRef)
        (module : (_ptr o LLVMModuleRef))
        (message : (_ptr io LLVMMessage) = #f)
        ->
        (err : LLVMBool)
        ->
        (if err message module)))


;Execution Engine

(define-llvm-multiple (LLVMLinkInJIT LLVMLinkInInterpreter) (_fun -> _void))

(define LLVMGenericValueRef _pointer)
(define LLVMExecutionEngineRef _pointer)

;/*===-- Operations on generic values --------------------------------------===*/

(define-llvm LLVMCreateGenericValueOfInt
 (_fun LLVMTypeRef _ulong LLVMBool -> LLVMGenericValueRef))

(define-llvm LLVMCreateGenericValueOfPointer
 (_fun _pointer -> LLVMGenericValueRef))

(define-llvm LLVMCreateGenericValueOfFloat
 (_fun LLVMTypeRef _double* -> LLVMGenericValueRef))

(define-llvm LLVMGenericValueIntWidth
 (_fun LLVMGenericValueRef -> _uint))

(define-llvm LLVMGenericValueToInt
 (_fun LLVMGenericValueRef LLVMBool -> _ulong))

(define-llvm LLVMGenericValueToPointer
 (_fun LLVMGenericValueRef -> _pointer))

(define-llvm LLVMGenericValueToFloat
 (_fun LLVMTypeRef LLVMGenericValueRef -> _double*))

(define-llvm LLVMDisposeGenericValue
 (_fun LLVMGenericValueRef -> _void))


;/*===-- Operations on execution engines -----------------------------------===*/

(define-llvm LLVMCreateExecutionEngineForModule
 (_fun (module) ::
       (execution-engine : (_ptr o LLVMExecutionEngineRef))
       (module : LLVMModuleRef)
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message execution-engine)))

(define-llvm LLVMCreateInterpreterForModule
 (_fun (module) ::
       (execution-engine : (_ptr o LLVMExecutionEngineRef))
       (module : LLVMModuleRef)
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message execution-engine)))

(define-llvm LLVMCreateJITCompilerForModule
 (_fun (module opt) ::
       (execution-engine : (_ptr o LLVMExecutionEngineRef))
       (module : LLVMModuleRef)
       (opt : _uint)
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message execution-engine)))

(define-llvm-multiple 
 (LLVMDisposeExecutionEngine
  LLVMRunStaticConstructors
  LLVMRunStaticDestructors)
 (_fun LLVMExecutionEngineRef -> _void))

#|
int LLVMRunFunctionAsMain(LLVMExecutionEngineRef EE, LLVMValueRef F,
                          unsigned ArgC, const char * const *ArgV,
                          const char * const *EnvP);
|#

(define-llvm LLVMRunFunction
 (_fun (engine function args) ::
       (engine : LLVMExecutionEngineRef)
       (function : LLVMValueRef)
       (_uint = (length args))
       (args : (_list i LLVMGenericValueRef))
       ->
       LLVMGenericValueRef))

#|
void LLVMFreeMachineCodeForFunction(LLVMExecutionEngineRef EE, LLVMValueRef F);

void LLVMAddModule(LLVMExecutionEngineRef EE, LLVMModuleRef M);


LLVMBool LLVMRemoveModule(LLVMExecutionEngineRef EE, LLVMModuleRef M,
                          LLVMModuleRef *OutMod, char **OutError);


LLVMBool LLVMFindFunction(LLVMExecutionEngineRef EE, const char *Name,
                          LLVMValueRef *OutFn);

void *LLVMRecompileAndRelinkFunction(LLVMExecutionEngineRef EE, LLVMValueRef Fn);

LLVMTargetDataRef LLVMGetExecutionEngineTargetData(LLVMExecutionEngineRef EE);

void LLVMAddGlobalMapping(LLVMExecutionEngineRef EE, LLVMValueRef Global,
                          void* Addr);

void *LLVMGetPointerToGlobal(LLVMExecutionEngineRef EE, LLVMValueRef Global);
|#
(define-llvm LLVMInitializeX86TargetInfo (_fun -> _void))
(define-llvm LLVMInitializeX86Target (_fun -> _void))

(LLVMInitializeX86TargetInfo)
(LLVMInitializeX86Target)

