#lang racket/base


(require ffi/unsafe)
(provide
 LLVMAttribute
 LLVMOpcode
 LLVMTypeKind
 LLVMLinkage
 LLVMVisibility
 LLVMCallConv
 LLVMIntPredicate
 LLVMIntPredicate?
 LLVMRealPredicate
 LLVMRealPredicate?
 LLVMLandingPadClauseTy)

;;TODO write define-enum that makes a predicate aswell

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


(define LLVMOpcode (_enum '(
  ;/* Terminator Instructions */
  LLVMRet            = 1
  LLVMBr             = 2
  LLVMSwitch         = 3
  LLVMIndirectBr     = 4
  LLVMInvoke         = 5
  ;LLVMUnwind         = 6
  ;Removed in version 3.0
  LLVMUnreachable    = 7

  ;/* Standard Binary Operators */
  LLVMAdd            = 8
  LLVMFAdd           = 9
  LLVMSub            = 10
  LLVMFSub           = 11
  LLVMMul            = 12
  LLVMFMul           = 13
  LLVMUDiv           = 14
  LLVMSDiv           = 15
  LLVMFDiv           = 16
  LLVMURem           = 17
  LLVMSRem           = 18
  LLVMFRem           = 19

  ;/* Logical Operators */
  LLVMShl            = 20
  LLVMLShr           = 21
  LLVMAShr           = 22
  LLVMAnd            = 23
  LLVMOr             = 24
  LLVMXor            = 25

  ;/* Memory Operators */
  LLVMAlloca         = 26
  LLVMLoad           = 27
  LLVMStore          = 28
  LLVMGetElementPtr  = 29

  ;/* Cast Operators */
  LLVMTrunc          = 30
  LLVMZExt           = 31
  LLVMSExt           = 32
  LLVMFPToUI         = 33
  LLVMFPToSI         = 34
  LLVMUIToFP         = 35
  LLVMSIToFP         = 36
  LLVMFPTrunc        = 37
  LLVMFPExt          = 38
  LLVMPtrToInt       = 39
  LLVMIntToPtr       = 40
  LLVMBitCast        = 41

  ;/* Other Operators */
  LLVMICmp           = 42
  LLVMFCmp           = 43
  LLVMPHI            = 44
  LLVMCall           = 45
  LLVMSelect         = 46
  ;/* UserOp1 */
  ;/* UserOp2 */
  LLVMVAArg          = 49
  LLVMExtractElement = 50
  LLVMInsertElement  = 51
  LLVMShuffleVector  = 52
  LLVMExtractValue   = 53
  LLVMInsertValue    = 54

  ;/* Atomic operators */
  LLVMFence          = 55
  LLVMAtomicCmpXchg  = 56
  LLVMAtomicRMW      = 57

  ;/* Exception Handling Operators */
  LLVMResume         = 58
  LLVMLandingPad     = 59)))

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
  LLVMVectorTypeKind      ;/**< SIMD 'packed' format, or other vector type */
  LLVMMetadataTypeKind    ;/**< Metadata */
  LLVMX86_MMXTypeKind)))  ;/**< X86 MMX */


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

(define LLVMCallConv (_enum '(
  LLVMCCallConv           = 0
  LLVMFastCallConv        = 8
  LLVMColdCallConv        = 9
  LLVMX86StdcallCallConv  = 64
  LLVMX86FastcallCallConv = 65)))

;; TODO make this not such a hack
(define LLVMIntPredicate? symbol?)
(define LLVMIntPredicate (_enum '(
  LLVMIntEQ = 32  ;/**< equal */
  LLVMIntNE        ;/**< not equal */
  LLVMIntUGT       ;/**< unsigned greater than */
  LLVMIntUGE       ;/**< unsigned greater or equal */
  LLVMIntULT       ;/**< unsigned less than */
  LLVMIntULE       ;/**< unsigned less or equal */
  LLVMIntSGT       ;/**< signed greater than */
  LLVMIntSGE       ;/**< signed greater or equal */
  LLVMIntSLT       ;/**< signed less than */
  LLVMIntSLE)))     ;/**< signed less or equal */

;; TODO make this not such a hack
(define LLVMRealPredicate? symbol?)
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

(define LLVMLandingPadClauseTy (_enum '(
  LLVMLandingPadCatch      ;/**< A catch clause   */
  LLVMLandingPadFilter)))  ;/**< A filter clause  */

