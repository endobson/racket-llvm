#lang racket
(require (for-syntax syntax/parse))
(require ffi/unsafe)

(require
  "llvm-enums.rkt"
  "llvm-types.rkt"
  "llvm-lib.rkt"
  "llvm-ctypes.rkt"
  "llvm-define.rkt")
(provide (all-from-out "llvm-enums.rkt"
                       "llvm-types.rkt"))

(provide (all-defined-out))

(provide
  llvm-value-ref?
  llvm-type-ref?
  llvm-module-ref?
  llvm-context-ref?
  llvm-basic-block-ref?
  llvm-builder-ref?)











;Racket added functions
;

(define-llvm-racket LLVMGetModuleDescription unsafe:LLVMModuleDescriptionMaker)
(define-llvm-racket LLVMIsValidTypeIndex (_fun LLVMTypeRef LLVMValueRef -> LLVMBool))
(define-llvm-racket LLVMGetTypeAtIndex (_fun LLVMTypeRef LLVMValueRef -> LLVMTypeRef))
(define-llvm-racket LLVMIsTerminatorInstruction (_fun LLVMValueRef -> LLVMBool))


(define-llvm-racket-safe LLVMGetModuleDescription safe:LLVMModuleDescriptionMaker)
(define-llvm-racket-safe LLVMIsValidTypeIndex (_fun safe:LLVMTypeRef safe:LLVMValueRef -> LLVMBool))
;(define-llvm-racket-safe LLVMGetTypeAtIndex (_fun LLVMTypeRef LLVMValueRef -> LLVMTypeRef))
(define-llvm-racket-safe LLVMIsTerminatorInstruction (_fun safe:LLVMValueRef -> LLVMBool))



(define-llvm LLVMInitializeCore (_fun LLVMPassRegistryRef -> _void))



;/*===-- Contexts ----------------------------------------------------------===*/


;/* Create and destroy contexts. */
(define-llvm LLVMContextCreate (_fun -> LLVMContextRef))
(define-llvm-safe LLVMContextCreate safe:LLVMContextCreator)


(define-llvm LLVMGetGlobalContext (_fun -> LLVMContextRef))
(define-llvm LLVMContextDispose (_fun LLVMContextRef -> _void))

(define-llvm LLVMGetMDKindIDInContext (_fun LLVMContextRef _string _uint -> _uint))

(define-llvm LLVMGetMDKindID (_fun _string _uint -> _uint))



;/*===-- Modules -----------------------------------------------------------===*/

;/* Create and destroy modules. */ 
;/** See llvm::Module::Module. */
(define-llvm LLVMModuleCreateWithName (_fun _string -> LLVMModuleRef))
(define-llvm LLVMModuleCreateWithNameInContext (_fun _string LLVMContextRef -> LLVMModuleRef))

(define-llvm-safe LLVMModuleCreateWithNameInContext safe:LLVMModuleCreator)


;/** See llvm::Module::~Module. */
(define-llvm LLVMDisposeModule (_fun LLVMModuleRef -> _void))


;/** Data layout. See Module::getDataLayout. */
(define-llvm LLVMGetDataLayout (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetDataLayout (_fun LLVMModuleRef _string -> _void))

;/** Target triple. See Module::getTargetTriple. */
(define-llvm LLVMGetTarget (_fun LLVMModuleRef -> _string))
(define-llvm LLVMSetTarget (_fun LLVMModuleRef _string -> _void))


;/** See Module::dump. */
(define-llvm LLVMDumpModule (_fun LLVMModuleRef -> _void))

;/** See Module::setModuleInlineAsm. */
(define-llvm LLVMSetModuleInlineAsm (_fun LLVMModuleRef _string -> _void))

;/** See Module::getContext. */
(define-llvm LLVMGetModuleContext (_fun LLVMModuleRef -> LLVMContextRef))





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
      macro(LandingPadInst)                 \
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
        macro(ResumeInst)                   \
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
(define-llvm LLVMSetOperand (_fun LLVMValueRef _uint LLVMValueRef -> _void))
(define-llvm LLVMGetNumOperands (_fun LLVMValueRef -> _int))

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
(define-llvm LLVMConstInt (_fun LLVMTypeRef _long LLVMBool -> LLVMValueRef))
(define-llvm LLVMConstIntOfArbitraryPrecision
 (_fun (type words) ::
       (type : LLVMTypeRef)
       (_uint = (length words))
       (words : (_list i _uint64))
       -> LLVMTypeRef))


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

(define-llvm LLVMBlockAddress (_fun LLVMValueRef LLVMBasicBlockRef -> LLVMValueRef))


;/* Operations on global variables, functions, and aliases (globals) */
(define-llvm LLVMGetGlobalParent (_fun LLVMValueRef -> LLVMModuleRef))

(define-llvm LLVMIsDeclaration (_fun LLVMValueRef -> LLVMBool))

(define-llvm LLVMGetLinkage (_fun LLVMValueRef -> LLVMLinkage))
(define-llvm LLVMSetLinkage (_fun LLVMValueRef LLVMLinkage -> _void))

(define-llvm LLVMGetSection (_fun LLVMValueRef -> _string))
(define-llvm LLVMSetSection (_fun LLVMValueRef _string -> _void))

(define-llvm LLVMGetVisibility (_fun LLVMValueRef -> LLVMVisibility))
(define-llvm LLVMSetVisibility (_fun LLVMValueRef LLVMVisibility -> _void))

(define-llvm LLVMGetAlignment (_fun LLVMValueRef -> _uint))
(define-llvm LLVMSetAlignment (_fun LLVMValueRef _uint -> _void))

;/* Operations on global variables */
(define-llvm LLVMAddGlobal
 (_fun LLVMModuleRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm LLVMAddGlobalInAddressSpace
 (_fun LLVMModuleRef LLVMTypeRef _string _uint -> LLVMValueRef))

(define-llvm LLVMGetNamedGlobal (_fun LLVMModuleRef _string -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetFirstGlobal LLVMGetLastGlobal)
 (_fun LLVMModuleRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetNextGlobal
  LLVMGetPreviousGlobal
  LLVMDeleteGlobal
  LLVMGetInitializer)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMSetInitializer (_fun LLVMValueRef LLVMValueRef -> _void))

(define-llvm-multiple
 (LLVMIsThreadLocal LLVMIsGlobalConstant)
 (_fun LLVMValueRef -> LLVMBool))
(define-llvm-multiple
 (LLVMSetThreadLocal LLVMSetGlobalConstant)
 (_fun LLVMValueRef LLVMBool -> _void))



;/* Operations on aliases */

(define-llvm LLVMAddAlias
 (_fun LLVMModuleRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))

;/* Operations on functions */
(define-llvm LLVMAddFunction (_fun LLVMModuleRef _string LLVMTypeRef -> LLVMValueRef))

(define-llvm LLVMGetNamedFunction (_fun LLVMModuleRef _string -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetFirstFunction
  LLVMGetLastFunction)
 (_fun LLVMModuleRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetNextFunction
  LLVMGetPreviousFunction)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMDeleteFunction
 (_fun LLVMValueRef -> _void))

(define-llvm LLVMGetIntrinsicID
 (_fun LLVMValueRef -> _uint))

(define-llvm LLVMGetFunctionCallConv
 (_fun LLVMValueRef -> LLVMCallConv))

(define-llvm LLVMSetFunctionCallConv
 (_fun LLVMValueRef LLVMCallConv -> _void))

(define-llvm LLVMGetGC
 (_fun LLVMValueRef -> _string))
(define-llvm LLVMSetGC (_fun LLVMValueRef _string -> _void))


(define-llvm-multiple
 (LLVMAddFunctionAttr
  LLVMRemoveFunctionAttr)
 (_fun LLVMValueRef LLVMAttribute -> _void))

(define-llvm LLVMGetFunctionAttr
 (_fun LLVMValueRef -> LLVMAttribute))
 

;/* Operations on parameters */

(define-llvm LLVMCountParams (_fun LLVMValueRef -> _uint))
(define-llvm LLVMGetParams
 (_fun (fun) ::
       (fun : LLVMValueRef)
       (params : (_list o LLVMValueRef (LLVMCountParams fun)))
       -> _void
       -> params))

(define-llvm LLVMGetParam (_fun LLVMValueRef _uint -> LLVMValueRef))

(define-llvm LLVMGetParamParent (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetFirstParam
  LLVMGetLastParam)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMGetNextParam
  LLVMGetPreviousParam)
 (_fun LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMAddAttribute
  LLVMRemoveAttribute)
 (_fun LLVMValueRef LLVMAttribute -> _void))

(define-llvm LLVMGetAttribute (_fun LLVMValueRef -> LLVMAttribute))
(define-llvm LLVMSetParamAlignment (_fun LLVMValueRef _uint -> _void))


;/* Operations on basic blocks */

(define-llvm LLVMBasicBlockAsValue (_fun LLVMBasicBlockRef -> LLVMValueRef))
(define-llvm LLVMValueIsBasicBlock (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMValueAsBasicBlock (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm LLVMGetBasicBlockParent (_fun LLVMBasicBlockRef -> LLVMValueRef))
;(define-llvm LLVMGetBasicBlockTerminator (_fun LLVMBasicBlockRef -> LLVMValueRef))
;Not yet in my dev repo of llvm

(define-llvm LLVMCountBasicBlocks (_fun LLVMValueRef -> _uint))
(define-llvm LLVMGetBasicBlocks
 (_fun (fun) ::
       (fun : LLVMValueRef)
       (blocks : (_list o LLVMBasicBlockRef (LLVMCountBasicBlocks fun)))
       -> _void
       -> blocks))

(define-llvm-multiple
 (LLVMGetEntryBasicBlock
  LLVMGetFirstBasicBlock
  LLVMGetLastBasicBlock)
 (_fun LLVMValueRef -> LLVMBasicBlockRef))

(define-llvm-multiple
 (LLVMGetNextBasicBlock
  LLVMGetPreviousBasicBlock)
 (_fun LLVMBasicBlockRef -> LLVMBasicBlockRef))

(define-llvm LLVMAppendBasicBlockInContext (_fun LLVMContextRef LLVMValueRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMInsertBasicBlockInContext (_fun LLVMContextRef LLVMBasicBlockRef _string -> LLVMBasicBlockRef))


(define-llvm LLVMAppendBasicBlock
 (_fun LLVMValueRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMInsertBasicBlock
 (_fun LLVMBasicBlockRef _string -> LLVMBasicBlockRef))
(define-llvm LLVMDeleteBasicBlock
 (_fun LLVMBasicBlockRef -> _void))
;(define-llvm LLVMRemoveBasicBlockFromParent
; (_fun LLVMBasicBlockRef -> _void))

(define-llvm-multiple
 (LLVMMoveBasicBlockBefore
  LLVMMoveBasicBlockAfter)
 (_fun LLVMBasicBlockRef LLVMBasicBlockRef -> _void))


(define-llvm-multiple
 (LLVMGetFirstInstruction
  LLVMGetLastInstruction)
 (_fun LLVMBasicBlockRef -> LLVMValueRef))

;/* Operations on instructions */
(define-llvm LLVMGetInstructionParent (_fun LLVMValueRef -> LLVMBasicBlockRef))
(define-llvm-multiple
 (LLVMGetNextInstruction
  LLVMGetPreviousInstruction)
 (_fun LLVMValueRef -> LLVMValueRef))

;/* Operations on call sites */

(define-llvm LLVMGetInstructionCallConv (_fun LLVMValueRef -> LLVMCallConv))
(define-llvm LLVMSetInstructionCallConv (_fun LLVMValueRef LLVMCallConv -> _void))

(define-llvm LLVMAddInstrAttribute (_fun LLVMValueRef _uint LLVMAttribute -> _void))
(define-llvm LLVMRemoveInstrAttribute (_fun LLVMValueRef _uint LLVMAttribute -> _void))

(define-llvm LLVMSetInstrParamAlignment (_fun LLVMValueRef _uint _uint -> _void))


;/* Operations on call instructions (only) */

(define-llvm LLVMIsTailCall (_fun LLVMValueRef -> LLVMBool))
(define-llvm LLVMSetTailCall (_fun LLVMValueRef LLVMBool -> _void))

;/* Operations on switch instructions (only) */
;(define-llvm LLVMGetSwitchDefaultDest (_fun LLVMValueRef -> LLVMBasicBlockRef))

;/* Operations on phi nodes */
(define-llvm LLVMAddIncoming
 (_fun (phi values blocks) ::
       (phi : LLVMValueRef)
       (values : (_list i LLVMValueRef))
       (blocks : (_list i LLVMBasicBlockRef))
       (list : _uint = (min (length values) (length blocks)))
       -> _void))

   

(define-llvm LLVMCountIncoming (_fun LLVMValueRef -> _uint))
(define-llvm LLVMGetIncomingValue (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMGetIncomingBlock (_fun LLVMValueRef _uint -> LLVMBasicBlockRef))

;/*===-- Instruction builders ----------------------------------------------===*/

;/* An instruction builder represents a point within a basic block, and is the
; * exclusive means of building instructions using the C interface.
; */

(define-llvm LLVMCreateBuilderInContext (_fun LLVMContextRef -> LLVMBuilderRef))
(define-llvm LLVMCreateBuilder (_fun -> LLVMBuilderRef))

(define-llvm LLVMPositionBuilder (_fun LLVMBuilderRef LLVMBasicBlockRef LLVMValueRef -> _void))
(define-llvm LLVMPositionBuilderBefore
  (_fun LLVMBuilderRef LLVMValueRef -> _void))

(define-llvm LLVMPositionBuilderAtEnd (_fun LLVMBuilderRef LLVMBasicBlockRef -> _void))

(define-llvm LLVMGetInsertBlock (_fun LLVMBuilderRef -> LLVMBasicBlockRef))

(define-llvm LLVMClearInsertionPosition
 (_fun LLVMBuilderRef -> _void))

(define-llvm LLVMInsertIntoBuilder
 (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm LLVMInsertIntoBuilderWithName
 (_fun LLVMBuilderRef LLVMValueRef _string -> _void))

(define-llvm LLVMDisposeBuilder (_fun LLVMBuilderRef -> _void))




;/* Metadata */
(define-llvm LLVMGetCurrentDebugLocation (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm LLVMSetCurrentDebugLocation (_fun LLVMBuilderRef LLVMValueRef -> _void))
(define-llvm LLVMSetInstDebugLocation (_fun LLVMBuilderRef LLVMValueRef -> _void))


;/* Terminators */
(define-llvm LLVMBuildRetVoid (_fun LLVMBuilderRef -> LLVMValueRef))
(define-llvm LLVMBuildRet (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))
(define-llvm LLVMBuildAggregateRet
  (_fun (builder vals) ::
        (builder : LLVMBuilderRef)
        (vals : (_list i LLVMValueRef))
        (_uint = (length vals))
        -> LLVMValueRef))


(define-llvm LLVMBuildBr (_fun LLVMBuilderRef LLVMBasicBlockRef -> LLVMValueRef))


(define-llvm LLVMBuildCondBr
 (_fun LLVMBuilderRef
       LLVMValueRef
       LLVMBasicBlockRef
       LLVMBasicBlockRef -> LLVMValueRef))

(define-llvm LLVMBuildSwitch
 (_fun LLVMBuilderRef LLVMValueRef LLVMBasicBlockRef _uint -> LLVMValueRef))

(define-llvm LLVMAddCase (_fun LLVMValueRef LLVMValueRef LLVMBasicBlockRef -> _void))

(define-llvm LLVMBuildIndirectBr (_fun LLVMBuilderRef LLVMValueRef _uint -> LLVMValueRef))
(define-llvm LLVMBuildInvoke
 (_fun (builder fun args then catch name) ::
       (builder : LLVMBuilderRef)
       (fun : LLVMValueRef)
       (args : (_list i LLVMValueRef))
       (_uint = (length args))
       (then : LLVMBasicBlockRef)
       (catch : LLVMBasicBlockRef)
       (name : _string)
       ->
       LLVMValueRef))

(define-llvm LLVMBuildLandingPad
 (_fun LLVMBuilderRef
       LLVMTypeRef
       LLVMValueRef
       _uint
       _string
       -> _void))
(define-llvm LLVMBuildResume
 (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMBuildUnreachable
 (_fun LLVMBuilderRef -> LLVMValueRef))

;/* Add a destination to the indirectbr instruction */
(define-llvm LLVMAddDestination (_fun LLVMValueRef LLVMBasicBlockRef -> _void))

(define-llvm LLVMAddClause (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm LLVMSetCleanup (_fun LLVMValueRef LLVMBool -> _void))

;/* Arithmetic */
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

(define-llvm LLVMBuildBinOp
 (_fun LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm-multiple
 (LLVMBuildNeg
  LLVMBuildNSWNeg
  LLVMBuildNUWNeg
  LLVMBuildFNeg
  LLVMBuildNot)
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))

;/* Memory */
(define-llvm-multiple
 (LLVMBuildMalloc
  LLVMBuildAlloca)
 (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))
(define-llvm-multiple
 (LLVMBuildArrayMalloc
  LLVMBuildArrayAlloca)
 (_fun LLVMBuilderRef LLVMTypeRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm LLVMBuildFree (_fun LLVMBuilderRef LLVMValueRef -> LLVMValueRef))

(define-llvm LLVMBuildLoad
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildStore
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef -> LLVMValueRef))

(define-llvm-multiple
 (LLVMBuildGEP
  LLVMBuildInBoundsGEP)
 (_fun (builder ptr indices name) ::
       (builder : LLVMBuilderRef)
       (ptr : LLVMValueRef)
       (indices : (_list i LLVMValueRef))
       (_uint = (length indices))
       (name : _string)
       -> LLVMValueRef))


(define-llvm LLVMBuildStructGEP
 (_fun LLVMBuilderRef LLVMValueRef _uint _string -> LLVMValueRef))

(define-llvm-multiple
 (LLVMBuildGlobalString LLVMBuildGlobalStringPtr)
 (_fun LLVMBuilderRef _string _string -> LLVMValueRef))


(define-llvm-multiple
 (LLVMBuildTrunc
  LLVMBuildZExt
  LLVMBuildSExt
  LLVMBuildFPToUI
  LLVMBuildFPToSI
  LLVMBuildUIToFP
  LLVMBuildSIToFP
  LLVMBuildFPTrunc
  LLVMBuildFPExt)
 (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm-multiple
 (LLVMBuildPtrToInt
  LLVMBuildIntToPtr
  LLVMBuildBitCast
  LLVMBuildZExtOrBitCast
  LLVMBuildSExtOrBitCast
  LLVMBuildTruncOrBitCast
  LLVMBuildPointerCast
  LLVMBuildIntCast
  LLVMBuildFPCast)
 (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm LLVMBuildCast
 (_fun LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))

;/* Comparisons */
(define-llvm LLVMBuildICmp
  (_fun LLVMBuilderRef
        LLVMIntPredicate
        LLVMValueRef
        LLVMValueRef
        _string -> LLVMValueRef))

(define-llvm LLVMBuildFCmp
  (_fun LLVMBuilderRef
        LLVMRealPredicate
        LLVMValueRef
        LLVMValueRef
        _string -> LLVMValueRef))


;/* Miscellaneous instructions */

(define-llvm LLVMBuildPhi (_fun LLVMBuilderRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm LLVMBuildCall
 (_fun (builder fun args name) ::
       (builder : LLVMBuilderRef)
       (fun : LLVMValueRef)
       (args : (_list i LLVMValueRef))
       (_uint = (length args))
       (name : _string)
       -> LLVMValueRef))

(define-llvm LLVMBuildSelect
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm LLVMBuildVAArg
 (_fun LLVMBuilderRef LLVMValueRef LLVMTypeRef _string -> LLVMValueRef))

(define-llvm LLVMBuildExtractElement
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))
(define-llvm LLVMBuildInsertElement
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm LLVMBuildShuffleVector
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))


(define-llvm LLVMBuildExtractValue
 (_fun LLVMBuilderRef LLVMValueRef _uint  _string -> LLVMValueRef))
(define-llvm LLVMBuildInsertValue
 (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _uint  _string -> LLVMValueRef))
(define-llvm-multiple
 (LLVMBuildIsNull
  LLVMBuildIsNotNull)
 (_fun LLVMBuilderRef LLVMValueRef _string -> LLVMValueRef))

(define-llvm LLVMBuildPtrDiff (_fun LLVMBuilderRef LLVMValueRef LLVMValueRef _string -> LLVMValueRef))


;/*===-- Module providers --------------------------------------------------===*/
; DEPRECATED SO NOT ADDING, MAY CHANGE MIND

;/* Changes the type of M so it can be passed to FunctionPassManagers and the
; * JIT.  They take ModuleProviders for historical reasons.
; */
;
;LLVMModuleProviderRef LLVMCreateModuleProviderForExistingModule(LLVMModuleRef M);

;/* Destroys the module M.
; */
;void LLVMDisposeModuleProvider(LLVMModuleProviderRef M);


;/*===-- Memory buffers ----------------------------------------------------===*/

(define-llvm LLVMCreateMemoryBufferWithContentsOfFile
   (_fun (path) ::
          (path : _string)
          (buffer : (_ptr o LLVMMemoryBufferRef))
          (message : (_ptr io LLVMMessage) = #f)
          ->
          (ans : LLVMBool)
          ->
          (if ans message buffer)))


(define-llvm LLVMCreateMemoryBufferWithSTDIN
   (_fun () ::
          (buffer : (_ptr o LLVMMemoryBufferRef))
          (message : (_ptr io LLVMMessage) = #f)
          ->
          (ans : LLVMBool)
          ->
          (if ans message buffer)))

(define-llvm LLVMDisposeMemoryBuffer
 (_fun LLVMMemoryBufferRef -> _void))



;/*===-- Pass Registry -----------------------------------------------------===*/

;/** Return the global pass registry, for use with initialization functions.
;    See llvm::PassRegistry::getPassRegistry. */
(define-llvm LLVMGetGlobalPassRegistry
 (_fun -> LLVMPassRegistryRef))


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

;/** Initializes all of the function passes scheduled in the function pass
;    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
;    See llvm::FunctionPassManager::doInitialization. */
(define-llvm LLVMInitializeFunctionPassManager (_fun LLVMPassManagerRef -> LLVMBool))

;/** Executes all of the function passes scheduled in the function pass manager
;    on the provided function. Returns 1 if any of the passes modified the
;    function, false otherwise.
;    See llvm::FunctionPassManager::run(Function&). */
(define-llvm LLVMRunFunctionPassManager (_fun LLVMPassManagerRef LLVMValueRef -> LLVMBool))

;/** Finalizes all of the function passes scheduled in in the function pass
;    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
;    See llvm::FunctionPassManager::doFinalization. */
(define-llvm LLVMFinalizeFunctionPassManager (_fun LLVMPassManagerRef -> LLVMBool))

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

(define (LLVMCreateGenericValueOfFunctionType fun-type)
 (get-ffi-obj 'LLVMCreateGenericValueOfPointer llvm-lib 
  (_fun fun-type -> LLVMGenericValueRef)))

(define-llvm LLVMCreateGenericValueOfFloat
 (_fun LLVMTypeRef _double* -> LLVMGenericValueRef))

(define-llvm LLVMGenericValueIntWidth
 (_fun LLVMGenericValueRef -> _uint))

(define-llvm LLVMGenericValueToInt
 (_fun LLVMGenericValueRef LLVMBool -> _long))

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


;TODO support env
(define-llvm LLVMRunFunctionAsMain
 (_fun (ee fun args) ::
       (ee : LLVMExecutionEngineRef)
       (fun : LLVMValueRef)
       (_uint = (length args))
       (args : (_list i _string))
       (env : (_list i _string) = (list #f))
       ->
       _sint))


(define-llvm LLVMRunFunction
 (_fun (engine function args) ::
       (engine : LLVMExecutionEngineRef)
       (function : LLVMValueRef)
       (_uint = (length args))
       (args : (_list i LLVMGenericValueRef))
       ->
       LLVMGenericValueRef))


(define-llvm LLVMAddModule (_fun LLVMExecutionEngineRef LLVMModuleRef -> _void))

(define-llvm LLVMFreeMachineCodeForFunction
 (_fun LLVMExecutionEngineRef LLVMValueRef -> _void))

(define-llvm LLVMRemoveModule
 (_fun (ee module) ::
       (ee : LLVMExecutionEngineRef)
       (module : LLVMModuleRef)
       (outmod : (_ptr o LLVMModuleRef))
       (message : (_ptr io LLVMMessage) = #f)
       ->
       (err : LLVMBool)
       ->
       (if err message outmod)))

(define-llvm LLVMFindFunction
 (_fun (ee name) ::
       (ee : LLVMExecutionEngineRef)
       (name : _string)
       (outfun : (_ptr o LLVMValueRef))
       -> (err : LLVMBool)
       -> (if err #f outfun)))


(define-llvm LLVMRecompileAndRelinkFunction
 (_fun LLVMExecutionEngineRef LLVMValueRef -> _pointer))

(define-llvm LLVMGetExecutionEngineTargetData
 (_fun LLVMExecutionEngineRef -> LLVMTargetDataRef))

(define-llvm LLVMAddGlobalMapping
 (_fun LLVMExecutionEngineRef LLVMValueRef _pointer -> _void))

(llvm-unsafe-context
 (define (LLVMAddGlobalMappingForFunction fun-type)
  (get-ffi-obj 'LLVMAddGlobalMapping llvm-lib 
   (_fun LLVMExecutionEngineRef LLVMValueRef fun-type -> LLVMGenericValueRef))))

(define-llvm LLVMGetPointerToGlobal
 (_fun LLVMExecutionEngineRef LLVMValueRef -> _pointer))

(define-llvm LLVMInitializeX86TargetInfo (_fun -> _void))
(define-llvm LLVMInitializeX86Target (_fun -> _void))

(LLVMInitializeX86TargetInfo)
(LLVMInitializeX86Target)

