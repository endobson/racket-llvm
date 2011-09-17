#lang racket

(require
  "define.rkt"
  "ctypes.rkt")

(require ffi/unsafe)

(provide (all-defined-out))

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
(define-llvm-unsafe LLVMTypeOf (_fun LLVMValueRef -> LLVMTypeRef))
(define-llvm-unsafe LLVMGetValueName (_fun LLVMValueRef -> _string))

(define-llvm-unsafe LLVMSetValueName (_fun LLVMValueRef _string -> _void))

(define-llvm-unsafe LLVMDumpValue (_fun LLVMValueRef -> _void))
(define-llvm-unsafe LLVMReplaceAllUsesWith (_fun LLVMValueRef LLVMValueRef -> _void))
(define-llvm-unsafe LLVMHasMetadata (_fun LLVMValueRef -> _int))
(define-llvm-unsafe LLVMGetMetadata (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm-unsafe LLVMSetMetadata (_fun LLVMValueRef _uint LLVMValueRef -> _void))


;/* Conversion functions. Return the input value if it is an instance of the
;   specified class, otherwise NULL. See llvm::dyn_cast_or_null<>. */
#|
#define LLVM_DECLARE_VALUE_CAST(name) \
  LLVMValueRef LLVMIsA##name(LLVMValueRef Val);
LLVM_FOR_EACH_VALUE_SUBCLASS(LLVM_DECLARE_VALUE_CAST)
|#

;/* Operations on Uses */
(define-llvm-unsafe LLVMGetFirstUse (_fun LLVMValueRef -> LLVMUseRef))
(define-llvm-unsafe LLVMGetNextUse (_fun LLVMUseRef -> LLVMUseRef))
(define-llvm-multiple-unsafe
 (LLVMGetUser LLVMGetUsedValue)
 (_fun LLVMUseRef -> LLVMValueRef))

;/* Operations on Users */
(define-llvm-unsafe LLVMGetOperand (_fun LLVMValueRef _uint -> LLVMValueRef))
(define-llvm-unsafe LLVMSetOperand (_fun LLVMValueRef _uint LLVMValueRef -> _void))
(define-llvm-unsafe LLVMGetNumOperands (_fun LLVMValueRef -> _int))


(define-llvm-multiple-unsafe
 (LLVMIsConstant
  LLVMIsNull
  LLVMIsUndef)
 (_fun LLVMValueRef -> LLVMBool))


;/* Operations on metadata */
(define-llvm-unsafe LLVMMDStringInContext
 (_fun LLVMContextRef _string _uint -> LLVMValueRef))
(define-llvm-unsafe LLVMMDString
 (_fun _string _uint -> LLVMValueRef))

(define-llvm-unsafe LLVMMDNodeInContext
 (_fun LLVMContextRef LLVMValueRef _uint -> LLVMValueRef))
(define-llvm-unsafe LLVMMDNode
 (_fun LLVMValueRef _uint -> LLVMValueRef))

