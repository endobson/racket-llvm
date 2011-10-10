
#include "llvm-c/Core.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Type.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/GlobalVariable.h"
#include "llvm/GlobalAlias.h"
#include "llvm/LLVMContext.h"
#include "llvm/LLVMContext.h"
#include "llvm/PassManager.h"
#include "llvm/InlineAsm.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/ADT/Triple.h"
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>


#ifdef __cplusplus

/* Need these includes to support the LLVM 'cast' template for the C++ 'wrap' 
   and 'unwrap' conversion functions. */
#include "llvm/Module.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/IRBuilder.h"

extern "C" {
#endif

using namespace llvm;


/*===-- Error handling ----------------------------------------------------===*/

const char* LLVMGetTypeDescription(LLVMTypeRef Ty) {
  std::string DescStr;
  raw_string_ostream DescOS(DescStr);
  unwrap(Ty)->print(DescOS);
  std::string str = DescOS.str();
  char* cstr = (char*) malloc(str.length()+1);
  strcpy(cstr,str.c_str());
  return cstr;
}

char* LLVMGetValueDescription(LLVMValueRef V) {
    std::string DescStr;
    raw_string_ostream DescOS(DescStr);
    unwrap(V)->print(DescOS);
    std::string str = DescOS.str();
    char* cstr = (char*) malloc(str.length()+1);
    strcpy(cstr,str.c_str());

    return cstr;
}


char* LLVMGetModuleDescription(LLVMModuleRef M) {
    std::string DescStr;
    raw_string_ostream DescOS(DescStr);
    unwrap(M)->print(DescOS,NULL);
    std::string str = DescOS.str();
    char* cstr = (char*) malloc(str.length()+1);
    strcpy(cstr,str.c_str());

    return cstr;
}


bool LLVMIsValidTypeIndex(LLVMTypeRef Ty, LLVMValueRef Idx) {
    CompositeType* Comp = cast<CompositeType>(unwrap(Ty));
    const Value* V = unwrap(Idx);
    return Comp->indexValid(V);
}

LLVMTypeRef LLVMGetTypeAtIndex(LLVMTypeRef Ty, LLVMValueRef Idx) {
    CompositeType* Comp = cast<CompositeType>(unwrap(Ty));
    const Value* V = unwrap(Idx);
    return wrap(Comp->getTypeAtIndex(V));
}

bool LLVMIsTerminatorInstruction(LLVMValueRef V) {
     return cast<Instruction>(unwrap(V))->isTerminator();
}

void LLVMInitializeRacket() {
    InitializeNativeTarget();
}

bool LLVMOptimizeModule(LLVMModuleRef Mod) {
    Module* M = unwrap(Mod);

  // Create a PassManager to hold and optimize the collection of passes we are
  // about to build.
  PassManager Passes;

  // Add an appropriate TargetLibraryInfo pass for the module's triple.
  TargetLibraryInfo *TLI = new TargetLibraryInfo(Triple(M->getTargetTriple()));

  // Add an appropriate TargetData instance for this module.
  const std::string &ModuleDataLayout = M->getDataLayout();
  if (!ModuleDataLayout.empty()) {
    TargetData *TD = new TargetData(ModuleDataLayout);
    Passes.add(TD);
  }


  Passes.add(createVerifierPass());  // Verify that input is correct

  // -std-compile-opts adds the same module passes as -O3.
  PassManagerBuilder Builder;
  Builder.Inliner = createFunctionInliningPass();
  Builder.OptLevel = 3;
  Builder.populateModulePassManager(Passes);


  // Now that we have all of the passes ready, run them.
  bool change = Passes.run(*M);

  return change;
}


LLVMValueRef LLVMGetIntrinsic(
  LLVMModuleRef M,
  Intrinsic::ID id,
  LLVMTypeRef *ParamTypes,
  unsigned ParamCount) {

 ArrayRef<Type*> Tys(unwrap(ParamTypes), ParamCount);
 return wrap(Intrinsic::getDeclaration(unwrap(M), id, Tys));
}



#ifdef __cplusplus
}

#endif /* !defined(__cplusplus) */
