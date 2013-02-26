#include "llvm-c/Core.h"
#include "llvm/Type.h"
#include "llvm/PassManager.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/DataLayout.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/ADT/Triple.h"


#ifdef __cplusplus

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

  // Add an appropriate DataLayout instance for this module.
  const std::string &ModuleDataLayout = M->getDataLayout();
  if (!ModuleDataLayout.empty()) {
    DataLayout *TD = new DataLayout(ModuleDataLayout);
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
