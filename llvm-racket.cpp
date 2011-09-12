
#include "llvm-c/Core.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Type.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/GlobalVariable.h"
#include "llvm/GlobalAlias.h"
#include "llvm/LLVMContext.h"
//#include "llvm/TypeSymbolTable.h"
#include "llvm/InlineAsm.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstdlib>
#include <cstring>


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



#ifdef __cplusplus
}

#endif /* !defined(__cplusplus) */
