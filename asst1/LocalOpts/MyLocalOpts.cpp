// 15-745 S15 Assignment 1: LocalOpts.cpp
// Group: mchoquet, neilshah
////////////////////////////////////////////////////////////////////////////////

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"


#include <ostream>
#include <fstream>
#include <iostream>

using namespace llvm;

namespace {

class LocalOpts : public ModulePass {

  // do some local optimizations on the module

  void doLocalOpts(Module& M) {
    outs() << "Module " << M.getModuleIdentifier().c_str() << "\n";

    int nAlgIdent = 0;
    int nConstFold = 0;
    int nStrRed = 0;

    Type *int32Typ = Type::getInt32Ty(M.getContext());
    Constant *constInt32_5 = ConstantInt::get(int32Typ, 5);

    for (Function& F : M) {
      for (BasicBlock& B : F) {
        for (BasicBlock::iterator i = B.begin(), iEnd = B.end(); i != iEnd;) {
          if (BinaryOperator *opInst = dyn_cast<BinaryOperator>(&*i)) {
            ReplaceInstWithValue(B.getInstList(), i, constInt32_5);
          } else {
            ++i;
          }
        }
      }
    }

  }

public:

  static char ID;

  LocalOpts() : ModulePass(ID) { }

  ~LocalOpts() { }

  virtual bool runOnModule(Module& M) {
    doLocalOpts(M);
    return false;
  }

};

// LLVM uses the address of this static member to identify the pass, so the
// initialization value is unimportant.
char LocalOpts::ID = 0;
RegisterPass<LocalOpts> X("some-local-opts", "15745: Some local optimizations");

}
