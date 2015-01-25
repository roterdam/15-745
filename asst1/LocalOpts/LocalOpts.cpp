// 15-745 S15 Assignment 1: LocalOpts.cpp
// Group: mchoquet, neilshah
////////////////////////////////////////////////////////////////////////////////

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Tranforms/Utils/BasicBlockUtils.h"

#include <ostream>
#include <fstream>
#include <iostream>

using namespace llvm;

namespace {

class LocalOpts : public ModulePass {

  // do some local optimizations on the module

  void doLocalOpts(Module& M) {
    outs() << "Module " << M.getModuleIdentifier().c_str() << "\n";

    nAlgIdent = 0;
    nConstFold = 0;
    nStrRed = 0;

    bool progress = true;

    while(progress)
    {
      
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
char FunctionInfo::ID = 0;
RegisterPass<LocalOpts> X("some-local-opts", "15745: Some local optimizations");

}
