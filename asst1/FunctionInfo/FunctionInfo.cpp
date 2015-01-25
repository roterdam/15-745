// 15-745 S15 Assignment 1: FunctionInfo.cpp
// Group: mchoquet, <<<<<<>>>>>>
////////////////////////////////////////////////////////////////////////////////

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Module.h"

#include <ostream>
#include <fstream>
#include <iostream>

using namespace llvm;

namespace {

class FunctionInfo : public ModulePass {

  // Output the function information to standard out.
  void printFunctionInfo(Module& M) {
    outs() << "Module " << M.getModuleIdentifier().c_str() << "\n"
           << "Name,\tArgs,\tCalls,\tBlocks,\tInsns\n";
  }

public:

  static char ID;

  FunctionInfo() : ModulePass(ID) { }

  ~FunctionInfo() { }

  // We don't modify the program, so we preserve all analyses
  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesAll();
  }

  virtual bool runOnFunction(Function &F) {
    int numInss = 0;
    for (BasicBlock& bb : F) {
      numInss += bb.size();
    }
    outs() << F.getName() << ",\t";
    if (F.isVarArg()) {
      outs() << "*";
    } else {
      outs() << F.getArgumentList().size();
    }
    outs() << ",\t"
           << F.getNumUses() << ",\t"
           << F.size() << ",\t"
           << numInss << "\n";
    return false;
  }
  
  virtual bool runOnModule(Module& M) {
    printFunctionInfo(M);
    for (Module::iterator MI = M.begin(), ME = M.end(); MI != ME; ++MI) {
      runOnFunction(*MI);
    }
    return false;
  }

};

// LLVM uses the address of this static member to identify the pass, so the
// initialization value is unimportant.
char FunctionInfo::ID = 0;
RegisterPass<FunctionInfo> X("function-info", "15745: Function Information");

}
