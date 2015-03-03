
#include "llvm/Pass.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include <ostream>
#include <fstream>
#include <iostream>
#include <utility>
#include <vector>

#include "dominance.h"

using namespace llvm;

using std::vector;
using std::make_pair;
using std::min;
using std::pair;

namespace {


/*
  Because we have SSA, the algorithm from class can be simplified a little.
  The instructions we will move are all of the following:
  --> loop-invariant.
  --> in a block that dominates all loop exits.
*/
class LoopInvariantCodeMotion : public LoopPass {
 private:
  /*
    For every block B in L, prints the immediate dominator of B.
    TODO: implement
  */
  static void printDominanceInformation(const Loop *L, const DTree& dTree) {
    
  }

  /*
    Returns the loop-invariant instructions that can be lifted out of the loop.
    The requirements are:
    --> Must be loop-invariant.
        --> SafeToSpeculativelyExecute, doesn't touch memory, not a landing pad.
        --> not a PHI
        --> all operands are either loop-invariant, or not defined in the loop.
    --> Must dominate all loop exits.
    TODO: implement
  */
  vector<Instruction *> getLiftableInstructions(const Loop *L,
                                                const DTree& dTree) {
    vector<Instruction *> candidates;
    for (auto it = L->block_begin(), et = L->block_end(); it != et; ++it) {
      BasicBlock& B = **it;
      if (/* TODO: B dominates all exits */ true) {
        for (Instruction& I : B) {
          if (isSafeToSpeculativelyExecute(&I) &&
              !I.mayReadFromMemory() &&
              !isa<LandingPadInst>(&I) &&
              !isa<PHINode>(&I)) {
            candidates.push_back(&I);
          }
        }
      }
    }

    vector<Instruction *> liftableInstructions;
    unsigned int oldCount;
    do {
      oldCount = liftableInstructions.size();
      for (Instruction *I : candidates) {
        if (count(liftableInstructions.begin(), liftableInstructions.end(), I) == 0) {
          liftableInstructions.push_back(I);
        }
      }
      /* Add loop-invariant instructions */
    } while (liftableInstructions.size() != oldCount);

    outs() << "found " << liftableInstructions.size() << " liftable instructions\n";

    return liftableInstructions;
  }

  /*
    Orders the instructions such that each instruction is before any
    instructions that depend on it.
    TODO: implement
  */
  void toposortInsts(vector<Instruction *>& insts) {

  }

 public:
  static char ID;
  LoopInvariantCodeMotion() : LoopPass(ID) { }

  bool runOnLoop(Loop *L, LPPassManager& LPM) {
    BasicBlock *preheader = L->getLoopPreheader();
    if (preheader == nullptr) {
      return false;
    }
    DTree *dt = buildDominanceTree(*(preheader->getParent()));
    printDominanceTree(*dt);
    printDominanceInformation(L, *dt);

    vector<Instruction *> loopInvariantInsts = getLiftableInstructions(L, *dt);
    toposortInsts(loopInvariantInsts);

    for (Instruction *I : loopInvariantInsts) {
      I->moveBefore(preheader->getTerminator());
    }

    delete dt;
    return false;
  }
};





char LoopInvariantCodeMotion::ID = 0;
RegisterPass<LoopInvariantCodeMotion> X("loop-invariant-code-motion",
                                        "15745: loop-invariant code motion");
} // namespace
