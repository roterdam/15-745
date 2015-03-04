
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
using std::swap;

namespace {

/*
  Because we have SSA, the algorithm from class can be simplified a little.
  The instructions we will move are all of the following:
  --> loop-invariant.
  --> in a block that dominates all loop exits (NO LONGER NEEDED).
*/
class LoopInvariantCodeMotion : public LoopPass {
 private:
  /*
    For every block B in L, prints the immediate dominator of B.
  */
  static void printDominanceInformation(const Loop *L, const DTree& dTree) {
    outs() << "Loop contents:\n";
    for (auto it = L->block_begin(), et = L->block_end(); it != et; ++it) {
      BasicBlock *B = *it;
      outs() << "  " << B->getName() << " <-- " << dTree.lookup(B)->getName()
             << "\n";
    }
  }

  /*
    Returns the loop-invariant instructions that can be lifted out of the loop.
    The requirements are:
    --> Must be loop-invariant.
        --> SafeToSpeculativelyExecute, doesn't touch memory, not a landing pad.
        --> not a PHI
        --> all operands are either loop-invariant, or not defined in the loop.
    --> Must dominate all loop exits. (UPDATE: we don't have to do this one)
    NOTE: because of the structure of the algorithm, the returned instructions
          are guaranteed to be in a valid order (i.e. a topological ordering
          with respect to each other).
  */
  vector<Instruction *> getLiftableInstructions(const Loop *L,
                                                const DTree& dTree) {
    /* Get all instructions that could be moved out, if they were invariant. */
    vector<Instruction *> candidates;
    for (auto it = L->block_begin(), et = L->block_end(); it != et; ++it) {
      for (Instruction& I : **it) {
        if (isSafeToSpeculativelyExecute(&I) &&
            !I.mayReadFromMemory() &&
            !isa<LandingPadInst>(&I) &&
            !isa<PHINode>(&I)) {
          candidates.push_back(&I);
        }
      }
    }

    /* Repeatedly loop over the candidates to extract invariant instructions. */
    DenseSet<Instruction *> invariantExps;
    unsigned int count = invariantExps.size();
    unsigned int oldCount;
    do {
      outs() << "start of iteration: count = " << count << "\n";
      oldCount = count;
      for (int i = count; i < candidates.size(); i++) {
        bool allOperandsInvariant = true;
        Instruction *I = candidates[i];
        outs() << "  considering " << I->getName() << "...";
        for (auto it = I->op_begin(), et = I->op_end(); it != et; ++it) {
          Instruction *I2 = dyn_cast<Instruction>(it->get());
          if (I2 != nullptr && L->contains(I2) && invariantExps.count(I2)==0) {
            outs() << "NO because of " << I2->getName() << "\n";
            allOperandsInvariant = false;
            break;
          }
        }
        if (!allOperandsInvariant) {
          continue;
        }
        outs() << "YES\n";
        invariantExps.insert(I);
        swap(candidates[i], candidates[count++]);
      }
      outs() << "end of iteration: count = " << count << "\n";
    } while (count != oldCount);
   
 
    /* Return the result. */
    vector<Instruction *>liftableInstructions(invariantExps.begin(),
                                              invariantExps.end());
    outs() << "found " << liftableInstructions.size() << " liftable instructions\n";

    return liftableInstructions;
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
