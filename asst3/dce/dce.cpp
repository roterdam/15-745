// 15-745 S15 Assignment 1: LocalOpts.cpp
// Group: mchoquet, neilshah
////////////////////////////////////////////////////////////////////////////////

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "dataflow.h"

#include <ostream>
#include <fstream>
#include <iostream>
#include <vector>

using namespace llvm;
using std::vector;

namespace {


struct TranslationMaps {
  DenseMap<const Value *, int> *bitMap;
  DenseMap<int, const Value *> *valMap;
};

static TranslationMaps getTranslationMaps(const Function&);
static vector<const Value *> getValuesUsedInFunction(const Function&);
static bool isTracked(const Value *);

/*********** liveness transfer functions ***********/
class LivenessTransferFunction : public dataflow::TransferFunction {
 public:
  ~LivenessTransferFunction() { }
  LivenessTransferFunction(const BitVector& genBV, const BitVector& killBV) :
    _genBV(genBV), _killBV(killBV) { }

  BitVector& operator()(BitVector& bv) const {
    return (bv.reset(_killBV)) |= _genBV;
  }

 private:
  const BitVector _genBV;
  const BitVector _killBV;
};


class LivenessTFBuilder : public dataflow::TransferFunctionBuilder {
 private:
  void processWrite(const Value *v, BitVector *genBV, BitVector *killBV) const {
    if (_bitMap.count(v) != 0) {
      const int i = _bitMap.lookup(v);
      genBV->reset(i);
      killBV->set(i);
    }
  }

  void processRead(const Value *v, BitVector *genBV, BitVector *killBV) const {
    if (_bitMap.count(v) != 0) {
      genBV->set(_bitMap.lookup(v));
    }
  }

  void processInst(const Instruction *I, BitVector *genBV, BitVector *killBV)
      const {
    processWrite(I, genBV, killBV);
    for (auto it = I->op_begin(), et = I->op_end(); it != et; ++it) {
      processRead((*it).get(), genBV, killBV);
    }
  }

  void processPhi(const PHINode *phi, const BasicBlock *prevBlock,
                  BitVector *genBV, BitVector *killBV) const {
    processWrite(phi, genBV, killBV);
    processRead(phi->getIncomingValueForBlock(prevBlock), genBV, killBV);
  }

 public:
  LivenessTFBuilder(const DenseMap<const Value *, int>& bitMap) :
    _bitMap(bitMap), _n(bitMap.size()) { }
  ~LivenessTFBuilder() { }

  dataflow::TransferFunction *makeInstTransferFn(
      const Instruction *inst) const {
    BitVector genBV(_n, false);
    BitVector killBV(_n, false);
    processInst(inst, &genBV, &killBV);
    return new LivenessTransferFunction(genBV, killBV);
  }


  LivenessTransferFunction *makePhiSeqTransferFn(
      const vector<const PHINode *>& phis, const BasicBlock *prevBlock) const {
    BitVector genBV(_n, false);
    BitVector killBV(_n, false);
    for (auto it = phis.rbegin(), et = phis.rend(); it != et; ++it) {
      processPhi(*it, prevBlock, &genBV, &killBV);
    }
    return new LivenessTransferFunction(genBV, killBV);
  }

  dataflow::TransferFunction *makeInstSeqTransferFn(
      const vector<const Instruction *>& insts) const {
    BitVector genBV(_n, false);
    BitVector killBV(_n, false);
    for (auto it = insts.rbegin(), et = insts.rend(); it != et; ++it) {
      processInst(*it, &genBV, &killBV);
    }
    return new LivenessTransferFunction(genBV, killBV);
  }

 private:
  const DenseMap<const Value *, int>& _bitMap;
  const int _n;
};




/********* mapping values to integers *********/

// Computes a mapping between Values used anywhere in F and ints in [0, n).
TranslationMaps getTranslationMaps(const Function& F) {
  DenseMap<const Value *, int> *bitMap = new DenseMap<const Value *, int>;
  DenseMap<int, const Value *> *valMap = new DenseMap<int, const Value *>;
  int current = 0;

  for (const Value *v : getValuesUsedInFunction(F)) {
    if (bitMap->count(v) == 0) {
      (*bitMap)[v] = current;
      (*valMap)[current] = v;
      current++;
    }
  }

  TranslationMaps tMaps;
  tMaps.bitMap = bitMap;
  tMaps.valMap = valMap;
  return tMaps;
}

// Returns a list of the Value*s used anywhere in the function F. This can be in
// arbitrary order, but cannot include duplicates.
vector<const Value *> getValuesUsedInFunction(const Function& F) {
  vector<const Value *> v;
  for (auto it = F.arg_begin(), et = F.arg_end(); it != et; ++it) {
    v.push_back(&*it);
  }
  for (const BasicBlock& B : F) {
    for (const Instruction& I : B) {
      if (isTracked(&I)) {
        v.push_back(&I);
      }
      for (auto it = I.op_begin(), et = I.op_end(); it != et; ++it) {
        if (isTracked((*it).get())) {
          v.push_back((*it).get());
        }
      }
    }
  }
  std::sort(v.begin(), v.end());
  auto newEnd = std::unique(v.begin(), v.end());
  int newSize = newEnd - v.begin();
  v.resize(newSize);
  return v;
}

/* TODO: update (the handout gave some hints I think?) */
bool isTracked(const Value *v) {
  const Type *tp = v->getType();
  return ((isa<Instruction>(v) || isa<Argument>(v)) &&
          (!(tp->isVoidTy() || tp->isLabelTy() ||
             tp->isMetadataTy() || tp->isFunctionTy())));
}






/********** dead code elimination pass **********/
class DeadCodeElimination : public FunctionPass {
 private:
  dataflow::DataflowConfiguration makeDataflowConfiguration(const Function& F,
      const DenseMap<const Value *, int> *bitMap) {
    dataflow::DataflowConfiguration config;
    config.dir = dataflow::FlowDirection::BACKWARD;
    config.fnBuilder = new LivenessTFBuilder(*bitMap);
    config.meetWith = dataflow::bvUnion;
    config.top = dataflow::zerosVector(bitMap->size());
    config.boundaryState = dataflow::zerosVector(bitMap->size());
    return config;
  }


 public:
  static char ID;
  DeadCodeElimination() : FunctionPass(ID) { }
  
  bool runOnFunction(Function& F) {
    TranslationMaps maps = getTranslationMaps(F);
    const DenseMap<const Value *, int> *bitMap = maps.bitMap;
    const DenseMap<int, const Value *> *valMap = maps.valMap;
    
    const dataflow::DataflowConfiguration config =
        makeDataflowConfiguration(F, bitMap);
    dataflow::DataMap *out = dataflow::dataflow(F, config);
    
    /* Remove all non-live instructions here. */
    
    delete bitMap;
    delete valMap;
    delete config.fnBuilder;
    delete out;

    return false;
  }
};


// LLVM uses the address of this static member to identify the pass, so the
// initialization value is unimportant.
char DeadCodeElimination::ID = 0;
RegisterPass<DeadCodeElimination> X("dce", "15745: dead code elimination");

}
