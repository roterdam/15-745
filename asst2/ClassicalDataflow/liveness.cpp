// 15-745 S15 Assignment 2: liveness.cpp
// Group: mchoquet, nshah, rokhinip
////////////////////// /////////////////////////////////////////////////////////

#include <algorithm>
#include <vector>

#include "llvm/IR/Function.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

#include "available-support.h"
#include "dataflow.h"

using namespace llvm;
using std::vector;

namespace {

struct TranslationMaps {
  DenseMap<const Value *, int> *bitMap;
  DenseMap<int, const Value *> *valMap;
};

static TranslationMaps getOperandMaps(const Function&);
static vector<const Value *> getValuesUsedInFunction(const Function&);
static bool isTracked(const Value *);
static void printLiveSet(const BitVector&);


class LivenessTransferFunction : public dataflow::TransferFunction {
 public:
  ~LivenessTransferFunction() { }
  LivenessTransferFunction(const BitVector& killBV, const BitVector& genBV) :
    _killBV(killBV), _genBV(genBV) { }

  BitVector& operator()(BitVector& bv) const {
    return (bv.reset(_killBV)) |= _genBV;
  }

 private:
  const BitVector _killBV;
  const BitVector _genBV;
};


class LivenessTFBuilder : public dataflow::TransferFunctionBuilder {
 public:
  LivenessTFBuilder(const DenseMap<const Value *, int>& bitMap) :
    _bitMap(bitMap), _n(bitMap.size()) { }
  ~LivenessTFBuilder() { }

  dataflow::TransferFunction *makeInstTransferFn(
      const Instruction *inst) const {
    BitVector killBV(_n, false);
    BitVector genBV(_n, false);
    if (_bitMap.count(inst) != 0) {
      killBV.set(_bitMap.lookup(inst));
    }
    for (auto it = inst->op_begin(), et = inst->op_end(); it != et; ++it) {
      Value *v = (*it).get();
      if (_bitMap.count(v) != 0) {
        genBV.set(_bitMap.lookup(v));
      }
    }
    return new LivenessTransferFunction(killBV, genBV);
  }
  
  LivenessTransferFunction *makePhiSeqTransferFn(const vector<const PHINode *>& phis,
                                                 const BasicBlock *prevBlock) const {
    BitVector killBV(_n, false);
    BitVector genBV(_n, false);
    for (auto it = phis.rbegin(), et = phis.rend(); it != et; ++it) {
      const PHINode *I = *it;
      if (_bitMap.count(I) != 0) {
        const int i = _bitMap.lookup(I);
        genBV.reset(i);
        killBV.set(i);
      }
      Value *arg = I->getIncomingValueForBlock(prevBlock);
      if (_bitMap.count(arg) != 0) {
        const int i = _bitMap.lookup(arg);
        genBV.set(i);
      }
    }
    return new LivenessTransferFunction(killBV, genBV);
  }

  dataflow::TransferFunction *makeInstSeqTransferFn(
      const vector<const Instruction *>& insts) const {
    BitVector killBV(_n, false);
    BitVector genBV(_n, false);
    for (auto it = insts.rbegin(), et = insts.rend(); it != et; ++it) {
      const Instruction *I = *it;
      if (_bitMap.count(I) != 0) {
        const int i = _bitMap.lookup(I);
        genBV.reset(i);
        killBV.set(i);
      }
      for (auto it = I->op_begin(), et = I->op_end(); it != et; ++it) {
        Value *v = (*it).get();
        if (_bitMap.count(v) != 0) {
          genBV.set(_bitMap.lookup(v));
        }
      }
    }
    return new LivenessTransferFunction(killBV, genBV);
  }

 private:
  const DenseMap<const Value *, int>& _bitMap;
  const int _n;
};

class LivenessBitVectorPrinter : public dataflow::BitVectorPrinter {
 public:
  LivenessBitVectorPrinter(const DenseMap<int, const Value *>& valMap) :
    _valMap(valMap) { }

  void print(const BitVector& bv) const {
    outs() << "{";
    bool firstItem = true;
    for (int i = 0; i < bv.size(); i++) {
      if (bv.test(i)) {
        if (!firstItem) {
          outs() << ", ";
        }
        outs() << getShortValueName(_valMap.lookup(i));
        firstItem = false;
      }
    }
    outs() << "}";
  }

 private:
  const DenseMap<int, const Value *>& _valMap;
};



class Liveness : public FunctionPass {
 public:
  static char ID;
  
  Liveness() : FunctionPass(ID) { }
  
  virtual bool runOnFunction(Function& F) {
    TranslationMaps maps = getOperandMaps(F);
    const DenseMap<const Value *, int> *bitMap = maps.bitMap;
    const DenseMap<int, const Value *> *valMap = maps.valMap;

    dataflow::DataflowConfiguration config;
    config.dir = dataflow::FlowDirection::BACKWARD;
    config.fnBuilder = new LivenessTFBuilder(*bitMap);
    config.meetWith = dataflow::bvUnion;
    config.top = dataflow::zerosVector(maps.bitMap->size());
    config.boundaryState = dataflow::zerosVector(maps.bitMap->size());

    dataflow::DataMap *out = dataflow::dataflow(F, config);

    const dataflow::BitVectorPrinter *printer = new LivenessBitVectorPrinter(*valMap);
    dataflow::printDataMap(F, *out, config.dir, printer);
    outs() << "\n\n";

    delete bitMap;
    delete valMap;
    delete config.fnBuilder;
    delete printer;
    delete out;

    return false;
  }
  
  virtual void getAnalysisUsage(AnalysisUsage& AU) const {
    AU.setPreservesAll();
  }
  
 private:        
  dataflow::DataflowConfiguration config;
};

// Computes a mapping between Values used anywhere in F and ints in [0, n).
TranslationMaps getOperandMaps(const Function& F) {
  DenseMap<const Value *, int> *bitMap = new DenseMap<const Value *, int>;
  DenseMap<int, const Value *> *valMap = new DenseMap<int, const Value *>;
  int current = 0;

  for (const Value *v : getValuesUsedInFunction(F)) {
    v->printAsOperand(outs());
    outs() << "\n";
    // TODO: this shouldn't be necessary, verify later.
    if (bitMap->count(v) == 0) {
      (*bitMap)[v] = current;
      (*valMap)[current] = v;
      current++;
    }
  }

  TranslationMaps retVal;
  retVal.bitMap = bitMap;
  retVal.valMap = valMap;
  return retVal;
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

bool isTracked(const Value *v) {
  const Type *tp = v->getType();
  return ((isa<Instruction>(v) || isa<Argument>(v)) &&
          (!(tp->isVoidTy() || tp->isLabelTy() ||
             tp->isMetadataTy() || tp->isFunctionTy())));
}


void printLiveSet(const BitVector& v) {
  outs() << "{";
  for (int i = 0; i < v.size(); i++) {
    outs() << (v[i] ? "1" : "0");
  }
  outs() << "}\n";
}

char Liveness::ID = 0;
RegisterPass<Liveness> X("liveness", "15745 Liveness");
  
}
