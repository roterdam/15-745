// 15-745 S15 Assignment 3: deadcode.cpp
// Group: mchoquet, rokhinip 
////////////////////////////////////////////////////////////////////////////////

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/IR/InstIterator.h" 
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
static void printBitVector(const BitVector& bv);

/*********** Faintness transfer functions ***********/

class FaintnessInstTransferFunction : public dataflow::TransferFunction {
 public:
  ~FaintnessInstTransferFunction() { }
  FaintnessInstTransferFunction(const BitVector& genBV, const BitVector& killBV,
  bool non_faint_instr, const int bit_idx) :
    _genBV(genBV), _killBV(killBV), 
    _non_faint_instr(non_faint_instr),  // 
    _bit_idx(bit_idx)  { }

  BitVector& operator()(BitVector& bv) const {
    // Transfer function for regular statements:
    //          _ 
    //         |  (x U LHS) - RHS if (LHS not in x)
    //  f(x) = |
    //         |_  x    otherwise
    //
    // LHS is the gen set, RHS is the kill set

    // Eg. function only returns 42;
    if (bv.size() == 0) return bv;

    if (_bit_idx >= bv.size()){
      printf("_bit_idx: %d, bv_size: %d\n", _bit_idx, bv.size());
      exit(-1); 
    }

    // Then all operands involved must be non-faint regardless of input value
    if (_non_faint_instr){
      bv.reset(_killBV);
      return bv;
    }
    
    // For other cases
    if (bv.test(_bit_idx) == 0){ 
      bv |= _genBV;
      bv.reset(_killBV);
    } 
    
    return bv;
  }

 private:
  const BitVector _genBV;
  const BitVector _killBV;
  bool _non_faint_instr;
  const int _bit_idx;
};

class ComposedTransferFunction : public dataflow::TransferFunction {
 public:
  ~ComposedTransferFunction() { }
  ComposedTransferFunction(vector<dataflow::TransferFunction *> tfs) :
   _transfer_fns(tfs) { }

  BitVector& operator()(BitVector& bv) const {
    // Execute the vector of transfer functions in order
    for (auto it = _transfer_fns.begin(), et = _transfer_fns.end(); it != et; ++it){
        dataflow::TransferFunction *tf = *it;
        bv = (*tf)(bv);
    }
    return bv;
  }

 private:
  vector<dataflow::TransferFunction *> _transfer_fns;
};

class FaintnessTFBuilder : public dataflow::TransferFunctionBuilder {
 private:
   void processLHS(const Value *v, BitVector *genBV, BitVector *killBV) const {
    if (_bitMap.count(v) != 0) {
      const int i = _bitMap.lookup(v);
      genBV->set(i);
    }
  }
    
  void processRHS(const Value *v, BitVector *genBV, BitVector *killBV) const {
    if (_bitMap.count(v) != 0) {
      int v_idx = _bitMap.lookup(v);
      killBV->set(v_idx);
    }
  }

  void processInst(const Instruction *I, BitVector *genBV, BitVector *killBV)
      const {
    
    processLHS(I, genBV, killBV);
    for (auto it = I->op_begin(), et = I->op_end(); it != et; ++it) {
      processRHS((*it).get(), genBV, killBV);
    }
  }

  void processPhi(const PHINode *phi, const BasicBlock *prevBlock,
                  BitVector *genBV, BitVector *killBV) const {
    processLHS(phi, genBV, killBV);
    processRHS(phi->getIncomingValueForBlock(prevBlock), genBV, killBV);
  }

 public:
  FaintnessTFBuilder(const DenseMap<const Value *, int>& bitMap) :
    _bitMap(bitMap), _n(bitMap.size()) { }
  ~FaintnessTFBuilder() { }

  dataflow::TransferFunction *makeInstTransferFn(
      const Instruction *inst) const {
    BitVector genBV(_n, false);
    BitVector killBV(_n, false);
    int bit_idx = _bitMap.lookup(inst);
   
    // Create the gen and kill set for this instr
    processInst(inst, &genBV, &killBV);

    // TODO: In small.c, we are looking at a div operation but somehow, it doesn't get 
    // categorized under mayHaveSideEffects or mayThrow. What?
    bool non_faint_instr = false;
    if (isa<TerminatorInst>(inst) || isa<DbgInfoIntrinsic>(inst) || 
        isa<LandingPadInst>(inst) || inst->mayHaveSideEffects() || 
        inst->mayThrow()){
      non_faint_instr = true;
    }
    inst->print(outs());
    outs() << "\tnon_faint_instr: " << non_faint_instr << "\n";
    return new FaintnessInstTransferFunction(genBV, killBV, non_faint_instr, bit_idx);
  }

  dataflow::TransferFunction *makePhiTransferFn(const PHINode *phi, 
                                                const BasicBlock *prevBlock) const {
    BitVector genBV(_n, false);
    BitVector killBV(_n, false);
   
    // Create the gen and kill set for this instr
    processLHS(phi, &genBV, &killBV);
    Value *arg = phi->getIncomingValueForBlock(prevBlock);
    processRHS(arg, &genBV, &killBV);

    int bit_idx = _bitMap.lookup(phi);

    return new FaintnessInstTransferFunction(genBV, killBV, false, bit_idx);
  }

  dataflow::TransferFunction *makePhiSeqTransferFn(
      const vector<const PHINode *>& phis, const BasicBlock *prevBlock) const {
    BitVector genBV(_n, false);
    BitVector killBV(_n, false);

    vector<dataflow::TransferFunction *> composition;
    for (auto it = phis.rbegin(), et = phis.rend(); it != et; ++it) {
      dataflow::TransferFunction *tf = makePhiTransferFn(*it, prevBlock);
      composition.push_back(tf);
    }
    return new ComposedTransferFunction(composition);
  }

  dataflow::TransferFunction *makeInstSeqTransferFn(
      const vector<const Instruction *>& insts) const {
    BitVector genBV(_n, false);
    BitVector killBV(_n, false);
    
    vector<dataflow::TransferFunction *>composition;

    for (auto it = insts.rbegin(), et = insts.rend(); it != et; ++it) {
       dataflow::TransferFunction *tf = makeInstTransferFn(*it);
       composition.push_back(tf);
    }
    return new ComposedTransferFunction(composition); 
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

bool isTracked(const Value *v) {
  const Type *tp = v->getType();
  return ((isa<Instruction>(v) || isa<Argument>(v)) &&
          (!(tp->isVoidTy() || tp->isLabelTy() ||
             tp->isMetadataTy() || tp->isFunctionTy())));
}


class FaintnessVarBitVectorPrinter : public dataflow::BitVectorPrinter {
  public:
      FaintnessVarBitVectorPrinter(const DenseMap<int, const Value *>& valMap):
         _valMap(valMap) { } 
        
      void print(const BitVector& bv) const {
        bool firstItem = true;
        outs() << "{";
        for (int i = 0; i < bv.size(); i++) {
            if (bv.test(i)) {
                if (!firstItem) {
                    outs() << ", ";
                } 
                outs() << *(_valMap.lookup(i));
                firstItem = false;
            }
        }
        outs() << "}";
      
      }
  private:
    const DenseMap<int, const Value *>& _valMap; 
};

/********** dead code elimination pass **********/
class DeadCodeElimination : public FunctionPass {
 private:
  dataflow::DataflowConfiguration makeDataflowConfiguration(const Function& F,
      const DenseMap<const Value *, int> *bitMap) {
    dataflow::DataflowConfiguration config;
    config.dir = dataflow::FlowDirection::BACKWARD;
    config.fnBuilder = new FaintnessTFBuilder(*bitMap);
    config.meetWith = dataflow::bvIntersect;
    config.top = dataflow::onesVector(bitMap->size());
    config.boundaryState = dataflow::onesVector(bitMap->size());
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

    const dataflow::BitVectorPrinter *printer = new FaintnessVarBitVectorPrinter(*valMap);
    dataflow::printDataMap(F, *out, config.dir, printer);

    /* Remove all faint instructions here. */
    // If variable t is defined at line L and variable t is faint at line L+1, then it can be 
    // safely eliminated 
    bool modified = false;
  
    inst_iterator prev = inst_begin(F);
    for (inst_iterator it = prev; it != inst_end(F); ++it){
      Instruction *prevI = &*prev; 
      if (prev != it && isTracked(prevI)) {
        Instruction *currI = &*it;
        BitVector bv = out->lookup(currI);  // Variables faint at line L+1

        int bit_idx = bitMap->lookup(prevI); 
        if (bv.test(bit_idx) == 1){        // Variable 
          // Line L can be safely eliminated
          
          // TODO: eraseFromParent removes instruction and drops all references to it. 
          // Yet somehow we are getting a use remains when def is gone error
          outs() << "Erasing: \n";
          prevI->print(outs());
          outs() << "\n";
          // Erase the definition of prev and all its uses
          prev->eraseFromParent();
          modified = true;
        }
      }
      prev = it;
    }
    
    delete bitMap;
    delete valMap;
    delete config.fnBuilder;
    delete out;
    delete printer;

    return modified;
  }

};

void printBitVector(const BitVector& bv) {
  for (int i = 0; i < bv.size(); i++) {
    outs() << (bv[i] ? "1" : "0"); 
  }
}


// LLVM uses the address of this static member to identify the pass, so the
// initialization value is unimportant.
char DeadCodeElimination::ID = 0;
RegisterPass<DeadCodeElimination> X("deadcode", "15745: dead code elimination");

}
