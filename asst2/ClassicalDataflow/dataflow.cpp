// 15-745 S15 Assignment 2: dataflow.cpp
// Group: mchoquet, nshah, rokhinip
////////////////////////////////////////////////////////////////////////////////

#include "dataflow.h"

#include "llvm/Support/raw_ostream.h"

#include <queue>

namespace llvm {
namespace dataflow {


struct BlockState {
  BitVector in;
  BitVector out;
  DenseMap<const BasicBlock *, TransferFunction *> phiTFs;
  TransferFunction *blockTF;
};
typedef DenseMap<const BasicBlock *, BlockState> BlockStateMap;

static void printFunctionSignature(const Function&);
static void initializeBlockStates(const Function&, BlockStateMap&,
                                  const DataflowConfiguration&);
static DataMap *traverseBackwards(const Function&, BlockStateMap&,
                                  const DataflowConfiguration&);
static DataMap *traverseForwards(const Function&, BlockStateMap&,
                                 const DataflowConfiguration&);
static void printBitVector(const BitVector&);

// The toplevel dataflow function.
DataMap *dataflow(const Function& F, const DataflowConfiguration& config) {
    BlockStateMap blockStates;
    initializeBlockStates(F, blockStates, config);

    if (config.dir == FORWARD) {
        return traverseForwards(F, blockStates, config);
    } else {
        return traverseBackwards(F, blockStates, config);
    }
}

void printDataMap(const Function& F, const DataMap& dataMap,
                  const FlowDirection dir, const BitVectorPrinter *printer) {
  printFunctionSignature(F);
  for (const BasicBlock& B : F) {
    B.printAsOperand(outs());
    outs() << ":\n";
    if (dir == FORWARD && &B == F.begin()) {
      outs() << "--> ";
      printer->print(dataMap.lookup(boundary_point));
      outs() << "\n";
    }
    for (const Instruction& I : B) {
      if (dir == FORWARD) {
        I.print(outs());
        outs() << "\n";
      }
      if (!isa<PHINode>(&I)) {
        outs() << "--> ";
        printer->print(dataMap.lookup(&I));
        outs() << "\n";
      }
      if (dir == BACKWARD) {
        I.print(outs());
        outs() << "\n";
      }
    }
  }
  if (dir == BACKWARD) {
    outs() << "--> ";
    printer->print(dataMap.lookup(boundary_point));
    outs() << "\n";
  }
  outs() << "}\n";
}

const BitVector onesVector(const unsigned int n) {
  return BitVector(n, true);
}

const BitVector zerosVector(const unsigned int n) {
  return BitVector(n, false);
}

BitVector& bvIntersect(BitVector& v1, const BitVector& v2) {
  return v1 &= v2;
}

BitVector& bvUnion(BitVector& v1, const BitVector& v2) {
  return v1 |= v2;
}


/////// private functions


void printFunctionSignature(const Function& F) {
  F.printAsOperand(outs());
  outs() << "(";
  bool firstRound = true;
  for (const Argument& A : F.args()) {
    if (!firstRound) {
      outs() << ", ";
    }
    A.print(outs());
    firstRound = false;
  }
  outs() << ") {\n";
}


// Sets the initial conditions for the given dataflow problem.
void initializeBlockStates(const Function& F, BlockStateMap& blockStates,
                           const DataflowConfiguration& config) {
    if (config.dir == FORWARD) {
        for (const BasicBlock& B : F) {
            blockStates[&B].out = config.top;
        }
    } else {
        for (const BasicBlock& B : F) {
            blockStates[&B].in = config.top;
        }
    }
    const auto *builder = config.fnBuilder;
    for (const BasicBlock& B : F) {
      vector<const PHINode *> phis;
      vector<const Instruction *> nonPhis;
      for (const Instruction& I : B) {
        const PHINode *phi = dyn_cast<const PHINode>(&I);
        if (phi) {
          phis.push_back(phi);
        } else {
          nonPhis.push_back(&I);
        }
      }
      blockStates[&B].blockTF = builder->makeInstSeqTransferFn(nonPhis);
      for (auto it = pred_begin(&B), et = pred_end(&B); it != et; ++it) {
        const BasicBlock *P = *it;
        blockStates[&B].phiTFs[P] = builder->makePhiSeqTransferFn(phis, P);
      }
    }
}

/*
  HOW TO HANDLE PHI INSTRUCTIONS:
    Treat them as being between blocks, rather than within any block.
    Each block still only has a single in[B] and out[B].
    During the "gather" phase, blocks execute the phi functions for the
      corresponding edge in the control flow graph.
*/

DataMap *traverseBackwards(const Function& F, BlockStateMap& blockStates,
                           const DataflowConfiguration& config) {
    vector<const BasicBlock *>workList;
    for (auto it = F.begin(), et = F.end(); it != et; ++it) {
      workList.push_back(&*it);
    } 

    while (!workList.empty()) {
        const BasicBlock *B = workList.back();
        workList.pop_back();

        // Meet in[s] for all successors s of b, and store in out[b].
        BitVector newOut = config.top;
        for (auto it = succ_begin(B), et = succ_end(B); it != et; ++it) {
            const BasicBlock* S = *it;
            BitVector succBV = blockStates[S].in;
            succBV = (*(blockStates[S].phiTFs[B]))(succBV);
            newOut = config.meetWith(newOut, succBV);
        }
        if (succ_begin(B) == succ_end(B)) {
          newOut = config.boundaryState;
        }
        blockStates[B].out = newOut;

        // Set in[b] = f(out[b]).
        BitVector oldIn = blockStates[B].in;
        BitVector newIn = (*(blockStates[B].blockTF))(newOut);
        blockStates[B].in = newIn;

        // If in[b] changed, add all predecessors of b to the work queue.
        if (newIn != oldIn){
            for (auto it = pred_begin(B), et = pred_end(B); it != et; ++it) {
                workList.push_back(*it);
            }
        }
    }

    // Loop through the blocks to get a solution at every program point.
    DataMap *d = new DataMap();
    for (const BasicBlock& B : F) {
      BitVector bv = blockStates[&B].out;
      for (auto it = B.rbegin(), et = B.rend(); it != et; ++it) {
        const Instruction *I = &*it;
        if (!isa<PHINode>(I)) {
          bv = (*(config.fnBuilder->makeInstTransferFn(I)))(bv);
          (*d)[I] = bv;
        }
      }
    }
    (*d)[boundary_point] = blockStates[&(F.back())].out;

    return d;
}


DataMap *traverseForwards(const Function& F, BlockStateMap& blockStates,
                          const DataflowConfiguration& config) {

    // Find a solution for the start of all the blocks.
    vector<const BasicBlock *>workList; 
    for (auto it = F.begin(), et = F.end(); it != et; ++it) {
        workList.push_back(&*it);
    } 

    while (!workList.empty()) {
        const BasicBlock *b = workList.back();
        workList.pop_back();
       
        // Meet out[p] for all predecessors p of b, and store in in[b].
        BitVector newIn = config.top;
        for (auto it = pred_begin(b), et = pred_end(b); it != et; ++it) {
            const BasicBlock *P = *it;
            BitVector predBV = blockStates[P].out;
            predBV = (*(blockStates[b].phiTFs[P]))(predBV);
            newIn = config.meetWith(newIn, blockStates[P].out);
        }
        if (pred_begin(b) == pred_end(b)) {
            newIn = config.boundaryState;
        }
        blockStates[b].in = newIn;

        // oldOut = out[b];
        BitVector oldOut = blockStates[b].out;
        BitVector newOut = (*(blockStates[b].blockTF))(newIn);
        blockStates[b].out = newOut;

        // If out[b] changed, add all succesors of b to the work queue.
        if (newOut != oldOut) {
            for (auto it = succ_begin(b), et = succ_end(b); it != et; ++it) {
                workList.push_back(*it);
            }
        }
    }

    /*
    outs() << "\n----------------\n";
    for (auto it = blockStates.begin(), et = blockStates.end(); it != et; ++it){
        const BasicBlock *block = (*it).first;
        BlockState state = (*it).second;
        block->printAsOperand(outs());
        outs() << ":\n";
        outs() << "in: ";
        printBitVector(state.in);
        outs() << "\nout: ";
        printBitVector(state.out);
        outs() << "\n";
    }
    outs() << "----------------\n";
    */

    // Loop through the blocks to get a solution at every program point.
    DataMap *d = new DataMap();
    for (const BasicBlock& B : F) {
        BitVector bv = blockStates[&B].in;
      
        // We loop forward since this is forward analysis
        for (auto it = B.begin(), et = B.end(); it != et; ++it) {
            const Instruction *I = &*it;
            if (!isa<PHINode>(I)) {
                bv = (*(config.fnBuilder->makeInstTransferFn(I)))(bv);
                (*d)[I] = bv;
            }
        }
    }
    (*d)[boundary_point] = blockStates[&(F.back())].in;

    return d;
}

void printBitVector(const BitVector& bv) {
  for (int i = 0; i < bv.size(); i++) {
    outs() << (bv[i] ? "1" : "0"); 
  }
  outs() << "\n";
}



} // namespace dataflow
} // namespace llvm
