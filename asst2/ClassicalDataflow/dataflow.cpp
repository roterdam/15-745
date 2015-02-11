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
                  const FlowDirection dir, void (*printBV)(const BitVector&)) {
  printFunctionSignature(F);
  for (const BasicBlock& B : F) {
    B.printAsOperand(outs());
    outs() << ":\n";
    if (dir == FORWARD && &B == F.begin()) {
      outs() << "--> ";
      printBV(dataMap.lookup(boundary_point));
    }
    for (const Instruction& I : B) {
      if (dir == FORWARD) {
        I.print(outs());
        outs() << "\n";
      }
      if (!isa<PHINode>(&I)) {
        outs() << "--> ";
        printBV(dataMap.lookup(&I));
      }
      if (dir == BACKWARD) {
        I.print(outs());
        outs() << "\n";
      }
    }
  }
  if (dir == BACKWARD) {
    outs() << "--> ";
    printBV(dataMap.lookup(boundary_point));
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
        blockStates[&(F.getEntryBlock())].in = config.boundaryState;
    } else {
        for (const BasicBlock& B : F) {
            blockStates[&B].in = config.top;
        }
        // Here we assume that there is a single exit block.
        blockStates[&(F.back())].out = config.boundaryState;
    }
}


// TODO: try the backwards reverse postorder iterator from piazza.
DataMap *traverseBackwards(const Function& F, BlockStateMap& blockStates,
                           const DataflowConfiguration& config) {
    // Find a solution for the start of all the blocks.
    std::queue<const BasicBlock *>work_queue; 
    work_queue.push(&F.back());

    while (!work_queue.empty()) {
        const BasicBlock *b = work_queue.front();
        work_queue.pop();
       
        // Meet in[s] for all successors s of b, and store in out[b].
        BitVector newOut = config.top; 
        for (auto it = succ_begin(b), et = succ_end(b); it != et; ++it) {
             newOut = config.meetWith(newOut, blockStates[*it].in);
        }
        blockStates[b].out = newOut;

        // Set in[b] = f(out[b]).
        BitVector oldIn = blockStates[b].in;
        BitVector newIn = (*(config.fnBuilder->makeBlockTransferFn(b)))(newOut);
        blockStates[b].in = newIn;        

        // If in[b] changed, add all predecessors of b to the work queue.
        if (newIn != oldIn){
            for (auto it = pred_begin(b), et = pred_end(b); it != et; ++it) {
                work_queue.push(*it);
            }
        }
    }

    outs() << "----------------\n";
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


// TODO: try the reverse postorder iterator from piazza.
DataMap *traverseForwards(const Function& F, BlockStateMap& blockStates,
                          const DataflowConfiguration& config) {
    // Find a solution for the start of all the blocks.
    std::queue<const BasicBlock *>work_queue; 
    work_queue.push(&F.front());

    while (!work_queue.empty()) {
        const BasicBlock *b = work_queue.front();
        work_queue.pop();
       
        // Meet out[p] for all predecessors p of b, and store in in[b].
        BitVector newIn = config.top;
        for (auto it = pred_begin(b), et = pred_end(b); it != et; ++it) {
             newIn = config.meetWith(newIn, blockStates[*it].out);
        }
        blockStates[b].in = newIn;

        // Set out[b] = f(in[b]).
        BitVector oldOut = blockStates[b].out;
        BitVector newOut = (*(config.fnBuilder->makeBlockTransferFn(b)))(newIn);
        blockStates[b].out = newOut;

        // If out[b] changed, add all succesors of b to the work queue.
        if (newOut != oldOut) {
            for (auto it = succ_begin(b), et = succ_end(b); it != et; ++it) {
                work_queue.push(*it);
            }
        }
    }

    // Loop through the blocks to get a solution at every program point.
    DataMap *d = new DataMap();
    return d;
}

void printBitVector(const BitVector& bv) {
  for (int i = 0; i < bv.size(); i++) {
    outs() << (bv[i] ? "1" : "0"); 
  }
}



} // namespace dataflow
} // namespace llvm
