// 15-745 S15 Assignment 2: dataflow_fallback_framework.h
// Group: mchoquet, nshah, rokhinip
////////////////////////////////////////////////////////////////////////////////

#ifndef __CLASSICAL_DATAFLOW_DATAFLOW_H__
#define __CLASSICAL_DATAFLOW_DATAFLOW_H__

// Some useful libraries. Change as you see fit
#include <stdio.h>
#include <iostream>
#include <ostream>
#include <fstream>
#include <queue>
#include <type_traits>
#include <vector>
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/ValueMap.h"
#include "llvm/IR/CFG.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {

namespace dataflow {

// Information that the client needs to provider to 
// the datafalow framework
class TransferFunction {
 public:
  ~TransferFunction() { }
  virtual BitVector& operator()(const BitVector&) const = 0;
};

class TransferFunctionBuilder {
 public:
  ~TransferFunctionBuilder() { }
  virtual TransferFunction *makeInstTransferFn(const Instruction *) const = 0;
  virtual TransferFunction *makeBlockTransferFn(const BasicBlock *) const = 0;
};

typedef BitVector& (*MeetFunction)(const BitVector&, const BitVector&);
typedef DenseMap<Instruction *, BitVector *> DataMap;
enum FlowDirection { FORWARD, BACKWARD };

struct DataflowConfiguration {
  FlowDirection dir;
  TransferFunctionBuilder *fnBuilder;
  MeetFunction meetWith;
  BitVector top;
  BitVector boundaryState;
};

// These are defined for the use of internal representation
// in the dataflow pass
class BlockState {
public:
    BitVector in;
    BitVector out;
};
typedef DenseMap<BasicBlock *, BlockState> BlockStates; 


// Liveness and AvailableExpression will be subclasses of this 
// and will be registered as passes
class DataFlowPass: public FunctionPass {
    public:
        DataFlowPass(char id, DataflowConfiguration& config);
        virtual bool runOnFunction(Function &F);
        virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    private:
        void initializeBasicBlocks(Function &F, BlockStates &BS);
        void traverseForwards(Function &F, BlockStates &BS);
        void traverseBackwards(Function &F, BlockStates &BS);
        void displayBlockStates(BlockStates &BS);
    protected:
        const FlowDirection _dir;
        const TransferFunctionBuilder *_fnBuilder;
        const MeetFunction _meetWith;
        const BitVector _top;
        const BitVector _boundaryState;
};



} // namespace dataflow
} // namespace llvm
#endif 
