// 15-745 S15 Assignment 2: dataflow.h
// Group: mchoquet, nshah, rokhinip
////////////////////////////////////////////////////////////////////////////////

#ifndef __CLASSICAL_DATAFLOW_DATAFLOW_H__
#define __CLASSICAL_DATAFLOW_DATAFLOW_H__

#include "llvm/IR/Instructions.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/CFG.h"

#include <vector>

using std::vector;

namespace llvm {
namespace dataflow {

// Transfer functions are required to subclass this interface.
class TransferFunction {
 public:
  ~TransferFunction() { }
  // Is allowed to modify and return the input vector.
  virtual BitVector& operator()(BitVector&) const = 0;
};

// An interface for transfer function factories.
class TransferFunctionBuilder {
 public:
  ~TransferFunctionBuilder() { }
  // Will be called on non-PHI instructions.
  virtual TransferFunction *makeInstTransferFn(const Instruction *) const = 0;
  // Will be called on PHI instructions, specialized once for each block the phi
  // instruction references.
  virtual TransferFunction *makePhiSeqTransferFn(const vector<const PHINode *>&,
                                                 const BasicBlock *) const = 0;
  // Should be equivalent to composing all the instructions in the given list.
  // Note that phi nodes are disallowed, just as in makeInstTransferFn.
  virtual TransferFunction *makeInstSeqTransferFn(
    const vector<const Instruction*>&) const = 0;
};

// Interface used in the printDataMap function.
class BitVectorPrinter {
 public:
  ~BitVectorPrinter() { }
  virtual void print(const BitVector&) const = 0;
};

// Is allowed to modify and return its first argument.
typedef BitVector& (*MeetFunction)(BitVector&, const BitVector&);
enum FlowDirection { FORWARD, BACKWARD };

// The input parameters to dataflow() that vary from problem to problem.
struct DataflowConfiguration {
  FlowDirection dir;
  TransferFunctionBuilder *fnBuilder;
  MeetFunction meetWith;
  BitVector top;
  BitVector boundaryState;
};

// The output of the dataflow program. "all program points" is implemented as a
// map from instructions to the value at the corresponding program point, plus a
// single special value that corresponds to the boundary condition program
// point.
// e.g. For forwards dataflow, map[inst] is the value just after inst, and
//      map[boundary_point] is the value at the very start of the program.
//      For backwards dataflow, this is reversed.
typedef DenseMap<const Instruction *, BitVector> DataMap;
static const Instruction *boundary_point = nullptr;


// Dataflow functions
DataMap *dataflow(const Function& F, const DataflowConfiguration& config);
void printDataMap(const Function& F, const DataMap& dataMap,
                  const FlowDirection dir, const BitVectorPrinter *printer);
const BitVector onesVector(const unsigned int n);
const BitVector zerosVector(const unsigned int n);
BitVector& bvIntersect(BitVector& v1, const BitVector& v2);
BitVector& bvUnion(BitVector& v1, const BitVector& v2);

} // namespace dataflow
} // namespace llvm

#endif
