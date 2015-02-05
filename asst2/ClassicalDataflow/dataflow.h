// 15-745 S15 Assignment 2: dataflow.h
// Group: mchoquet, nshah, rokhinip
////////////////////////////////////////////////////////////////////////////////

#ifndef __CLASSICAL_DATAFLOW_DATAFLOW_H__
#define __CLASSICAL_DATAFLOW_DATAFLOW_H__

// Some useful libraries. Change as you see fit
#include <stdio.h>
#include <iostream>
#include <queue>
#include <type_traits>
#include <vector>
#include "llvm/IR/Instructions.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/ValueMap.h"
#include "llvm/IR/CFG.h"

namespace llvm {
namespace dataflow {

// To get type-safety, transfer functions are required to subclass this
// interface. Their class definitions will look like:
// class MyTF : public TransferFunction<MyTF> {
//  public:
//    MyTF(const Instruction *inst) {
//      ...
//    }
//    ...
// };
template<typename TFunc>
class TransferFunction {
 public:
  ~TransferFunction() { }
  virtual void compose(const TFunc& t) = 0;
  virtual BitVector operator()(const BitVector&) const = 0;
};

enum class FlowDirection { FORWARD, BACKWARD };

typedef BitVector& (*MeetFunction)(const BitVector&, const BitVector&);
typedef DenseMap<Instruction *, BitVector *> DataMap;

// A collection of all the problem-specific arguments.
template<typename TFunc>
struct DataflowParameters {
  static_assert(std::is_constructible<TFunc, const Instruction *>::value,
                "TFunc should have a (const Instruction *) constructor");
  static_assert(std::is_base_of<TransferFunction<TFunc>, TFunc>::value,
                "TFunc should implement \"TransferFunction<TFunc>\"");

  FlowDirection dir;
  MeetFunction meetWith;
  BitVector top;
  BitVector boundary_state;
};


template<typename TFunc>
DataMap *dataflow(const Function& code,
                  const DataflowParameters<TFunc>& configParams) {
  // In here, transfer functions can be created with "new TFunc(instPtr)".
  return new DataMap();
}

} // namespace dataflow
} // namespace llvm

#endif
