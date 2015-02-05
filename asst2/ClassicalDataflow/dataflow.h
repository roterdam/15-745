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
  // "t1.thenCall(t2)(b)" should be equivalent to "t2(t1(b))", aka it's function
  // composition with arguments in reverse order.
  virtual TFunc& thenCall(const TFunc& t) = 0;
  // t1(b) is allowed to modify and return its input argument b.
  virtual BitVector& operator()(BitVector&) const = 0;
};

enum class FlowDirection { FORWARD, BACKWARD };

// The meet funciton is expected to modify and return its first argument.
typedef BitVector& (*MeetFunction)(BitVector&, const BitVector&);

// The output of the dataflow program. "all program points" is implemented as a
// map from instructions to the value at the corresponding program point, plus a
// single special value that corresponds to the boundary condition program
// point.
// e.g. For forwards dataflow, map[inst] is the value just before inst, and
//      map[boundary_point] is the value at the very end of the program.
//      For backwards dataflow, this is reversed.
typedef DenseMap<Instruction *, BitVector *> DataMap;
static const Instruction *boundary_point = nullptr;

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
  // Our implementation of Dataflow goes here.

  // NOTE: in here, transfer functions can be created with "new TFunc(instPtr)".
  return new DataMap();
}

} // namespace dataflow
} // namespace llvm

#endif
