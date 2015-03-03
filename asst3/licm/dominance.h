#ifndef __DOMINANCE_H_
#define __DOMINANCE_H_

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include <utility>
#include <vector>

using namespace llvm;
using std::make_pair;
using std::min;
using std::pair;
using std::vector;


/* Represents the dominance tree for a program, by mapping each block to its
 * nearest dominating ancestor. Blocks with no parents get NULL. */
typedef DenseMap<BasicBlock *, BasicBlock *> DTree;
typedef DenseSet<BasicBlock *> BlockSet;
typedef DenseSet<pair<BasicBlock *, BasicBlock *>> BackEdgeSet;

DTree *buildDominanceTree(Function&);
void printDominanceTree(const DTree&);

#endif /* __DOMINANCE_H_ */
