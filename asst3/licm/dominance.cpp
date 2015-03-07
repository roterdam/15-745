#include "dominance.h"


static void visitBE(BasicBlock&, BlockSet&, BlockSet&, BackEdgeSet&);
static BackEdgeSet findBackEdges(Function&);
static BasicBlock *findCommonLDA(const vector<BasicBlock *>&, const DTree *);
static void computeLDA(BasicBlock&, const BackEdgeSet&, DTree *);

/*
  Internal DFS function used by findBackEdges.
  ASSUMES: B has never been visited before.
*/
void visitBE(BasicBlock& B, BlockSet& seenSet, BlockSet& pathSet,
             BackEdgeSet& backEdges) {
  seenSet.insert(&B);
  pathSet.insert(&B);
  for (auto it = succ_begin(&B), et = succ_end(&B); it != et; ++it) {
    BasicBlock& childB = **it;
    if (pathSet.count(&childB) == 1) {
      backEdges.insert(make_pair(&B, &childB));
    } else if (seenSet.count(&childB) == 0) {
      visitBE(childB, seenSet, pathSet, backEdges);
    }
  }
  pathSet.erase(&B);
}


/*
  Use a forwards DFS and track the current path.
*/
BackEdgeSet findBackEdges(Function& F) {
  BackEdgeSet backEdges;
  BlockSet seenSet;
  BlockSet pathSet;

  visitBE(F.getEntryBlock(), seenSet, pathSet, backEdges);
  
  return backEdges;
}


BasicBlock *findCommonLDA(const vector<BasicBlock *>& blocks,
                          const DTree *dTree) {
  int nBlocks = blocks.size();

  if (nBlocks == 0) {
    return nullptr;
  }
  if (nBlocks == 1) {
    return blocks[0];
  }
  
  vector<BasicBlock *> dLists[nBlocks];
  for (int i = 0; i < nBlocks; i++) {
    BasicBlock *B = blocks[i];
    do {
      dLists[i].push_back(B);
      B = dTree->lookup(B);
    } while (B != nullptr);
    std::reverse(dLists[i].begin(), dLists[i].end());
  }

  unsigned int minSize = dLists[0].size();
  for (int i = 1; i < nBlocks; i++) {
    minSize = min(minSize, dLists[i].size());
  }

  int j = 0;
  while (j < minSize) {
    bool allEqual = true;
    for (int i = 1; i < nBlocks; i++) {
      if (dLists[i][j] != dLists[0][j]) {
        allEqual = false;
        break;
      }
    }
    if (!allEqual) {
      break;
    }
    j++;
  }
  return dLists[0][j - 1];
}


/*
  Internal function used by buildDominanceTree.
  ASSUMES: any <block, lda> pair in dTree is correct & finalized.
*/
void computeLDA(BasicBlock& B, const BackEdgeSet& backEdges, DTree *dTree) {
  if (dTree->count(&B) != 0) {
    return;
  }

  vector<BasicBlock *> predBlocks;
  for (auto it = pred_begin(&B), et = pred_end(&B); it != et; ++it) {
    BasicBlock& P = **it;
    if (backEdges.count(make_pair(&P, &B)) == 0) {
      computeLDA(P, backEdges, dTree);
      predBlocks.push_back(&P);
    }
  }

  (*dTree)[&B] = findCommonLDA(predBlocks, dTree);
  return;
}


/*
  Builds a dominance tree in two steps:
    1. Find back-edges.
    2. Compute dominance recursively, using (1) to ignore back-edges.
*/
DTree *buildDominanceTree(Function& F) {
  BackEdgeSet backEdges = findBackEdges(F);
  DTree *dTree = new DTree();

  for (BasicBlock& B : F) {
    computeLDA(B, backEdges, dTree);
  }

  return dTree;
}

void printDominanceTree(const DTree& dTree) {
  outs() << "dominance tree is:\n";
  for (auto it = dTree.begin(), et = dTree.end(); it != et; ++it) {
    BasicBlock *p1 = it->first;
    BasicBlock *p2 = it->second;
    outs() << "  " << (p1 != nullptr ? p1->getName() : "NULL")
           << " --> " << (p2 != nullptr ? p2->getName() : "NULL") << "\n";
  }
}
