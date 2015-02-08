// 15-745 S15 Assignment 2: dataflow.cpp
// Group: mchoquet, nshah, rokhinip
////////////////////////////////////////////////////////////////////////////////

#include "dataflow_fallback_framework.h"

namespace llvm {

namespace dataflow {


DataFlowPass::DataFlowPass(char id, DataflowConfiguration& config):
    FunctionPass(id),
    _dir(config.dir),
    _fnBuilder(config.fnBuilder),
    _meetWith(config.meetWith),
    _top(config.top),
    _boundaryState(config.boundaryState) {};

void displayBlockStates(BlockStates& BS){
    // Pretty printing code
}

void DataFlowPass::traverseBackwards(Function &F, BlockStates& blockStates){
    
    std::queue<BasicBlock *>work_queue; 
    work_queue.push(&F.back());

    while (!work_queue.empty()){
        BasicBlock *block = work_queue.front();
        work_queue.pop();
       
        // out[b] = meet op (in[p] for all predecessors p)
        BitVector initMeet = _top; 
        for (auto it = succ_begin(block), et = succ_end(block);
             it != et; ++it){
             BasicBlock *p = *it;
             initMeet = _meetWith(blockStates[p].in, initMeet);
        }
        blockStates[block].out = initMeet;

        // oldin = in[b]
        BitVector& oldin = blockStates[block].in;

        // in[b] = f_b(out[b])
        TransferFunction *fn = _fnBuilder->makeBlockTransferFn(block);
        BitVector& newin = (*fn)(blockStates[block].out);

        // if (oldin != in[b]) 
        //      for all predecessors p of b, add p to work queue
        if (newin != oldin){

            for (auto it = pred_begin(block), et = pred_begin(block);
                 it != et; ++it){
                BasicBlock *p = *it;
                work_queue.push(p);
            }
        }
    }
}

// Generalized from the worklist algorithm used 
// for reaching defintions (from Lecture 4)
void DataFlowPass::traverseForwards(Function &F, BlockStates& blockStates){

    std::queue<BasicBlock *>work_queue; 
    work_queue.push(&F.front());

    while (!work_queue.empty()){
        BasicBlock *block = work_queue.front();
        work_queue.pop();
       
        // in[b] = meet op (out[p] for all predecessors p)
        BitVector initMeet = _top; 
        for (auto it = pred_begin(block), et = pred_end(block);
             it != et; ++it){
             BasicBlock *p = *it;
             initMeet = _meetWith(blockStates[p].out, initMeet);
        }
        blockStates[block].in = initMeet;

        // oldout = out[b]
        BitVector& oldout = blockStates[block].out;

        // out[b] = f_b(in[b])
        TransferFunction *fn = _fnBuilder->makeBlockTransferFn(block);
        BitVector& newout = (*fn)(blockStates[block].in);

        // if (oldout != out[b]) 
        //      for all successors s of b, add s to work queue
        if (oldout != newout){

            for (auto it = succ_begin(block), et = succ_end(block);
                 it != et; ++it){
                BasicBlock *s = *it;
                work_queue.push(s);
            }
        }
    }
}

void DataFlowPass::initializeBasicBlocks(Function &F, BlockStates &blockStates){
    for (BasicBlock& B: F){
        if (_dir == FORWARD){
            // We use the name of the basic block since it is 
            // a unique identifier in a function
            if (B.getName() == F.front().getName()){
                blockStates[&B].out = _boundaryState; 
            } else {
                blockStates[&B].out = _top;
            }
        } else { 
            // We use the name of the basic block since it is 
            // a unique identifier in a function
            if (B.getName() == F.back().getName()){
                blockStates[&B].in = _boundaryState;
            } else {
                blockStates[&B].in = _top;
            }
        }
    }
}

bool DataFlowPass::runOnFunction(Function &F){
    
    /* 1. Build CFG (done by virtue of LLVM)
     * 2. For each basic block, depending on the 
     *    direction, initialize the state of the block
     *    if _dir == BACKWARD:
     *      in[exit] = boundaryState;
     *      in[b] = top forall BasicBlock b
     * 3. Loop through graph applying transfer and meet 
     *    functions until convergence
     */
    
    BlockStates BS;
    initializeBasicBlocks(F, BS);

    if (_dir == FORWARD){
        traverseForwards(F, BS);
    } else {
        traverseBackwards(F, BS);
    }
    
    outs() << F.getName() << "\n";
    displayBlockStates(BS);
    return true;
}


} // namespace dataflow
} // namespace llvm
