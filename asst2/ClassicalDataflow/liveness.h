// 15-745 S15 Assignment 2: liveness.h
// Group: mchoquet, nshah, rokhinip
////////////////////////////////////////////////////////////////////////////////

#ifndef __LIVENESS_H__
#define __LIVENESS_H__

#include "dataflow_fallback_framework.h"

namespace llvm {

namespace dataflow {
    class Liveness: public DataFlowPass {
        public:
            static char ID;
            BitVector top;
            BitVector boundaryState;
            Liveness();

            BitVector& meetWith(const BitVector& a, 
                                const BitVector& b);
            TransferFunctionBuilder *transferFn;
    };

}
}


#endif /* __LIVENESS_H_ */
