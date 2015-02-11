// 15-745 S15 Assignment 2: available.cpp
// Group: mchoquet, rokhinip 
////////////////////////////////////////////////////////////////////////////////

#include "llvm/IR/Function.h"
#include "llvm/Pass.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"

#include "dataflow.h"
#include "available-support.h"

#include <set>

using namespace llvm;
using namespace std;

namespace {  

struct TranslationMaps {
  // Expression to bit vector index
  DenseMap<const Expression, int> *bitMap;  
  // Bit vector index to expression
  DenseMap<int, const Expression> *expMap;  
  // Value to set of expressions using that value
  DenseMap<const Value *, std::set<Expression>> *varExpMap;  
};

static TranslationMaps getOperandMaps(const Function&);
static vector<const Expression> getExpsUsedInFunction(const Function&);
static bool isTracked(const Expression);
static void printAvailExpSet(const BitVector&);

void printAvailExpSet(const BitVector& bv){
    outs() << "{";
    for (int i = 0; i < bv.size(); i++) {
        outs() << (bv[i] ? "1" : "0");
    }
    outs() << "}\n";
}

class AvailExpTransferFunction : public dataflow::TransferFunction {

    public:
     ~AvailExpTransferFunction(){ }
     AvailExpTransferFunction(const BitVector &genBV, const BitVector& killBV):
        _genBV(genBV), _killBV(killBV) { }

     BitVector& operator()(BitVector& bv) const {
        // out = gen union (in - kill)
        return (bv.reset(_killBV)) |= _genBV;
     }
    private:
        const BitVector _genBV;
        const BitVector _killBV;
};

class AvailExpTFBuilder : public dataflow::TransferFunctionBuilder {

    public:
     ~AvailExpTFBuilder(){ }
     AvailExpTFBuilder(const DenseMap<Expression, int>& bitMap, 
                       const DenseMap<Value *, std::set<Expression>>& varExpMap):
        _bitMap(bitMap), _varExpMap(varExpMap), _n(bitMap.size()) { }

     dataflow::TransferFunction *makeInstTransferFn(
                    const Instruction *I) const {

        BitVector genBV(_n, false); 
        BitVector killBV(_n, false); 
       
        // Build genBV, only for binary operator expressions
        if (BinaryOperator *BO = dyn_cast<const BinaryOperator>(I)){
            Expression exp = Expression(BO);
            if (_bitMap.count(exp) != 0){ 
                genBV.set(_bitMap.lookup(exp)); // Set that exp as being generated
            }
        }

        // Build killBV
        if (_varExpMap.count(I) != 0) {  
            std::set<Expression> expsKilled = _varExpMap.lookup(I);  
            for (auto it = expsKilled.begin(); it != expsKilled.end(); ++it){
                Expression exp = *it;
                if (_bitMap.count(exp) != 0){ // Expression is in bit vector
                    killBV.set(_bitMap.lookup(exp));
                }
            }
        }

        return new AvailExpTransferFunction(genBV, killBV);
     }

     dataflow::TransferFunction *makeBlockTransferFn(
                    const BasicBlock *B) const {
        BitVector genBV(_n, false); 
        BitVector killBV(_n, false); 
       
        for (auto it = B->rbegin(), et = B->rend(); it != et; ++it){
            Instruction *I = &(*it);

            // x = y op z
            if (BinaryOperator *BO = dyn_cast<BinaryOperator>(I)){
                Expression exp = Expression(BO);  // y op z
                if (_bitMap.count(exp) != 0){ 
                    const int i = _bitMap.lookup(exp);
                    genBV.set(i);   // y op z is generated
                    killBV.reset(i);// Since y op z is generated, it is no longer killed 
                                    // since it has been recomputed
                }
            }
            
            if (_varExpMap.count(I) != 0) {  
                std::set<Expression> expsKilled = _varExpMap.lookup(I);   // Expressions containing x
                for (auto it = expsKilled.begin(); it != expsKilled.end(); ++it){
                    Expression exp = *it;
                    if (_bitMap.count(exp) != 0){ 
                        killBV.set(_bitMap.lookup(exp));
                    }
                }
            }
        }

        return new AvailExpTransferFunction(genBV, killBV);
     }

    private:
        const DenseMap<Expression, int>& _bitMap;
        int _n;
        const DenseMap<Value *, std::set<Expression>>& _varExpMap;
};



class AvailableExpressions : public FunctionPass {
    
  public:
    static char ID;
    
    AvailableExpressions() : FunctionPass(ID) { }

    vector<Expression> getExpsUsedInFunction(const Function& F){

        vector<Expression> expressions;
        for (Function::iterator FI = F.begin(), FE = F.end(); FI != FE; ++FI) {
           BasicBlock* block = FI;      
           for (BasicBlock::iterator i = block->begin(), e = block->end(); i!=e; ++i) {
               Instruction * I = i;
               // We only care about available expressions for BinaryOperators
               if (BinaryOperator * BI = dyn_cast<BinaryOperator>(I)) {
                   expressions.push_back(Expression(BI));
               }
           }	  
        }

        // Sort and remove duplicates
        std::sort(expressions.begin(), expressions.end());
        auto newEnd = std::unique(expressions.begin(), expressions.end());
        int newSize = newEnd - expressions.begin();
        expressions.resize(newSize);

        return expressions;
    }

    TranslationMaps getOperandMaps(Function& F){
        
        DenseMap<const Expression, int> *bitMap = new DenseMap<const Expression, int>;
        DenseMap<int, const Expression> *expMap = new DenseMap<int, const Expression>;
        DenseMap<const Value *, std::set<Expression>> *varExpMap = 
                new DenseMap<const Value *, std::set<Expression>>;
                                                                        
        int current = 0; // current index into bitvector

        for (Expression& e: getExpsUsedInFunction(F)){
            if (bitMap->count(e) == 0) {
                // Update mapping of bit vector index to expression
                (*bitMap)[e] = current;
                (*expMap)[current] = e;
                current++;

                // Update mapping of var to expressions
                Value *v1 = e.v1;
                Value *v2 = e.v2;
                (*varExpMap)[v1].insert(e);
                (*varExpMap)[v2].insert(e);
            }
        }
        
        TranslationMaps retVal;
        retVal.bitMap = bitMap;
        retVal.expMap = expMap;
        retVal.varExpMap = varExpMap;
        return retVal;
    }

    virtual bool runOnFunction(Function& F) {      
            
        /* 1. Compute three maps:
         *       bit index --> expression
         *       expression --> bit index
         *       values --> expressions containing those values
         * 2. Set up the configuration 
         * 3. Call dataflow on it
         * 4. Print the data map
         */
        TranslationMaps maps = getOperandMaps(F);
        const DenseMap<const Expression, int> *bitMap = maps.bitMap;
        const DenseMap<int, const Expression> *expMap = maps.expMap;
        const DenseMap<const Value *, std::set<Expression>> *varExpMap = maps.varExpMap;
        
        dataflow::DataflowConfiguration config;
        config.dir = dataflow::FlowDirection::FORWARD;
        config.fnBuilder = new AvailExpTFBuilder(*bitMap, *varExpMap);
        config.meetWith = dataflow::bvIntersect;
        config.top = dataflow::onesVector(bitMap->size());
        config.boundaryState = dataflow::zerosVector(bitMap->size());

        dataflow::DataMap *out = dataflow::dataflow(F, config); 
        dataflow::printDataMap(F, *out, config.dir, printAvailExpSet);
        outs() << "\n\n";

        delete bitMap;
        delete expMap;
        delete varExpMap;

        delete config.fnBuilder;
        delete out;

        // Did not modify the incoming Function.
        return false;
    }

    virtual void getAnalysisUsage(AnalysisUsage& AU) const {   
      AU.setPreservesAll();
    }
    
  private:
  };
  
  char AvailableExpressions::ID = 0;
  RegisterPass<AvailableExpressions> X("available",
				      "15745 Available Expressions");
  
}
