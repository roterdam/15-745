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
  map<Expression, int> *bitMap;  
  // Bit vector index to expression
  DenseMap<int, Expression> *expMap;  
  // Value to set of expressions using that value
  DenseMap<const Value *, std::set<Expression>> *varExpMap;  
};

static TranslationMaps getOperandMaps(const Function&);
static vector<const Expression> getExpsUsedInFunction(const Function&);

class AvailExpTransferFunction : public dataflow::TransferFunction {

    public:
     ~AvailExpTransferFunction(){ }
     AvailExpTransferFunction(const BitVector &genBV, const BitVector& killBV):
        _genBV(genBV), _killBV(killBV) { }

     BitVector& operator()(BitVector& in) const {
        // out = gen union (in - kill)
        return (in.reset(_killBV) |= _genBV);
     }
    private:
        const BitVector _genBV;
        const BitVector _killBV;
};

class AvailExpTFBuilder : public dataflow::TransferFunctionBuilder {

   public:
    ~AvailExpTFBuilder(){ }
    AvailExpTFBuilder(const map<Expression, int>& bitMap, 
                      const DenseMap<const Value *, std::set<Expression>>& varExpMap):
       _bitMap(bitMap), _varExpMap(varExpMap), _n(bitMap.size()) { }

    dataflow::TransferFunction *makeInstTransferFn(
                    const Instruction *I) const {

        BitVector genBV(_n, false); 
        BitVector killBV(_n, false); 

        // Build genBV, only for binary operator expressions
        if (const BinaryOperator *BO = dyn_cast<const BinaryOperator>(I)){
            Expression exp = Expression(BO);
            if (_bitMap.count(exp) != 0){ 
                genBV.set((*(_bitMap.find(exp))).second); // Set that exp as being generated
            }
        }

        // Build killBV
        if (_varExpMap.count(I) != 0) {  
            std::set<Expression> expsKilled = _varExpMap.lookup(I);  
            for (auto it = expsKilled.begin(); it != expsKilled.end(); ++it){
                Expression exp = *it;
                if (_bitMap.count(exp) != 0){ // Expression is in bit vector
                    killBV.set((*(_bitMap.find(exp))).second);
                }
            }
        }

        return new AvailExpTransferFunction(genBV, killBV);
    }

    dataflow::TransferFunction *makePhiSeqTransferFn(
        const vector<const PHINode *>& phis, const BasicBlock *B) const {
        BitVector genBV(_n, false);
        BitVector killBV(_n, false);

        for (const PHINode *phi : phis) { 
            Value *phiArg = phi->getIncomingValueForBlock(B);
            
            if (_varExpMap.count(phiArg) != 0) {
                std::set<Expression> expsKilled = _varExpMap.lookup(phiArg);   
                for (const Expression& exp : expsKilled) {
                    if (_bitMap.count(exp) != 0){ 
                        killBV.set((*(_bitMap.find(exp))).second);
                    }
                }
            }
        }

        return new AvailExpTransferFunction(genBV, killBV);
    }


     dataflow::TransferFunction *makeInstSeqTransferFn(
                    const vector<const Instruction *>& insts) const {
        BitVector genBV(_n, false); 
        BitVector killBV(_n, false); 

        // Loop forward since this is a forward analysis
        for (const Instruction *I : insts) {

            // x = y op z
            if (const BinaryOperator *BO = dyn_cast<const BinaryOperator>(I)){
                Expression exp = Expression(BO);  // y op z
                if (_bitMap.count(exp) != 0){ 
                    const int i = (*(_bitMap.find(exp))).second;
                    genBV.set(i);   // y op z is generated
                    killBV.reset(i);// Since y op z is generated, it is no longer killed 
                                    // since it has been recomputed. 
                }
            }
            
            if (_varExpMap.count(I) != 0) {
                std::set<Expression> expsKilled = _varExpMap.lookup(I);   // Expressions containing x
                for (const Expression& exp : expsKilled) {
                    if (_bitMap.count(exp) != 0){ 
                        killBV.set((*(_bitMap.find(exp))).second);
                    }
                }
            }
        }
        
        return new AvailExpTransferFunction(genBV, killBV);
     }

    private:
        const map<Expression, int>& _bitMap;
        int _n;
        const DenseMap<const Value *, std::set<Expression>>& _varExpMap;
};

class AvailableExprBitVectorPrinter : public dataflow::BitVectorPrinter {
    public:
        AvailableExprBitVectorPrinter(const DenseMap<int, Expression>& expMap):
            _expMap(expMap) { }
    
        void print(const BitVector& bv) const {
            outs() << "{";
            bool firstItem = true;
            for (int i = 0; i < bv.size(); i++) {
                if (bv.test(i)) {
                    if (!firstItem) {
                        outs() << ", ";
                    }
                    outs() << (_expMap.lookup(i)).toString();
                    firstItem = false;
                }
            }
            outs() << "}";
        }

    private:
        const DenseMap<int, Expression>& _expMap;
};

class AvailableExpressions : public FunctionPass {
    
  public:
    static char ID;
    
    AvailableExpressions() : FunctionPass(ID) { }

    vector<Expression> getExpsUsedInFunction(const Function& F){

        vector<Expression> expressions;
        for (const BasicBlock& B : F) {
            for (const Instruction& I : B) {
               // We only care about available expressions for BinaryOperators
               if (const BinaryOperator *BI = dyn_cast<const BinaryOperator>(&I)) {
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
        
        map<Expression, int> *bitMap = new map<Expression, int>;
        DenseMap<int, Expression> *expMap = new DenseMap<int, Expression>;
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
        const map<Expression, int> *bitMap = maps.bitMap;

        for (auto it = bitMap->begin(), et = bitMap->end(); it != et; ++it){
            outs() << it->first.toString() << " --> " << it->second << "\n";
        }

        outs() << "\n\n";

        const DenseMap<int, Expression> *expMap = maps.expMap;
        const DenseMap<const Value *, std::set<Expression>> *varExpMap = maps.varExpMap;

        dataflow::DataflowConfiguration config;
        config.dir = dataflow::FlowDirection::FORWARD;
        config.fnBuilder = new AvailExpTFBuilder(*bitMap, *varExpMap);
        config.meetWith = dataflow::bvIntersect;
        config.top = dataflow::onesVector(maps.bitMap->size());
        config.boundaryState = dataflow::zerosVector(maps.bitMap->size());

        dataflow::DataMap *out = dataflow::dataflow(F, config); 

        const dataflow::BitVectorPrinter *printer = new AvailableExprBitVectorPrinter(*expMap);
        dataflow::printDataMap(F, *out, config.dir, printer);
        outs() << "\n\n";

        delete bitMap;
        delete expMap;
        delete varExpMap;

        delete config.fnBuilder;
        delete printer;
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
