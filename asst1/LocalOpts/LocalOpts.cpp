// 15-745 S15 Assignment 1: LocalOpts.cpp
// Group: mchoquet, neilshah
////////////////////////////////////////////////////////////////////////////////

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include <ostream>
#include <fstream>
#include <iostream>

using namespace llvm;

namespace {

class LocalOpts : public ModulePass {

  // do some local optimizations on the module

  void doLocalOpts(Module& M) {
    outs() << "Module " << M.getModuleIdentifier().c_str() << "\n";

    int nAlgIdent = 0;
    int nConstFold = 0;
    int nStrRed = 0;

    bool progress = true;

    while(progress)
    {
      int prevAlgIdent = nAlgIdent;
      int prevConstFold = nConstFold;
      int prevStrRed = nStrRed;

      for(Module::iterator miter = M.begin(); miter != M.end(); ++miter)
      {
        for(Function::iterator fiter = miter->begin(); fiter != miter->end(); ++fiter)
        {
          for(BasicBlock::iterator biter = fiter->begin(); biter != fiter->end(); ++biter)
          {
            if(BinaryOperator* inst = dyn_cast<BinaryOperator>(&*biter))
            {
              inst->print(errs()); outs() << "\n";
		ConstantInt* x = dyn_cast<ConstantInt>(inst->getOperand(0));
              ConstantInt* y = dyn_cast<ConstantInt>(inst->getOperand(1));

              if(x && y)
              {
                //here we can constant fold
                const APInt& xval = x->getValue();
                const APInt& yval = y->getValue();
                ConstantInt* result = 0;

                // 3 types of constant folding (addition, subtraction, multiplication)
                if(inst->getOpcode() == Instruction::Add)
                  result = ConstantInt::get(x->getContext(), xval+yval);
                else if(inst->getOpcode() == Instruction::Sub)
                  result = ConstantInt::get(x->getContext(), xval-yval);
                else if(inst->getOpcode() == Instruction::Mul)
                  result = ConstantInt::get(x->getContext(), xval*yval);

                if(result)
                {
                  ReplaceInstWithValue(inst->getParent()->getInstList(), biter, result);
                  nConstFold++;
                }
              }

              else if (x || y)
              {
                ConstantInt* constval;
                Value* constval_v;
                Value* other_v;
                if(x)
                {
                  constval = x;
                  constval_v = inst->getOperand(0);
                  other_v = inst->getOperand(1);
                }
                else if(y)
                {
                  constval = y;
                  constval_v = inst->getOperand(1);
                  other_v = inst->getOperand(0);
                }
                // algebraic identity or strength reduction might be doable

                if(constval->isZero())
                {
                  if(inst->getOpcode() == Instruction::Add)
                  {             
                       // algebraic identity 1: x + 0 = x
                       ReplaceInstWithValue(inst->getParent()->getInstList(), biter, other_v);
                       nAlgIdent++; 
                  }
                  else if(inst->getOpcode() == Instruction::Mul)
                  {
                        //algebraic identity 2: x * 0 = 0
                        ReplaceInstWithValue(inst->getParent()->getInstList(), biter, constval_v);
                        nAlgIdent++;
                  }
                }

                if(inst->getOpcode() == Instruction::Mul)
                {
			//strength reduction 1: x * powerof2 = x shl log2(powerof2)
                  if(constval->getValue().isPowerOf2())
                  {
                    Value* nshift_v = ConstantInt::get(constval->getType(), log2(constval->getSExtValue()));
                    ReplaceInstWithInst(inst->getParent()->getInstList(), biter, BinaryOperator::Create(Instruction::Shl, other_v, nshift_v));
			nStrRed++;
                  }
                } 

                if(y && (inst->getOpcode() == Instruction::UDiv || inst->getOpcode() == Instruction::SDiv))
                {
			//strength reduction 2: x / powerof2 = x lshr log2(powerof2)
                    if(constval->getValue().isPowerOf2())
                    {
                      Value* nshift_v = ConstantInt::get(constval->getType(), log2(constval->getSExtValue()));
                      ReplaceInstWithInst(inst->getParent()->getInstList(), biter, BinaryOperator::Create(Instruction::LShr, other_v, nshift_v));
			nStrRed++;
                    }
                }
              }
              
            }
          }


        }
      }

      progress = (nAlgIdent != prevAlgIdent) || (nStrRed != prevStrRed) || (nConstFold != prevConstFold);
	outs() << "\n\n";
    }

    outs() << "Transformations applied:" << "\n" << "\tAlgebraic identities: " << nAlgIdent << "\n" << "\tConstant folding: " << nConstFold << "\n" << "\tStrength reduction: " << nStrRed << "\n";

  }

public:

  static char ID;

  LocalOpts() : ModulePass(ID) { }

  ~LocalOpts() { }

  virtual bool runOnModule(Module& M) {
    doLocalOpts(M);
    return false;
  }

};

// LLVM uses the address of this static member to identify the pass, so the
// initialization value is unimportant.
char LocalOpts::ID = 0;
RegisterPass<LocalOpts> X("some-local-opts", "15745: Some local optimizations");

}
