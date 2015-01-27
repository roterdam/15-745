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
 private:
  
  /*
    If the input binary operator instruction can be constant folded to a number, do so and
    return the result. Otherwise, return null.
  */
  ConstantInt *tryConstantFold(BinaryOperator *inst) {
    ConstantInt *lPtr = dyn_cast<ConstantInt>(inst->getOperand(0));
    ConstantInt *rPtr = dyn_cast<ConstantInt>(inst->getOperand(1));
    LLVMContext& ctx = inst->getContext();
    if (lPtr && rPtr) {
      const APInt& l = lPtr->getValue();
      const APInt& r = rPtr->getValue();
      switch (inst->getOpcode()) {
        case (Instruction::Add):
          return ConstantInt::get(ctx, l + r);
        case (Instruction::Sub):
          return ConstantInt::get(ctx, l - r);
        case (Instruction::Mul):
          return ConstantInt::get(ctx, l * r);
        case (Instruction::SDiv):
          return ConstantInt::get(ctx, l.sdiv(r));
        case (Instruction::UDiv):
          return ConstantInt::get(ctx, l.udiv(r));
        default:
          return nullptr;
      }
    } else {
      return nullptr;
    }
  }

  /* If the operator is commutative, orders the operands such that constants prefer to be in
   * arg2 and non-constants prefer arg1. This reduces the number of cases we have to consider
   * in the other functions.
   * If the operator is non-commutative, it does nothing. */
  void orderOperands(const unsigned opCode, Value **arg1, Value **arg2) {
    if ((opCode == Instruction::Add || opCode == Instruction::Mul) &&
        isa<ConstantInt>(*arg1)) {
      std::swap(*arg1, *arg2);
    }
  }

  /*
    If the input binary operator instruction can be simplified using an algebraic identity
    (i.e. x + 0 == x,  y - y == 0), return the resulting value.
    NOTE: the handout for this hw mentions that we don't have to worry about divide-by-zero
          issues.

    Example cases:
    x + 0 == x
    x - 0 == x
    x - x == 0
    x * 0 == 0
    x * 1 == x
    x / x == 1
    x / 1 == x
    0 / x == 0 
  */
  Value *tryAlgebraicIdentity(BinaryOperator *inst) {
    const unsigned opCode = inst->getOpcode();
    Value *l = inst->getOperand(0);
    Value *r = inst->getOperand(1);
    orderOperands(opCode, &l, &r);
    ConstantInt *lAsInt = dyn_cast<ConstantInt>(l);
    ConstantInt *rAsInt = dyn_cast<ConstantInt>(r);
    switch (opCode) {
      case (Instruction::Add):
        if (rAsInt && rAsInt->isZero()) {
          return l;
        }
        break;
      case (Instruction::Sub):
        if (rAsInt && rAsInt->isZero()) {
          return l;
        } else if (l == r) {
          return ConstantInt::get(l->getType(), 0);
        }
        break;
      case (Instruction::Mul):
        if (rAsInt && rAsInt->isZero()) {
          return rAsInt;
        } else if (rAsInt && rAsInt->isOne()) {
          return l;
        }
        break;
      case (Instruction::UDiv): case (Instruction::SDiv):
        if (l == r) {
          return ConstantInt::get(l->getType(), 1);
        } else if (rAsInt && rAsInt->isOne()) {
          return l;
        } else if (lAsInt && lAsInt->isZero()) {
          return lAsInt;
        }
        break;
      default:
        return nullptr;
    }
    return nullptr;
  }

  /*
    Tries to apply a local strength reduction to simplify the given binop instruction.

    Example cases:
    x * (2 ^ k) == x << k
    x / (2 ^ k) == x >> k
  */
  BinaryOperator *tryStrengthReduction(BinaryOperator *inst) {
    const unsigned opCode = inst->getOpcode();
    Value *l = inst->getOperand(0);
    Value *r = inst->getOperand(1);
    orderOperands(opCode, &l, &r);
    ConstantInt *lAsInt = dyn_cast<ConstantInt>(l);
    ConstantInt *rAsInt = dyn_cast<ConstantInt>(r);
    switch (opCode) {
      case (Instruction::Mul):
        if (rAsInt && rAsInt->getValue().isPowerOf2()) {
          uint64_t k = rAsInt->getValue().countTrailingZeros();
          return BinaryOperator::Create(Instruction::Shl, l, ConstantInt::get(l->getType(), k));
        }
        break;
      case (Instruction::UDiv):
        if (rAsInt && rAsInt->getValue().isPowerOf2()) {
          uint64_t k = rAsInt->getValue().countTrailingZeros();
          return BinaryOperator::Create(Instruction::LShr, l, ConstantInt::get(l->getType(), k));
        }
        break;
      case (Instruction::SDiv):
        if (rAsInt && rAsInt->getValue().isPowerOf2()) {
          uint64_t k = rAsInt->getValue().countTrailingZeros();
          return BinaryOperator::Create(Instruction::AShr, l, ConstantInt::get(l->getType(), k));
        }
        break;
      default:
        return nullptr;
    }
    return nullptr;
  }

  void doLocalOpts(Module& M) {
    outs() << "Module " << M.getModuleIdentifier().c_str() << "\n";

    int nAlgIdent = 0;
    int nConstFold = 0;
    int nStrRed = 0;

    for(Function& F : M) {
      for(BasicBlock& B : F) {
        for(BasicBlock::iterator biter = B.begin(), bEnd = B.end(); biter != bEnd;) {
          if(BinaryOperator* inst = dyn_cast<BinaryOperator>(&*biter)) {
            inst->print(errs()); outs() << "\n";
            if (ConstantInt *result = tryConstantFold(inst)) {
              outs() << "replaced with " << result->getValue() << "\n";
              ReplaceInstWithValue(inst->getParent()->getInstList(), biter, result);
              nConstFold++;
            } else if (Value *result = tryAlgebraicIdentity(inst)) {
              outs() << "replaced with ";
              result->printAsOperand(outs());
              outs() << "\n";
              ReplaceInstWithValue(inst->getParent()->getInstList(), biter, result);
              nAlgIdent++;
            } else if (BinaryOperator *result = tryStrengthReduction(inst)) {
              outs() << "replaced with ";
              result->print(outs());
              outs() << "\n";
              ReplaceInstWithInst(inst->getParent()->getInstList(), biter, result);
              nStrRed++;
              ++biter;
            } else {
              ++biter;
            }
          } else {
            ++biter;
          }
        }
      }
    }
    outs() << "Transformations applied:\n\tAlgebraic identities: " << nAlgIdent
           << "\n\tConstant folding: " << nConstFold
           << "\n\tStrength reduction: " << nStrRed << "\n";
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
