#ifndef GENERATOR_H
#define GENERATOR_H

#include "ConditionList.h"

//size_t to exprID?
//

// using ExprIDPair = std::pair<ExprID,ExprID>;

class Generator {
  ExprID exprCounter;
  std::deque<ExprNode*> slab;
  llvm::DenseMap<llvm::Value*, ExprID> leafTable;
  llvm::DenseMap<ExprKey, ExprID> exprTable;

private:
  ExprID
  GenerateVacuousExprID() {
    assert(exprCounter == 0);
    auto vacuousExprNode = new ConstantExprNode{exprCounter, nullptr};
    slab.push_back(vacuousExprNode);
    leafTable.insert({nullptr, exprCounter});
    return exprCounter;
  }

  ExprID // Templatize
  GenerateConstantExprID(llvm::Constant* constant) {
    assert(constant != nullptr);
    assert(!llvm::isa<llvm::Constant>(constant));

    llvm::errs() << "\nGenerating constant for\n";
    llvm::errs() << *constant;

    auto* newConstantExprNode = new ConstantExprNode(++exprCounter, constant);
    slab.push_back(newConstantExprNode);
    leafTable.insert({constant, exprCounter});
    return exprCounter;
  }

  ExprID
  GenerateValueExprID(llvm::Value* value) {
    llvm::errs() << "\nGenerating a value node\n";
//  assert(not value is not a nullpointer);

    auto* newValueExprNode = new ValueExprNode(++exprCounter, value);
    slab.push_back(newValueExprNode);
    leafTable.insert({value, exprCounter});
    return exprCounter;
  }

//  static ValueExprNode* GenerateValueExprNode(llvm::Instruction* branch) { }

  ExprID
  GenerateBinaryExprID(llvm::BinaryOperator* binOperator) {
    llvm::errs() << "\nGenerating new binary expression with id = " << ++exprCounter;
    auto* lhs = binOperator->getOperand(0);
    auto* rhs = binOperator->getOperand(1);
    llvm::errs() << *lhs << "\n";
    llvm::errs() << *rhs << "\n";

    auto lhsID = GetOrCreateExprID(lhs);
    auto rhsID = GetOrCreateExprID(rhs);
    ExprKey key{lhsID, binOperator->getOpcode(), rhsID};

    auto* newBinaryExprNode
      = new BinaryExprNode(++exprCounter, lhsID, rhsID, binOperator->getOpcode());
    slab.push_back(newBinaryExprNode);
    exprTable.insert({key, exprCounter});
    return exprCounter;
  }

public:
  ExprID //Handle BinaryInstrucions
  GetOrCreateExprID(llvm::BinaryOperator* binOperator) {
    assert(binOperator != nullptr);
    auto* lhs = binOperator->getOperand(0);
    auto* rhs = binOperator->getOperand(1);

    llvm::errs() << *lhs << "\n";
    llvm::errs() << *rhs << "\n";

    auto lhsID = GetOrCreateExprID(lhs);
    auto rhsID = GetOrCreateExprID(rhs);
    ExprKey key{lhsID, binOperator->getOpcode(), rhsID};

    if (auto found = exprTable.find(key); found != exprTable.end()) {
      return found->second;
    }
    return GenerateBinaryExprID(binOperator);
  }

  ExprID
  GetOrCreateExprID(llvm::Value* value) {
    assert(value != nullptr);

    // Get Constant or Value for leaf
    if (auto found = leafTable.find(value); found != leafTable.end()) {
      return found->second;
    }
    if (llvm::isa<Constant>(value)) {
      return GenerateConstantExprID(llvm::dyn_cast<Constant>(value));
    }
    return GenerateValueExprID(value);
  }

  ExprID
  GetOrCreateVacuousExprID() {
    if (auto vacuousExprPair = leafTable.find(nullptr);
        vacuousExprPair != leafTable.end()){
      return vacuousExprPair->second;
    }
    return GenerateVacuousExprID();
  }
};

#endif
