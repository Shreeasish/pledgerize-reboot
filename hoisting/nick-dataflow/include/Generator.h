#ifndef GENERATOR_H
#define GENERATOR_H

#include "ConditionList.h"

//size_t to exprID?

class Generator {
  ExprID exprCounter = 0;
  std::deque<ConstantExprNode> constantSlab;
  std::deque<ValueExprNode> valueSlab;
  std::deque<BinaryExprNode> binarySlab;
  llvm::DenseMap<llvm::Value*, ExprID> leafTable;
  llvm::DenseMap<ExprKey, ExprID> exprTable;

private:
  ExprID // Templatize
  GenerateConstantExprID(llvm::Constant* constant) {
    const auto old = exprCounter;

    assert(constant != nullptr);
    assert(llvm::isa<llvm::Constant>(constant));

    llvm::errs() << "\nGenerating constant for\n";
    llvm::errs() << *constant;

    constantSlab.emplace_back(constant);
    exprCounter++;
    leafTable.insert({constant, exprCounter});

    assert(exprCounter == old+1);
    return exprCounter;
  }


  ExprID
  GenerateValueExprID(llvm::Value* value) {
    const auto old = exprCounter;
    llvm::errs() << "\nGenerating a value node\n";

    if (value != nullptr) llvm::errs() << *value;
    valueSlab.emplace_back(value);
    exprCounter++;
    leafTable.insert({value, exprCounter});

    assert(exprCounter == old+1);
    return exprCounter;
  }

  ExprID
  GenerateBinaryExprID(ExprKey key) {
    binarySlab.emplace_back(BinaryExprNode{std::get<0>(key), std::get<1>(key), std::get<2>(key)});
    exprTable.insert({key, ++exprCounter});
    return exprCounter;
  }

  ExprID //Handle CmpInst
  GetOrCreateExprID(llvm::Instruction* cmpInst) {
    assert(cmpInst != nullptr);
    auto* lhs = cmpInst->getOperand(0);
    auto* rhs = cmpInst->getOperand(1);
    auto lhsID = GetOrCreateExprID(lhs);
    auto rhsID = GetOrCreateExprID(rhs);
    ExprKey key{lhsID, cmpInst->getOpcode(), rhsID};

    if (auto found = exprTable.find(key); found != exprTable.end()) {
      return found->second;
    }
    return GenerateBinaryExprID(key);
  }

public:
  Generator()
   : exprCounter{0} {
    constantSlab.emplace_back(ConstantExprNode{nullptr});
    leafTable.insert({nullptr, exprCounter});
  }
  
  ExprID //Handle BinaryOperator
  GetOrCreateExprID(llvm::BinaryOperator* binOperator) {
    return GetOrCreateExprID(llvm::dyn_cast<llvm::Instruction>(binOperator));
//    assert(binOperator != nullptr);
//    auto* lhs = binOperator->getOperand(0);
//    auto* rhs = binOperator->getOperand(1);
//    auto lhsID = GetOrCreateExprID(lhs);
//    auto rhsID = GetOrCreateExprID(rhs);
//    ExprKey key{lhsID, binOperator->getOpcode(), rhsID};
//
//    if (auto found = exprTable.find(key); found != exprTable.end()) {
//      return found->second;
//    }
//    return GenerateBinaryExprID(key);
  }

  ExprID //Handle CmpInst
  GetOrCreateExprID(llvm::CmpInst* cmpInst) {
    return GetOrCreateExprID(llvm::dyn_cast<llvm::Instruction>(cmpInst));
//    assert(cmpInst != nullptr);
//    auto* lhs = cmpInst->getOperand(0);
//    auto* rhs = cmpInst->getOperand(1);
//    auto lhsID = GetOrCreateExprID(lhs);
//    auto rhsID = GetOrCreateExprID(rhs);
//    ExprKey key{lhsID, cmpInst->getOpcode(), rhsID};
//
//    if (auto found = exprTable.find(key); found != exprTable.end()) {
//      return found->second;
//    }
//    return GenerateBinaryExprID(key);
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
  GetVacuousExprID() {
    return 0;
  }
};

#endif
