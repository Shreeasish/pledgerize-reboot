#ifndef GENERATOR_H
#define GENERATOR_H

#include "ConditionList.h"

//size_t to exprID?
//

// using ExprIDPair = std::pair<ExprID,ExprID>;

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

    constantSlab.emplace_back(ConstantExprNode{constant});
    leafTable.insert({constant, ++exprCounter});

    assert(exprCounter == old+1);
    return exprCounter;
  }


  ExprID
  GenerateValueExprID(llvm::Value* value) {
    const auto old = exprCounter;
    llvm::errs() << "\nGenerating a value node\n";

    if (value != nullptr) llvm::errs() << *value;
//  assert(not value is not a nullpointer);

    valueSlab.emplace_back(ValueExprNode{value});
    leafTable.insert({value, ++exprCounter});

    assert(exprCounter == old+1);
    return exprCounter;
  }

  ExprID
  GenerateBinaryExprID(llvm::BinaryOperator* binOperator) {
    const auto old = exprCounter;

    llvm::errs() << "\nGenerating new binary expression with id = " << exprCounter;
    auto* lhs = binOperator->getOperand(0);
    auto* rhs = binOperator->getOperand(1);
    llvm::errs() << *lhs << "\n";
    llvm::errs() << *rhs << "\n";

    auto lhsID = GetOrCreateExprID(lhs);
    auto rhsID = GetOrCreateExprID(rhs);
    ExprKey key{lhsID, binOperator->getOpcode(), rhsID};
    binarySlab.emplace_back(BinaryExprNode{lhsID, rhsID, binOperator->getOpcode()});
    exprTable.insert({key, ++exprCounter});

    assert(exprCounter == old+1);
    return exprCounter;
  }

public:
  Generator()
   : exprCounter{0} {
    constantSlab.emplace_back(ConstantExprNode{nullptr});
    leafTable.insert({nullptr, exprCounter});
  }
  
  // Eliminate accidental calls to GetOrCreateExprID(llvm::Instruction*)
  ExprID
  GetOrCreateExprID(llvm::Instruction inst) {
    assert(false &&  "Generator::GetOrCreateExprID(llvm::Instruction*) called");
  }

  ExprID //Handle BinaryInstrucions
  GetOrCreateExprID(llvm::BinaryOperator* binOperator) {
    assert(binOperator != nullptr);
    auto* lhs = binOperator->getOperand(0);
    auto* rhs = binOperator->getOperand(1);
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
  GetVacuousExprID() {
    return 0;
  }
};

#endif
