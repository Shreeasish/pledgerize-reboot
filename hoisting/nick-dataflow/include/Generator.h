#ifndef GENERATOR_H
#define GENERATOR_H

#include "ConditionList.h"

//size_t to exprID?

//TODO: Add checks for signed int overflow.
//TODO: Add checks for boundaries between ID types
//TODO: CamelCase functions
class Generator {
public:
  Generator()
       : valueExprCounter   { reservedVExprBits() }, 
         binaryExprCounter  { reservedBExprBits() },
         constantExprCounter{ reservedCExprBits() } {
      constantSlab.emplace_back(ConstantExprNode{nullptr});
      leafTable.insert({nullptr, constantExprCounter});
    }

  ExprID //Handle BinaryOperator
  GetOrCreateExprID(const llvm::BinaryOperator* binOperator) {
    return GetOrCreateExprID(llvm::dyn_cast<llvm::Instruction>(binOperator));
  }

  ExprID //Handle CmpInst
  GetOrCreateExprID(const llvm::CmpInst* cmpInst) {
    return GetOrCreateExprID(llvm::dyn_cast<llvm::Instruction>(cmpInst));
  }

  ExprID
  GetOrCreateExprID(const llvm::Value* value) {
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
  inline GetVacuousExprID() {
    return 0;
  }

  // TODO: Establish whether only binaryExprs are needed
  inline int 
  GetExprType(const ExprID exprID) {
    return exprID & (reservedCExprBits() >> (typeSize - 2));
  }

  // Getters for backingstore
  const BinaryExprNode& //Invariant: The ID searched for will always be there
  GetBinaryExprNode(const ExprID conjunctID) const {
    assert((conjunctID && reservedBExprBits()) && "ConjunctID is not a BinaryExpr");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (!reservedBExprBits()); //Removal: remove bitwise
      return index;
    };
    return binarySlab[asIndex(conjunctID)];
  }

  const ConstantExprNode&
  GetConstantExprNode(const ExprID conjunctID) const {
    assert((conjunctID && reservedCExprBits()) && "ConjunctID is not a ConstantExpr");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (!reservedCExprBits());
      return index;
    };
    return constantSlab[asIndex(conjunctID)];
  }

  const ValueExprNode&
  GetValueExprNode(const ExprID conjunctID) const {
    assert((conjunctID && reservedVExprBits()) && "ConjunctID is not a ValueExprID");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (!reservedVExprBits());
      return index;
    };
    return valueSlab[asIndex(conjunctID)];
  }

  bool //Generator should not check in the state itself
  isUsed(const llvm::Value* value) const {
    return leafTable.count(value) > 0;
  }

private:
  ExprID valueExprCounter    = 0;
  ExprID binaryExprCounter   = 0;
  ExprID constantExprCounter = 0;

  std::deque<ConstantExprNode> constantSlab;
  std::deque<ValueExprNode>    valueSlab;
  std::deque<BinaryExprNode>   binarySlab;

  llvm::DenseMap<ExprKey, ExprID> exprTable;
  llvm::DenseMap<const llvm::Value*, ExprID> leafTable;

  constexpr ExprID reservedVExprBits() const {
    return 1 << (typeSize - 3); // -1 for sign bit
  }
  constexpr ExprID reservedBExprBits() const {
    return 1 << (typeSize - 2); // typeSize - 1 is the last bit
  }
  constexpr ExprID reservedCExprBits() const {
    return (1 << (typeSize - 3)) | (1 << (typeSize - 2));
  }

  ExprID // Templatize
  GenerateConstantExprID(const llvm::Constant* constant) {
    const auto old = constantExprCounter;

    assert(constant != nullptr);
    assert(llvm::isa<llvm::Constant>(constant));

    llvm::errs() << "\nGenerating constant for\n";
    llvm::errs() << *constant;

    constantSlab.emplace_back(constant);
    constantExprCounter++;
    leafTable.insert({constant, constantExprCounter});

    assert(constantExprCounter == old+1);
    return constantExprCounter;
  }

  ExprID
  GenerateValueExprID(const llvm::Value* value) {
    const auto old = valueExprCounter;
    llvm::errs() << "\nGenerating a value node\n";

    if (value != nullptr) llvm::errs() << *value;
    valueSlab.emplace_back(value);
    valueExprCounter++;
    leafTable.insert({value, valueExprCounter});

    assert(valueExprCounter == old+1);
    return valueExprCounter;
  }

  ExprID
  GenerateBinaryExprID(ExprKey key) {
    binarySlab.emplace_back(BinaryExprNode{std::get<0>(key), std::get<1>(key), std::get<2>(key)});
    exprTable.insert({key, ++binaryExprCounter});
    return binaryExprCounter;
  }

  ExprID //Handle CmpInst
  GetOrCreateExprID(const llvm::Instruction* cmpInst) {
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

};

#endif
