#ifndef GENERATOR_H
#define GENERATOR_H

#include "ConditionList.h"

#include <stack>

//size_t to exprID?

//TODO: Add checks for signed int overflow.
//TODO: Add checks for boundaries between ID types
//TODO: CamelCase functions
//TODO: Move traversals to a single function
using ValueConstOrBinaryExprNode = std::variant<const ValueExprNode, const ConstantExprNode, const BinaryExprNode>;

class Generator {
public:
  enum ExprNodeType {
    Special,
    Value,
    Binary,
    Constant
  };

  Generator()
       : valueExprCounter   { reservedVExprBits() }, 
         binaryExprCounter  { reservedBExprBits() },
         constantExprCounter{ reservedCExprBits() } {
      constantSlab.emplace_back(ConstantExprNode{nullptr});
      constantSlab.emplace_back(ConstantExprNode{nullptr});
      leafTable.insert({nullptr, GetVacuousExprID()});
    }

  void
  dumpState() {
    llvm::errs() << "\n------------ Binary Table --------\n";
    for (auto [exprKey, exprID] : exprTable) {
      llvm::errs() << "ID:: " << exprID;
      llvm::errs() << "\t ExprKey --- (" 
                   << std::get<0>(exprKey) << "|"
                   << std::get<1>(exprKey) << "|" 
                   << std::get<2>(exprKey) << ")\n";
    }
    llvm::errs() << "\n------------ Leaf Table ---------\n";
    for (auto [value, exprID] : leafTable) {
      llvm::errs() << "ID:: " << exprID << "\t";
      if (value == nullptr) {
        llvm::errs() << "nullptr" ;
      }
      else {
        llvm::errs() << *value << "\n";
      }
    }
    llvm::errs() << "\n------------ Leaf Table ------\n";
  }

  ExprID
  GetOrCreateExprID(ExprKey key, llvm::Instruction* const instruction) {
    if (auto found = exprTable.find(key); found != exprTable.end()){
      return found->second;
    }
    return GenerateBinaryExprID(key, instruction);
  }


  ExprID
  GetOrCreateExprID(llvm::BinaryOperator* const binOperator) {
    return GetOrCreateExprID(llvm::dyn_cast<llvm::Instruction>(binOperator));
  }

  ExprID
  GetOrCreateExprID(llvm::CmpInst* const cmpInst) {
    return GetOrCreateExprID(llvm::dyn_cast<llvm::Instruction>(cmpInst));
  }

  ExprID
  GetOrCreateExprID(llvm::Value* const value) {
    assert(value != nullptr);
    if (auto found = leafTable.find(value); found != leafTable.end()) {
      return found->second;
    }
    if (llvm::isa<llvm::Constant>(value)) {
      return GenerateConstantExprID(llvm::dyn_cast<llvm::Constant>(value));
    }
    return GenerateValueExprID(value);
  }

  ExprID
  GetOrCreateExprID(llvm::GetElementPtrInst* const gep) {
    assert(gep != nullptr);

    auto* lhs = gep->getPointerOperand();
    auto lhsID = GetOrCreateExprID(lhs);
    for (auto& index : gep->indices()) {
      auto rhsID = GetOrCreateExprID(index);
      ExprKey key{lhsID, gep->getOpcode(), rhsID};
      lhsID = GetOrCreateExprID(key, gep);
    }
    return lhsID;
  }

  ExprID
  GetOrCreateExprID(llvm::LoadInst* const loadInst) {
    auto* pointer      = loadInst->getPointerOperand();
    auto pointerExprID = GetOrCreateExprID(pointer);
    return GetOrCreateExprID({pointerExprID, OpIDs::loadOp, GetEmptyExprID()}, loadInst);
  }

  ExprID
  constexpr GetEmptyExprID() {
    return ReservedExprIDs::emptyExprID;
  }

  ExprID
  constexpr GetVacuousExprID() {
    return ReservedExprIDs::vacuousExprID;
  }

  Conjunct
  GetVacuousConjunct() {
    return {ReservedExprIDs::vacuousExprID, true};
  }

  // TODO: Establish whether only binaryExprs are needed
  inline int 
  GetExprType(const ExprID exprID) const {
    return exprID >> (typeSize - 3); // Assumption: Signbit will not be set
  }

  auto inline 
  getLeafTableSize() {
    return leafTable.size();
  }

  const ConstantExprNode&
  GetSpecialExprNode(const ExprID conjunctID) const {
    assert(!(conjunctID & reservedCExprBits()) && "ConjunctID is not a SpecialID");
    auto asIndex = conjunctID;
    return constantSlab[asIndex];
  }
  
  const BinaryExprNode& 
  GetBinaryExprNode(const ExprID conjunctID) const {
    assert((conjunctID & reservedBExprBits()) && "ConjunctID is not a BinaryExpr");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (~reservedBExprBits());
      return index - 1; // Start from 0 // Expr Counters start from 1
    };
    return binarySlab[asIndex(conjunctID)];
  }

  const ConstantExprNode&
  GetConstantExprNode(const ExprID conjunctID) const {
    assert((conjunctID & reservedCExprBits()) && "ConjunctID is not a ConstantExpr");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (~reservedCExprBits());
      return index; // Expr Counter starts from 0, Special case for Vacuous Expr
    };
    return constantSlab[asIndex(conjunctID)];
  }

  const ValueExprNode&
  GetValueExprNode(const ExprID conjunctID) const {
    assert((conjunctID & reservedVExprBits()) && "ConjunctID is not a ValueExprID");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (~reservedVExprBits());
      return index - 1; // Start from 0 // Expr Counters start from 1
    };
    return valueSlab[asIndex(conjunctID)];
  }

  ValueConstOrBinaryExprNode
  GetExprNode(const ExprID exprID) {
    auto exprType = GetExprType(exprID);
    switch (exprType) {
      case ExprNodeType::Special: {
        return GetSpecialExprNode(exprID);
        break;
      }
      case ExprNodeType::Value: {
        return GetValueExprNode(exprID);
        break;
      }
      case ExprNodeType::Binary: {
        return GetBinaryExprNode(exprID);
        break;
      }
      case ExprNodeType::Constant: {
        return GetConstantExprNode(exprID);
        break;
      }
      default: {
        llvm_unreachable("Node type doesn't work");
      }
    }
  }


  //TODO: Memoize
  Disjunction
  rewrite(const Disjunction& disjunction, const ExprID oldExprID, const ExprID newExprID) {
    //TODO: Make Node types enum
    auto isBinaryExprID = [this](const ExprID exprID) -> bool {
      return GetExprType(exprID) == 2;
    };

    auto postOrderRebuild = 
      [&,this] (const ExprID& exprID, auto& postOrderRebuild) -> ExprID {
      if (exprID == oldExprID) {
        return newExprID;  // Return the newID implying a replacement
      }                    // Doesn't matter what type of conjunct
      if (!isBinaryExprID(exprID)) {
        return exprID;     // Leaf node and !==oldExprID
      }
      auto binaryNode = GetBinaryExprNode(exprID);
      auto leftID  = postOrderRebuild(binaryNode.lhs, postOrderRebuild);
      auto rightID = postOrderRebuild(binaryNode.rhs, postOrderRebuild);
      return GetOrCreateExprID(
          {ExprID(leftID), OpKey(binaryNode.op.opCode), ExprID(rightID)}, binaryNode.instruction);
    };

    auto rebuildConjunct = [&postOrderRebuild] (const Conjunct& conjunct) -> Conjunct {
      auto rebuiltID   = postOrderRebuild(conjunct.exprID, postOrderRebuild);
      return Conjunct{rebuiltID, conjunct.notNegated};
    };

    Disjunction newDisjunction{ };
    for (auto& disjunct : disjunction.disjuncts) {
      Disjunct newDisjunct{};
      std::transform(disjunct.conjunctIDs.begin(), disjunct.conjunctIDs.end(),
                  std::back_inserter(newDisjunct.conjunctIDs), rebuildConjunct);
      std::sort(newDisjunct.conjunctIDs.begin(), newDisjunct.conjunctIDs.end());
      newDisjunction.addDisjunct(newDisjunct);
    }
    return newDisjunction;
  }

  Disjunction
  dropConjunct(const Disjunction& disjunction, const ExprID oldExprID) {
    auto isBinaryExprID = [this](const ExprID exprID) -> bool {
      return GetExprType(exprID) == 2;
    };

    auto preOrderFind  = [this, &oldExprID, &isBinaryExprID](const Conjunct& conjunct) -> bool {
      std::stack<ExprID> exprStack;
      exprStack.push(conjunct.exprID);
      while (!exprStack.empty()) {
        auto exprID = exprStack.top();
        exprStack.pop();
        if (oldExprID == exprID){
          return true;
        }
        if (!isBinaryExprID(exprID)) {
          continue;
        }
        auto binaryNode = GetBinaryExprNode(exprID);
        exprStack.push(binaryNode.lhs);
        exprStack.push(binaryNode.rhs);
      }
      return false;
    };

    Disjunction newDisjunction = disjunction; // Return new by value
    for (auto& disjunct : newDisjunction.disjuncts) {
      std::replace_if(disjunct.conjunctIDs.begin(), disjunct.conjunctIDs.end(),
                      preOrderFind, Conjunct{GetVacuousExprID(), true});
      std::sort(disjunct.conjunctIDs.begin(), disjunct.conjunctIDs.end());
    }
    return newDisjunction;
  }

  Disjunction
  pushToTrue(const Disjunction& disjunction, const ExprID oldExprID) {
    Disjunction localDisjunction = disjunction;
    for (auto& disjunct : localDisjunction) {
      for (auto& conjunct : disjunct) {
        if (find(conjunct, oldExprID)) {
          conjunct = {GetVacuousExprID(), true};
        }
      }
    }
    return localDisjunction;
  }
  
  bool
  isUsed(llvm::Value* const value) const {
    return value && leafTable.count(value) > 0;
  }

  bool
  find(const Conjunct& conjunct, const ExprID& targetExprID) const {
    auto isBinaryExprID = [this](const ExprID exprID) -> bool {
      return GetExprType(exprID) == 2;
    };
    auto preOrder = [this, &targetExprID, &isBinaryExprID](const Conjunct& conjunct) -> bool {
      std::stack<ExprID> exprStack;
      exprStack.push(conjunct.exprID);
      while (!exprStack.empty()) {
        auto exprID = exprStack.top();
        exprStack.pop();
        if (targetExprID == exprID){
          return true;
        }
        if (!isBinaryExprID(exprID)) {
          continue;
        }
        auto binaryNode = GetBinaryExprNode(exprID);
        exprStack.push(binaryNode.lhs);
        exprStack.push(binaryNode.rhs);
      }
      return false;
    };

    return preOrder(conjunct);
  }

  template <class Visitor>
  Disjunction
  for_each(const Disjunction& disjunction, Visitor visitor) {
    auto preOrder = [&, this](Conjunct& conjunct) {
      std::stack<ExprID> exprStack;
      exprStack.push(conjunct.exprID);
      while (!exprStack.empty()) {
        auto exprID = exprStack.top();
        exprStack.pop();

        auto asVariant = GetExprNode(exprID);
        // FIXME: I don't like this
        std::variant<ExprID> exprIDAsVariant{exprID};
        std::visit(visitor, exprIDAsVariant, asVariant);

        if (auto binaryNode = std::get_if<const BinaryExprNode>(&asVariant)) {
          exprStack.push(binaryNode->lhs);
          exprStack.push(binaryNode->rhs);
        }
      }
    };

    auto localDisjunction = disjunction;
    for (auto disjunct : localDisjunction.disjuncts) {
      for (auto conjunct : disjunct) {
        preOrder(conjunct);
      }
    }
    return localDisjunction;
  }

  template <class Visitor>
  Conjunct
  for_each(const Conjunct& conjunct, Visitor visitor) {
    auto preOrder = [&, this](Conjunct& conjunct) {
      std::stack<ExprID> exprStack;
      exprStack.push(conjunct.exprID);
      while (!exprStack.empty()) {
        auto exprID = exprStack.top();
        exprStack.pop();

        auto asVariant = GetExprNode(exprID);
        // FIXME: I don't like this
        std::variant<ExprID> exprIDAsVariant{exprID};
        std::visit(visitor, exprIDAsVariant, asVariant);

        if (auto binaryNode = std::get_if<const BinaryExprNode>(&asVariant)) {
          exprStack.push(binaryNode->lhs);
          exprStack.push(binaryNode->rhs);
        }
      }
    };
    
    auto localConjunct = conjunct;
    preOrder(localConjunct);
    return localConjunct;
  }

private:
  std::deque<ConstantExprNode> constantSlab;
  std::deque<ValueExprNode>    valueSlab;
  std::deque<BinaryExprNode>   binarySlab;

  ExprID valueExprCounter;
  ExprID binaryExprCounter;
  ExprID constantExprCounter;

  llvm::DenseMap<ExprKey, ExprID> exprTable;
  llvm::DenseMap<llvm::Value*, ExprID> leafTable;

  
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
  GenerateConstantExprID(llvm::Constant* const constant) {
    assert(constant != nullptr);

    constantSlab.emplace_back(constant);
    constantExprCounter++;
    leafTable.insert({constant, constantExprCounter});
    return constantExprCounter;
  }

  ExprID
  GenerateValueExprID(llvm::Value* const value) {
    valueSlab.emplace_back(value);
    valueExprCounter++;
    leafTable.insert({value, valueExprCounter});
    return valueExprCounter;
  }

  ExprID
  GenerateBinaryExprID(ExprKey key, llvm::Instruction* const instruction) {
    binarySlab.emplace_back(BinaryExprNode{
        std::get<0>(key), std::get<1>(key), std::get<2>(key), instruction});
    binaryExprCounter++;
    exprTable.insert({key, binaryExprCounter});
    return binaryExprCounter;
  }

  ExprID
  GetOrCreateExprID(llvm::Instruction* const inst) {
    assert(inst != nullptr);
    auto*  lhs = inst->getOperand(0);
    auto*  rhs = inst->getOperand(1);
    auto lhsID = GetOrCreateExprID(lhs);
    auto rhsID = GetOrCreateExprID(rhs);
    ExprKey key{lhsID, inst->getOpcode(), rhsID};

    if (auto found = exprTable.find(key); found != exprTable.end()) {
      return found->second;
    }
    return GenerateBinaryExprID(key, inst);
  }
};

#endif
