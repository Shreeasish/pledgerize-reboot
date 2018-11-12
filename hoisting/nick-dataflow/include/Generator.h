#ifndef GENERATOR_H
#define GENERATOR_H

#include "ConditionList.h"

#include <stack>

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
      leafTable.insert({nullptr, 0});
    }
  /* Debugging Function -----------------*/
  void
  dumpState() {
    llvm::errs() << "\n------------ Binary Table ------\n";
    for (auto [exprKey, exprID] : exprTable) {
      llvm::errs() << "\n ExprKey --- {" 
                   << std::get<0>(exprKey) /*lhs*/ << "," << std::get<2>(exprKey) /*rhs*/
                   << "}" ;
      llvm::errs() << "\t ID:: " << exprID;
    }
    llvm::errs() << "\n------------ Binary Table ------\n";
    llvm::errs() << "\n------------ Leaf Table ------\n";
    for (auto [value, exprID] : leafTable) {
      llvm::errs() << "\n llvm::value --- ";
      if (value == nullptr) {
        llvm::errs() << "nullptr" ;
      }
      else {
        llvm::errs() << *value;
      }
      llvm::errs() << "\t ID:: " << exprID;
    }
    llvm::errs() << "\n------------ Leaf Table ------\n";
  }
  /* Debugging Function -----------------*/

  ExprID
  GetOrCreateExprID(ExprKey key) {
    if (auto found = exprTable.find(key); found != exprTable.end()){
      return found->second;
    }
    return GenerateBinaryExprID(key);
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
  GetExprType(const ExprID exprID) const {
    return exprID >> (typeSize - 3); // Assumption: Signbit will not be set
  }

  // Getters for backingstore
  const BinaryExprNode& //Invariant: The ID searched for will always be there
  GetBinaryExprNode(const ExprID conjunctID) const {
    assert((conjunctID && reservedBExprBits()) && "ConjunctID is not a BinaryExpr");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (~reservedBExprBits()); //Removal: remove bitwise
      return index - 1; // Start from 0 // Expr Counters start from 1
    };
    return binarySlab[asIndex(conjunctID)];
  }

  const ConstantExprNode&
  GetConstantExprNode(const ExprID conjunctID) const {
    assert((conjunctID && reservedCExprBits()) && "ConjunctID is not a ConstantExpr");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (~reservedCExprBits());
      return index; // Expr Counter starts from 0, Special case for Vacuous Expr
    };
    return constantSlab[asIndex(conjunctID)];
  }

  const ValueExprNode&
  GetValueExprNode(const ExprID conjunctID) const {
    assert((conjunctID && reservedVExprBits()) && "ConjunctID is not a ValueExprID");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (~reservedVExprBits());
      return index - 1; // Start from 0 // Expr Counters start from 1
    };
    return valueSlab[asIndex(conjunctID)];
  }

  Disjunction
  rewrite(const Disjunction& disjunction, const ExprID oldExprID, const ExprID newExprID) {
    //TODO: Make Node types enum
    auto isBinaryExprID = [this](const ExprID exprID) -> bool {
      return GetExprType(exprID) == 2;
    };

    auto replace = [](auto& conjunct, const ExprID newExprID) {
      conjunct = Conjunct{newExprID, conjunct.notNegated};
    };

    auto postOrderRebuild = 
      [this, &newExprID, &oldExprID, &isBinaryExprID] (const ExprID& exprID, auto& postOrderRebuild) -> ExprID {
      if (exprID == oldExprID) {
        return newExprID;  // Return the newID implying a replacement
      }                    // Doesn't matter what type of conjunct
      if (!isBinaryExprID(exprID)) {
        return exprID;     // Leaf node and !==oldExprID
      }
      auto binaryNode = GetBinaryExprNode(exprID);
      auto leftID  = postOrderRebuild(binaryNode.lhs, postOrderRebuild);
      auto rightID = postOrderRebuild(binaryNode.rhs, postOrderRebuild);
      return GetOrCreateExprID({ExprID(leftID), OpKey(binaryNode.op.opCode), ExprID(rightID)}); //Readability
    };

    Disjunction newDisjunction = disjunction; // Return new by value
    for (auto& disjunct : newDisjunction.disjuncts) {
      for (auto& conjunct : disjunct.conjunctIDs) {
        if (isBinaryExprID(conjunct.exprID)) {
          auto rebuildID = postOrderRebuild(conjunct.exprID, postOrderRebuild);
          replace(conjunct, rebuildID); 
          continue; // skip check for direct comparison
        }
        if ( conjunct.exprID == oldExprID) {
          replace(conjunct, newExprID);
          continue; 
        }
      }
    }
    return newDisjunction;
  }

  Disjunction
  dropConjunct(const Disjunction& disjunction, const ExprID oldExprID) {
    auto isBinaryExprID = [this](const ExprID exprID) -> bool {
      return GetExprType(exprID) == 2;
    };

    auto preOrderFind  = [this, &oldExprID, &isBinaryExprID](const Conjunct& conjunct) -> bool {
//      if (oldExprID == exprID) {
//        found = true;
//        return found;
//      }
//      if (!isBinaryExprID(exprID)) {
//        return found;
//      }
//      auto binaryNode = GetBinaryExprNode(exprID);
//      postOrderFind(binaryNode.lhs, postOrderFind);
//      postOrderFind(binaryNode.rhs, postOrderFind);
//
//      return found;
//      Iterative Find
//
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
      std::replace_if( std::begin(disjunct.conjunctIDs), std::end(disjunct.conjunctIDs),
                       preOrderFind, Conjunct{GetVacuousExprID(), true});
    }
    return newDisjunction;
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
    assert(constant != nullptr);

    constantSlab.emplace_back(constant);
    constantExprCounter++;

    llvm::errs() << "\nGenerating constant for\n";
    llvm::errs() << *constant;
    llvm::errs() << "\n" << constantExprCounter;

    leafTable.insert({constant, constantExprCounter});
    return constantExprCounter;
  }

  ExprID
  GenerateValueExprID(const llvm::Value* value) {
    valueSlab.emplace_back(value);
    valueExprCounter++;
    leafTable.insert({value, valueExprCounter});

    llvm::errs() << "\nGenerating a value node for\n";
    llvm::errs() << *value;
    llvm::errs() << "\n" << valueExprCounter;

    return valueExprCounter;
  }

  ExprID
  GenerateBinaryExprID(ExprKey key) {
    binarySlab.emplace_back(BinaryExprNode{std::get<0>(key), std::get<1>(key), std::get<2>(key)});
    binaryExprCounter++;
    exprTable.insert({key, binaryExprCounter});
    return binaryExprCounter;
  }

  ExprID //TODO: Rename cmpInst to inst
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

  // llvm::DenseMap<ExprKey, ExprID> exprTable;
  // llvm::DenseMap<const llvm::Value*, ExprID> leafTable;
  //
  // std::deque<ConstantExprNode> constantSlab;
  // std::deque<ValueExprNode>    valueSlab;
  // std::deque<BinaryExprNode>   binarySlab;

};

#endif
