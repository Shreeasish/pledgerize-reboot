#ifndef GENERATOR_H
#define GENERATOR_H

#include "ConditionList.h"

#include <stack>

//size_t to exprID?

//TODO: Add checks for signed int overflow.
//TODO: Add checks for boundaries between ID types
//TODO: CamelCase functions
//TODO: Move traversals to a single function
using ValueConstOrBinaryExprNode = std::
    variant<const ValueExprNode, const ConstantExprNode, const BinaryExprNode>;

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
      constantExprCounter++;
      constantExprCounter++;
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
  GetOrCreateExprID(llvm::Value* const value) {
    assert(value != nullptr);
    if (auto* constant = llvm::dyn_cast<llvm::Constant>(value)) {
      return GetOrCreateExprID(constant);
    }
    if (auto found = leafTable.find(value); found != leafTable.end()) {
      return found->second;
    } //TODO: Refactor to function
    return GenerateValueExprID(value);
  }

  ExprID
  GetOrCreateExprID(llvm::Constant* const constant) {
    assert(constant != nullptr);
    if (auto constantExpr = llvm::dyn_cast<llvm::ConstantExpr>(constant)) {
      llvm::errs() << "\n\n Found Constant Expr" << *constant;
    }
    if (auto* asGepOp  = llvm::dyn_cast<llvm::GEPOperator>(constant)) {
      return GetOrCreateExprID(asGepOp);
    }
    if (auto found = leafTable.find(constant); found != leafTable.end()) {
      return found->second;
    }
    return GenerateConstantExprID(constant);
  }

  ExprID
  GetOrCreateExprID(ExprKey key, llvm::Value* const value) {
    if (auto found = exprTable.find(key); found != exprTable.end()){
      return found->second;
    }
    return GenerateBinaryExprID(key, value);
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
  GetOrCreateExprID(llvm::GetElementPtrInst* const gep) {
    assert(gep != nullptr);
    auto asGepOp = llvm::dyn_cast<llvm::GEPOperator>(gep);
    return GetOrCreateExprID(asGepOp);
  }

  ExprID
  GetOrCreateExprID(llvm::GEPOperator* const gep) {
    assert(gep != nullptr);
    auto* lhs  = gep->getPointerOperand();
    auto lhsID = GetOrCreateExprID(lhs);
    for (auto idxIt = gep->idx_begin(); idxIt < gep->idx_end(); idxIt++) {
      auto rhsID = GetOrCreateExprID(*idxIt);
      ExprKey key{lhsID, gep->getOpcode(), rhsID};
      lhsID = GetOrCreateExprID(key, gep);
    }
    return lhsID;
  }

  ExprID
  GetOrCreateExprID(llvm::LoadInst* const loadInst) {
    auto* pointer      = loadInst->getPointerOperand();
    auto pointerExprID = GetOrCreateExprID(pointer);
    return GetOrCreateExprID({GetEmptyExprID(), llvm::Instruction::Load, pointerExprID}, loadInst);
  }

  ExprID
  GetOrCreateExprID(llvm::CastInst* const castInst) {
    // Get first operand
    llvm::Value* operand = castInst->op_begin()->get();
    auto opExprID = GetOrCreateExprID(operand);
    llvm::errs() << "\n cast handled explicitly";
    return GetOrCreateExprID({opExprID, OpIDs::Cast, GetEmptyExprID()}, castInst);
  }

  ExprID
  GetOrCreateExprID(llvm::CallSite& cs) {
    auto* fun  = llvm::dyn_cast<llvm::Function>(cs.getCalledValue()->stripPointerCasts());
    llvm::errs() << "\ninserting function" << *fun;
    auto lhsID = GetOrCreateExprID(fun);
    for (auto& arg : cs.args()) {
      auto rhsID = GetOrCreateExprID(arg);
      ExprKey key{lhsID, llvm::Instruction::Call, rhsID};
      lhsID = GetOrCreateExprID(key, cs.getInstruction());
    }
    ExprKey withSentinel{lhsID, llvm::Instruction::Call, GetEmptyExprID()};
    return GetOrCreateExprID(withSentinel, cs.getInstruction());
  }

  ExprID
  GetOrCreateAliasID(const BinaryExprNode& node, llvm::StoreInst* const store) {
    auto storePtr   = store->getPointerOperand();
    auto storePtrID = GetOrCreateExprID(storePtr);
    auto loadPtrID  = node.rhs;
    ExprKey key{loadPtrID, OpIDs::Alias, storePtrID};
    return GetOrCreateExprID(key, store);
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
    assert((conjunctID & reservedBExprBits()) && "ConjunctID is not a BinaryExprID");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (~reservedBExprBits());
      return index;
    };
    return binarySlab[asIndex(conjunctID)];
  }

  const ConstantExprNode&
  GetConstantExprNode(const ExprID conjunctID) const {
    assert((conjunctID & reservedCExprBits()) && "ConjunctID is not a ConstantExprID");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (~reservedCExprBits());
      return index; 
    };
    return constantSlab[asIndex(conjunctID)];
  }

  const ValueExprNode&
  GetValueExprNode(const ExprID conjunctID) const {
    assert((conjunctID & reservedVExprBits()) && "ConjunctID is not a ValueExprID");
    auto asIndex = [this](const auto& conjunctID) -> ExprID {
      auto index = conjunctID & (~reservedVExprBits());
      return index;
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
          {ExprID(leftID), OpKey(binaryNode.op.opCode), ExprID(rightID)}, binaryNode.value);
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
      auto from = std::unique(newDisjunct.begin(), newDisjunct.end());
      newDisjunct.conjunctIDs.erase(from, newDisjunct.end());
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
      auto from = std::remove_if(
          disjunct.begin(), disjunct.end(), [&](const auto& conjunct) {
            return find(conjunct, oldExprID);
          });
      disjunct.conjunctIDs.erase(from, disjunct.end());

      if (!disjunct.size()) {
        disjunct.addConjunct({GetVacuousExprID(), true});
      }
    }
    return localDisjunction;
  }

  bool
  isUsed(llvm::Value* const value) const {
    bool ops =
        std::any_of(value->use_begin(), value->use_end(), [this](const auto& use) {
          return leafTable.count(use.get());
        });
    return (value && leafTable.count(value) > 0) || ops;
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
  preOrderFor(const Disjunction& disjunction, Visitor& visitor) {
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
  preOrderFor(const Conjunct& conjunct, Visitor& visitor) {
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
          exprStack.push(binaryNode->rhs);
          exprStack.push(binaryNode->lhs);
        }
      }
    };
    
    auto localConjunct = conjunct;
    preOrder(localConjunct);
    return localConjunct;
  }

  template <class Visitor>
  Conjunct
  inOrderFor(const Conjunct& conjunct, Visitor& visitor) {
    auto inOrder = [&, this](const ExprID& exprID, auto inOrder) -> void {
      auto asVariant   = std::variant<ExprID>(exprID);
      auto node = GetExprNode(exprID);
      if (auto binaryNode = std::get_if<const BinaryExprNode>(&node)) {
        inOrder(binaryNode->lhs, inOrder);
        std::visit(visitor, asVariant, node);
        inOrder(binaryNode->rhs, inOrder);
      } else {
        std::visit(visitor, asVariant, node);
      }
    };

    inOrder(conjunct.exprID, inOrder);
    return conjunct;
  }

  //TODO: Fix this unsightly mess
  template <class Visitor>
  std::pair<Conjunct, llvm::Value*>
  postOrderFor(const Conjunct& conjunct, Visitor& visitor) {
    auto postOrder = [&, this](const ExprID& exprID, auto postOrder) -> llvm::Value* {
      auto node = GetExprNode(exprID);
      if (auto binaryNode = std::get_if<const BinaryExprNode>(&node)) {
          llvm::Value* retLhs = postOrder(binaryNode->lhs, postOrder);
          llvm::Value* retRhs = postOrder(binaryNode->rhs, postOrder);
          return visitor(exprID, *binaryNode, retLhs, retRhs);  
      } else if (auto constantNode = std::get_if<const ConstantExprNode>(&node)) {
          return visitor(exprID, *constantNode);
      } else if (auto valueExprNode = std::get_if<const ValueExprNode>(&node)) {
          return visitor(exprID, *valueExprNode);
      } else {
        llvm_unreachable("fuck visitors");
      }
    };

    auto finalRet = postOrder(conjunct.exprID, postOrder);
    return std::make_pair(conjunct, finalRet);
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
    leafTable.insert({constant, constantExprCounter});
    return constantExprCounter++;
  }

  ExprID
  GenerateValueExprID(llvm::Value* const value) {
    valueSlab.emplace_back(value);
    leafTable.insert({value, valueExprCounter});
    return valueExprCounter++;
  }

  ExprID
  GenerateBinaryExprID(ExprKey key, llvm::Value* const value) {
    auto getPredicate =
        [](llvm::Value* const value) -> llvm::CmpInst::Predicate {
      if ( auto asCmp = llvm::dyn_cast<llvm::CmpInst>(value)) {
        return asCmp->getPredicate();
      } else if (auto asSwitch = llvm::dyn_cast<llvm::SwitchInst>(value)) {
				return Predicate::ICMP_EQ;
			}
      return llvm::CmpInst::Predicate::BAD_ICMP_PREDICATE;
    };
    binarySlab.emplace_back(BinaryExprNode{std::get<0>(key),
                                           std::get<1>(key),
                                           std::get<2>(key),
                                           getPredicate(value),
                                           value});
    exprTable.insert({key, binaryExprCounter});
    return binaryExprCounter++;
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
