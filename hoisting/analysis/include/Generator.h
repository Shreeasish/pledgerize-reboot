#ifndef GENERATOR_H
#define GENERATOR_H

#include "llvm/Transforms/Utils/OrderedInstructions.h"
#include "llvm/IR/Dominators.h"

#include "ConditionList.h"

#include <stack>

//size_t to exprID?

//TODO: Add checks for signed int overflow.
//TODO: Add checks for boundaries between ID types
//TODO: CamelCase functions
//TODO: Move traversals to a single function
using ValueConstOrBinaryExprNode = 
    std::variant<const ValueExprNode, const ConstantExprNode, const BinaryExprNode>;

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
      initializeMap();
    }

/*-------------------------------------------------*
*---------------- DEBUG HELPERS -------------------*
*-------------------------------------------------*/
  void
  initializeMap() {
    #define HANDLE(a, b, c) opMap.try_emplace(a, #b);
    #include "OperatorStrings.def"
    #undef HANDLE
  }

  llvm::DenseMap<OpKey, const char*> opMap;

  //static constexpr bool SUPPRESSED = true;
	template<typename Printer>
  void
  dumpToFile(llvm::Module& module) {
    if constexpr (SUPPRESSED) {
      return;
    }

		llvm::outs() << "\nDumping Expr Table -- Size :" << exprTable.size();
    auto ec = std::error_code{};
    auto [modulePath, _]  = module.getName().split(".");
    auto [__, moduleName] = modulePath.rsplit("/");
    auto baseString       = "/home/shreeasish/pledgerize-reboot/hoisting/logs/"
                      + moduleName.str();
    
    std::string exprFileName = baseString + "/ids.binary";
    //llvm::sys::fs::create_directories(exprFileName);
    auto exprFile = llvm::raw_fd_ostream{exprFileName, ec};
    exprFile << "id,left,right,opId,opName" ;
    for (auto [exprKey, exprID] : exprTable) {
      exprFile << "\n" << exprID               << ","
                       << std::get<0>(exprKey) << "," //  left
                       << std::get<2>(exprKey) << "," // right
                       << std::get<1>(exprKey) << ","
                       << opMap[std::get<1>(exprKey)];
    }
    exprFile.close();


    std::string leafFileName = baseString + "/ids.leaves";
    //llvm::sys::fs::create_directories(leafFileName);
    auto leafFile = llvm::raw_fd_ostream{leafFileName, ec};
    leafFile <<  "id|:string";
    leafFile << "\n1|:nullptr"; // SPECIALCASE: Empty ExprID has value 1


    for (auto [value, exprID] : leafTable) {
      if (value == nullptr) {
        leafFile << "\n" << exprID << "|:nullptr" ;
      }
      else {
        if (auto* func = llvm::dyn_cast<llvm::Function>(value)) {
          leafFile << "\n" << exprID << "|:" << func->getName();
        } else {
          leafFile << "\n" << exprID << "|:" << *value;
        }
      }
    }
    leafFile.close();

    std::string astFileName = baseString + "/ids.flattened";
    //llvm::sys::fs::create_directories(astFileName);
    auto astFile = llvm::raw_fd_ostream{astFileName, ec};
    astFile <<  "id|:aststring";
    Printer printer{this, astFile};
    for (auto [exprKey, exprID] : exprTable) {
      astFile << "\n" << exprID << "|:";
      printer.printFlattened(exprID, astFile);
    }
    astFile.close();
		//`llvm::outs() << "\nDumping Expr Table -- Size :" << exprTable.size();
  }

  void
  dumpState(llvm::raw_ostream& outs) {
    outs << "\n------------ Binary Table --------\n";
    for (auto [exprKey, exprID] : exprTable) {
      outs << "ID:: " << exprID;
      outs << "\t ExprKey --- (" 
                   << std::get<0>(exprKey) << "|"
                   << std::get<1>(exprKey) << "|" 
                   << std::get<2>(exprKey) << ")\n";
    }
    outs << "\n------------ Leaf Table ---------\n";
    for (auto [value, exprID] : leafTable) {
      outs << "ID:: " << exprID << "\t";
      if (value == nullptr) {
        outs << "nullptr" ;
      }
      else {
        outs << *value << "\n";
      }
    }
    outs << "\n------------ Leaf Table ------\n";
  }

  auto getSize() { return exprTable.size() + leafTable.size();}

  void
  dumpExprData(llvm::raw_ostream& outs) {
    debugger.initializeMap();
    debugger.dumpBinExpr(outs);
  }
/*--------------------------------------------------*
*---------------- DEBUG HELPERS --------------------*
*--------------------------------------------------*/
  
  void
  setLocation(llvm::Instruction* location) {
    this->location = location;
  }

  llvm::Instruction*
  getLocation() {
    return this->location;
  }

  llvm::Instruction* // Fail for bad queuries
  getOrigin(ExprID exprID) {
    llvm::errs() << "\nGetting origin for" << exprID;
    assert(originMap.count(exprID) && "Does not have an origin");
    return originMap[exprID];
  };

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
      llvm::outs().changeColor(llvm::raw_ostream::Colors::RED);
      llvm::outs() << "\nFound ConstExpr GEP:"
                   << *constant;
      llvm::outs().resetColor();
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
    //llvm::errs() << "\n cast handled explicitly";
    return GetOrCreateExprID({opExprID, OpIDs::Cast, GetEmptyExprID()}, castInst);
  }

  ExprID
  GetOrCreateExprID(llvm::CallSite& cs) {
    // $val can be a load or a function. Stored as a value for generality
    auto* val = cs.getCalledValue()->stripPointerCasts();
    llvm::errs() << "\nGenerator: Calling" << *val;
    auto lhsID = GetOrCreateExprID(val);
    for (auto& arg : cs.args()) {
      auto rhsID = GetOrCreateExprID(arg);
      ExprKey key{lhsID, llvm::Instruction::Call, rhsID};
      lhsID = GetOrCreateExprID(key, cs.getInstruction());
    }
    ExprKey withSentinel{lhsID, llvm::Instruction::Call, GetEmptyExprID()};
    return GetOrCreateExprID(withSentinel, cs.getInstruction());
  }

  /* Store the pointers to loads and stores and not the instructions themeselevs.
   * Enables computation of alias checks when performing lowering instead of 
   * using them as markers only.*/
  ExprID
  GetOrCreateAliasID(const BinaryExprNode& node, llvm::StoreInst* const store) {
    auto storePtr   = store->getPointerOperand();
    auto storePtrID = GetOrCreateExprID(storePtr);

    auto loadPtrID  = node.rhs;

    ExprKey key{loadPtrID, OpIDs::Alias, storePtrID};
    return GetOrCreateExprID(key, node.value); // Carry the load instruction 
  }                                            // to check with AA Results later on.

  ExprID
  GetOrCreateAliasID(llvm::Function* const callee, const llvm::CallSite& caller) {
    auto* funcAsValue = llvm::dyn_cast<llvm::Value>(callee);
    auto funcExprID   = GetOrCreateExprID(funcAsValue);

    auto* callOperand = caller.getCalledValue();
    auto* asValue = llvm::dyn_cast<llvm::Value>(callOperand);
    auto opExprID   = GetOrCreateExprID(asValue);

    llvm::errs() << "\nUsing Call Operand " << *callOperand
                 << " With value" << opExprID;
    ExprKey key{funcExprID, OpIDs::Alias, opExprID};

    //auto* asInst = caller.getInstruction();
    //auto* csAsValue = llvm::dyn_cast<llvm::Value>(asInst);
    return GetOrCreateExprID(key, funcAsValue);
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
  
  // Janky place to hold these
  using DTree = std::unique_ptr<llvm::DominatorTree>;
  llvm::DenseMap<llvm::Function*, DTree> functionDTs;
  
  using OInsts = std::unique_ptr<llvm::OrderedInstructions>;
  llvm::DenseMap<llvm::Function*, OInsts> orderedInstMap;


  struct DefaultPolicy {
  public:
    virtual 
    std::optional<Disjunction>  // Return if changed
    operator()(const Disjunction& disjunction,
               const ExprID oldExprID,
               const ExprID newExprID) {
      return std::nullopt; // no-op in this case
    }
  };


  //TODO: Memoize
  template<typename RewritingPolicy = DefaultPolicy>
  Disjunction
  rewrite(const Disjunction& disjunction, const ExprID oldExprID, const ExprID newExprID) {
    //TODO: Make Node types enum
    if (auto enforced = RewritingPolicy{}(disjunction, oldExprID, newExprID)) {
      return *enforced; // Also a good place to cache now
    }

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

    auto reachable = disjunction.isReachable;
    Disjunction newDisjunction{reachable};
    for (auto& disjunct : disjunction.disjuncts) {
      Disjunct newDisjunct{};
      std::transform(disjunct.conjunctIDs.begin(), disjunct.conjunctIDs.end(),
                  std::back_inserter(newDisjunct.conjunctIDs), rebuildConjunct);
      std::sort(newDisjunct.conjunctIDs.begin(), newDisjunct.conjunctIDs.end());
      auto from = std::unique(newDisjunct.begin(), newDisjunct.end());
      newDisjunct.conjunctIDs.erase(from, newDisjunct.end());
      newDisjunction.addDisjunct(newDisjunct);
    }
    llvm::errs() << "\nAfter rewrites, New Disjunction";
    newDisjunction.print(llvm::errs());
    return newDisjunction;
  }

  //Disjunction
  //dropConjunct(const Disjunction& disjunction, const ExprID oldExprID) {
  //  auto isBinaryExprID = [this](const ExprID exprID) -> bool {
  //    return GetExprType(exprID) == 2;
  //  };

  //  auto preOrderFind  = [this, &oldExprID, &isBinaryExprID](const Conjunct& conjunct) -> bool {
  //    std::stack<ExprID> exprStack;
  //    exprStack.push(conjunct.exprID);
  //    while (!exprStack.empty()) {
  //      auto exprID = exprStack.top();
  //      exprStack.pop();
  //      if (oldExprID == exprID){
  //        return true;
  //      }
  //      if (!isBinaryExprID(exprID)) {
  //        continue;
  //      }
  //      auto binaryNode = GetBinaryExprNode(exprID);
  //      exprStack.push(binaryNode.lhs);
  //      exprStack.push(binaryNode.rhs);
  //    }
  //    return false;
  //  };

  //  Disjunction newDisjunction = disjunction; // Return new by value
  //  for (auto& disjunct : newDisjunction.disjuncts) {
  //    std::replace_if(disjunct.conjunctIDs.begin(), disjunct.conjunctIDs.end(),
  //                    preOrderFind, Conjunct{GetVacuousExprID(), true});
  //    std::sort(disjunct.conjunctIDs.begin(), disjunct.conjunctIDs.end());
  //  }
  //  return newDisjunction;
  //}

  // Accounts for state becoming true
  Disjunction
  pushToTrue(const Disjunction& disjunction, const ExprID oldExprID) {
    Disjunction copy = disjunction;
    for (auto& disjunct : copy) {
      auto from = std::remove_if(
          disjunct.begin(), disjunct.end(), [&](const auto& conjunct) {
            return find(conjunct, oldExprID);
          });
      disjunct.conjunctIDs.erase(from, disjunct.end());

      if (!disjunct.size()) {  // Drop state to true
        Disjunct disjunct{};
        disjunct.addConjunct(GetVacuousConjunct());
        copy.disjuncts = Disjuncts{disjunct};
        break;
      }
    }

    if (!(copy == disjunction)) {
      // Track states at approximation locations
      approximationMap[location] = oldExprID;
    }
    return copy;
  }

  Disjunction
  pushAllToTrue(const Disjunction& disjunction) {
    Disjunction copy = disjunction;
    Disjunct disjunct{};
    disjunct.addConjunct(this->GetVacuousConjunct());
    copy.disjuncts = Disjuncts{disjunct};
    if (!(copy == disjunction)) {
      approximationMap[location] = 0;
    }
    return copy;
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
  void 
  inOrderFor(const ExprID exprID, Visitor& visitor) {
    auto inOrder = [&visitor, this](const ExprID& exprID, auto inOrder) -> void {
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

    inOrder(exprID, inOrder);
    return;
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

  using AbstractState = std::array<Disjunction, COUNT - 1>;
  using States = std::vector<AbstractState>;

  auto&
  getApproxMap() {
    return stateApproxMap;
  }

  auto
  hasApproximation(llvm::Instruction* inst) { 
    return approximationMap.find(inst) 
      != approximationMap.end();
  }

  void
  storePreApproxState(const AbstractState& preApproxState, llvm::Instruction* inst) {
    if (approximationMap.count(inst)) {
      stateApproxMap[inst].push_back(preApproxState);
    }
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

  llvm::Instruction* location = nullptr;
  using Origin = llvm::Instruction*;

  llvm::DenseMap<ExprID, Origin> originMap;

  llvm::DenseMap<llvm::Instruction*, States> stateApproxMap;
  llvm::DenseMap<llvm::Instruction*, ExprID> approximationMap;
  
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

    auto* location = getLocation();
    originMap[constantExprCounter] = location;
    return constantExprCounter++; 
  }

  ExprID
  GenerateValueExprID(llvm::Value* const value) {
    valueSlab.emplace_back(value);
    leafTable.insert({value, valueExprCounter});

    auto* location = getLocation();
    originMap[valueExprCounter] = location;
    return valueExprCounter++;
  }

  ExprID
  GenerateBinaryExprID(ExprKey key, llvm::Value* const value) {
    debugger.increment(std::get<1>(key));
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

    auto* location = getLocation();
    originMap[binaryExprCounter] = location;
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

  class Debugger {
  public:
    std::unordered_map<int, size_t> exprCountMap;
    std::unordered_map<int, size_t> leafExprCountMap;
    llvm::DenseMap<OpKey, const char*> opMap;

    void
    increment(int code) {
      exprCountMap[code]++;
    }
  
    void
    dumpBinExpr(llvm::raw_ostream& outs) {
      for (auto& [code, count] : exprCountMap) {
        outs << "\n" << opMap[code] << "," << count;
      }
    }
  
    void
    initializeMap() {
      #define HANDLE(a, b, c) opMap.try_emplace(a, #b);
      #include "OperatorStrings.def"
      #undef HANDLE
    }
  };  // end Debugger
  Debugger debugger;
};  // end Generator

#endif
