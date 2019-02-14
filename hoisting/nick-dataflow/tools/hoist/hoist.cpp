#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SparseBitVector.h"

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/CFLSteensAliasAnalysis.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolutionAliasAnalysis.h"
#include "llvm/Analysis/ScopedNoAliasAA.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TypeBasedAliasAnalysis.h"

#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"

#include "llvm/IRReader/IRReader.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"


#include <bitset>
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <unordered_set>


#include "CustomDataflowAnalysis.h"
#include "TaintAnalysis.h"
#include "FutureFunctions.h"
#include "ConditionList.h"
#include "Generator.h"
#include "Printer.h"
#include "IndirectCallResolver.h"

using namespace llvm;


using std::string;
using std::unique_ptr;

static cl::OptionCategory futureFunctionsCategory{"future functions options"};

static cl::opt<string> inPath{cl::Positional,
                              cl::desc{"<Module to analyze>"},
                              cl::value_desc{"bitcode filename"},
                              cl::init(""),
                              cl::Required,
                              cl::cat{futureFunctionsCategory}};


std::unordered_map<std::string, FunctionPledges> libCHandlers;
std::unique_ptr<Generator> generator;
std::unique_ptr<IndirectCallResolver> resolver;
std::unique_ptr<Printer> printer;
llvm::DenseMap<const llvm::Function*, std::unique_ptr<llvm::MemorySSA>> functionMemSSAs;
llvm::DenseMap<const llvm::Function*, llvm::AAResultsWrapperPass*> functionAAs;

using Edge  = std::pair<const llvm::BasicBlock*,const llvm::BasicBlock*>;
using Edges = llvm::SmallVector<Edge, 10>;
llvm::DenseMap<const llvm::Function*, Edges> functionBackEdges;

using DisjunctionValue  = Disjunction;
using DisjunctionState  = analysis::AbstractState<DisjunctionValue>;
using DisjunctionResult = analysis::DataflowResult<DisjunctionValue>;


Edges
getBackedges(const llvm::Function* func) {
  return functionBackEdges[func];
}


static void
setRequiredPrivileges(Privileges requiredPrivileges, llvm::CallSite cs, const Context& context) {
  auto fun = cs.getCalledFunction();
  if (!fun) {
    return;
  }

  auto functionName = fun->getName().str();
  auto found = libCHandlers.find(functionName);
  if(found != libCHandlers.end()) {
    //Use the context of the current callsite
    auto promisesBitset = found->second.getPromisesBitset(cs, context);
    requiredPrivileges |= promisesBitset;
  }
}


class DisjunctionMeet : public analysis::Meet<DisjunctionValue, DisjunctionMeet> {
public:
  DisjunctionValue
  meetPair(DisjunctionValue& s1, DisjunctionValue& s2) const {
    auto vacuousConjunct = generator->GetVacuousConjunct();
    return Disjunction::unionDisjunctions(s1,s2)
            .simplifyTrues()
            .simplifyAdjacentNegation()
            .simplifyNeighbourNegation(vacuousConjunct)
            .simplifyImplication()
            .simplifyUnique();
  }
};


class DisjunctionEdgeTransformer {
public:
  Disjunction
  operator()(const Disjunction& toMerge,
             llvm::Value* branchAsValue,
             llvm::Value* destination) {
    // Use the label of the basic block of the branch to replace the appropriate
    // operand of the phi
    auto getAssocValue = [&branchAsValue](llvm::PHINode* const phi) {
      auto* basicBlock =
          llvm::dyn_cast<llvm::Instruction>(branchAsValue)->getParent();
      return phi->getIncomingValueForBlock(basicBlock);
    };
    auto isUsedPhi = [](llvm::PHINode& phi) -> bool {
      return generator->isUsed(&phi);
    };
    auto handlePhi =
        [&getAssocValue, &destination, &isUsedPhi](Disjunction destState) {
          auto destBlock = llvm::dyn_cast<llvm::BasicBlock>(destination);
          for (auto& phi : destBlock->phis()) {
            if (!isUsedPhi(phi)) {
              continue;
            }
            auto phiAsValue    = llvm::dyn_cast<llvm::Value>(&phi);
            auto phiExprID     = generator->GetOrCreateExprID(phiAsValue);
            auto phiOperand    = getAssocValue(&phi);
            auto operandExprID = generator->GetOrCreateExprID(phiOperand);
            destState = generator->rewrite(destState, phiExprID, operandExprID);
          }
          return destState;
        };

    auto branchOrSwitch    = llvm::dyn_cast<llvm::Instruction>(branchAsValue);
    auto isConditionalJump = [&branchOrSwitch]() {
      return branchOrSwitch->getNumOperands() > 2;
    };

    auto edgeOp = [&](Disjunction destState) {
      destState = handlePhi(destState);
      if (!isConditionalJump()) {
        return destState;
      }
      return handle(branchOrSwitch, destState, destination);
    };

    llvm::errs() << "\n Before edge op";
    toMerge.print(llvm::errs());
    auto temp = edgeOp(toMerge);
    llvm::errs() << "\n After edge op";
    temp.print(llvm::errs());
    // return edgeOp(toMerge);
    return temp;
  }

private:
  //TODO: Ask Nick about perfect forwarding
  Disjunction
  handle(llvm::Instruction* branchOrSwitch, Disjunction& destState, llvm::Value* destination) {
    if (auto branchInst = llvm::dyn_cast<llvm::BranchInst>(branchOrSwitch)) {
      return handleAsBranch(branchInst, destState, destination);
    } else {
      auto switchInst = llvm::dyn_cast<llvm::SwitchInst>(branchOrSwitch);
      return handleAsSwitch(switchInst, destState, destination);
    } 
    return destState;
  }

  Disjunction
  handleAsSwitch(llvm::SwitchInst* switchInst, Disjunction& destState, llvm::Value* destination) {
    auto getConjunctForm = [&](auto& caseOp) {
      llvm::BasicBlock* targetBB = caseOp.getCaseSuccessor();
      return targetBB == llvm::dyn_cast<BasicBlock>(destination);
    };
    auto getCaseExprID = [&](auto& caseOp) {
      llvm::Constant* caseVal = caseOp.getCaseValue();
      auto caseValueAsExprID  = generator->GetOrCreateExprID(caseVal);
      auto* condition         = switchInst->getCondition();
      auto conditionAsExprID  = generator->GetOrCreateExprID(condition);

      ExprKey key{conditionAsExprID, OpIDs::switchOp, caseValueAsExprID};
      auto form = getConjunctForm(caseOp);
      return std::make_pair(generator->GetOrCreateExprID(key, switchInst), form);
    };

    Disjunction combined{};
    for (auto& caseOp : switchInst->cases()) {
      auto  stateCopy = destState;
      auto  [asExprID, form]  = getCaseExprID(caseOp);
      stateCopy.applyConjunct({asExprID, form});
      combined = Disjunction::unionDisjunctions(combined, stateCopy);
    }
    return combined;
  }
  Disjunction
  handleAsBranch(llvm::BranchInst* branchInst, Disjunction destState, llvm::Value* destination) {
    auto conjunctForm = [&] ( ) {
      return destination == branchInst->getOperand(2);
    };
    auto* condition        = branchInst->getCondition();
    auto conditionAsExprID = generator->GetOrCreateExprID(condition);
    destState.applyConjunct({conditionAsExprID, conjunctForm()});
    return destState;
  }
};


class DisjunctionTransfer {
private:
  bool
  handleCallSite(const llvm::CallSite& cs, DisjunctionState& state, const Context& context) {
    if (!cs.getInstruction()) {
      return false;
    }
    const auto* fun = getCalledFunction(cs);
    if (!fun) {
      return false;
    }
    llvm::errs() << "\tFrom CallSite: " << fun->getName();
    if (fun->getName().startswith("llvm.")) {
      return true;
    }

    if (fun->getName().startswith("wait")) {
      auto vacExpr    = generator->GetVacuousExprID();
      Disjunction disjunction{};
      Disjunct disjunct{};
      disjunct.addConjunct(generator->GetVacuousConjunct());
      disjunction.addDisjunct(disjunct);
      state[nullptr] = disjunction;
      return true;
    } 
    if (fun->isDeclaration()) {
      return true;
    }

    state[nullptr] = state[cs.getInstruction()];
    using argParamPair = std::pair<llvm::Value*, llvm::Value*>;
    std::vector<argParamPair> argPairs;
    { int i = 0;
      for (auto& param : fun->args()) {
        auto* paramAsValue = (llvm::Value*) &param;
        auto arg = cs.getArgument(i);
        argPairs.push_back(argParamPair{arg, paramAsValue});
        i++;
      }
    };

    auto rewritePair = [&](auto* arg, auto* param) {
      auto argExprID   = generator->GetOrCreateExprID(arg);
      auto paramExprID = generator->GetOrCreateExprID(param);
      return generator->rewrite(state[nullptr], paramExprID, argExprID);
    };

    for (auto [arg, param] : argPairs) {
      state[nullptr] = rewritePair(arg, param);
    }
    return true;
  }

  bool
  handleBinaryOperator(llvm::Value* const value, DisjunctionState& state) {
    auto* const binOp = llvm::dyn_cast<llvm::BinaryOperator>(value);
    if (!binOp) {
      return false;
    }
    auto oldExprID = generator->GetOrCreateExprID(value);
    auto exprID    = generator->GetOrCreateExprID(binOp);
    if (oldExprID == exprID) {
      return false;
    }
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, exprID);
    return true;
  }

  bool
  handleCmpInst(llvm::Value* const value, DisjunctionState& state) {
    auto cmpInst = llvm::dyn_cast<llvm::CmpInst>(value);
    if (!cmpInst) {
      return false;
    }

    auto oldExprID = generator->GetOrCreateExprID(value);
    auto exprID = generator->GetOrCreateExprID(cmpInst);
    if (oldExprID == exprID) {
      return false;
    }
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, exprID);
    return true;
  }

  bool
  handleLoad(llvm::Value* const value, DisjunctionState& state) {
    auto* loadInst = llvm::dyn_cast<llvm::LoadInst>(value);
    if (!loadInst) {
      return false;
    }
    auto oldExprID = generator->GetOrCreateExprID(value);
    auto newExprID = generator->GetOrCreateExprID(loadInst);
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, newExprID);
    return true;
  }

  std::vector<llvm::LoadInst*>
  getLoads(DisjunctionState& state) {
    std::unordered_set<llvm::LoadInst*> asLoads;
    auto isBinaryExprID = [](const ExprID exprID) -> bool {
      return generator->GetExprType(exprID) == 2;
    };
    auto insertIfLoad = [&asLoads](const auto binaryNode) {
      if (auto loadInst =
              llvm::dyn_cast<llvm::LoadInst>(binaryNode.instruction)) {
        asLoads.insert(loadInst);
      }
    };
    auto findLoads = [&](const ExprID exprID, auto& findLoads) {
      if (!isBinaryExprID(exprID)) {
        return;
      }
      auto& binaryNode = generator->GetBinaryExprNode(exprID);
      insertIfLoad(binaryNode);
      findLoads(binaryNode.lhs, findLoads);
      findLoads(binaryNode.rhs, findLoads);
      return;
    };

    for (auto& disjunct : state[nullptr].disjuncts) {
      for (auto& conjunct : disjunct.conjunctIDs) {
        findLoads(conjunct.exprID, findLoads);
      }
    }
    return {asLoads.begin(), asLoads.end()};
  }


  AliasResult
  getMemSSAResults(llvm::StoreInst* const storeInst,
                   llvm::LoadInst* const loadInst) {
    auto [storeFunc, loadFunc] = [&]() {
      return std::make_pair(storeInst->getFunction(), loadInst->getFunction());
    }();
    if (storeFunc != loadFunc) {
      return AliasResult::MayAlias;
    } // Conservatively assert that mem-ops from different functions alias

    auto [walker, memSSA] = [&]() {
      auto* func = storeInst->getFunction();
      return std::make_pair(functionMemSSAs[func]->getWalker(),
                            functionMemSSAs[func].get());
    }();
    assert(memSSA != nullptr && "No memSSA for function");

    auto* clobber     = walker->getClobberingMemoryAccess(loadInst);
    auto* storeMemAcc = memSSA->getMemoryAccess(storeInst);
    auto* storeMemDef = llvm::dyn_cast<llvm::MemoryDef>(storeMemAcc);
    for (auto memDef = clobber->defs_begin(); memDef != clobber->defs_end(); memDef++) {
      if (memDef == storeMemDef) {
        return AliasResult::MustAlias;
      }
    }
    //llvm_unreachable("Part of the same function, but does not alias");
    return AliasResult::NoAlias;
  }

  AliasResult
  getAliasType(llvm::StoreInst* const storeInst,
               llvm::LoadInst* const loadInst) {
    auto* storeFunction = storeInst->getFunction();
    auto* AAWrapper     = functionAAs[storeFunction];
    auto& AAResults = AAWrapper->getAAResults();
    assert(AAWrapper != nullptr && "AA is nullptr");

    auto loadAsMemLoc  = llvm::MemoryLocation::get(loadInst);
    auto storeAsMemLoc = llvm::MemoryLocation::get(storeInst);
    auto aliasType = AAResults.alias(loadAsMemLoc, storeAsMemLoc);
    if (aliasType == AliasResult::MustAlias
        /*|| aliasType == AliasResult::NoAlias*/) {
      return aliasType;
    }
    //Consult MemSSA for may-alias
    return getMemSSAResults(storeInst, loadInst);
  }

  auto
  seperateLoads(llvm::StoreInst* const storeInst, std::vector<LoadInst*>& loads) {
    std::vector<llvm::LoadInst*> otherLoads;
    std::vector<llvm::LoadInst*> strongLoads;

    auto distill = [&] (llvm::LoadInst* loadInst) {
      auto aliasResult = getAliasType(storeInst, loadInst);
      if (aliasResult == AliasResult::MustAlias) {
        strongLoads.push_back(loadInst);
      } else if (aliasResult == AliasResult::NoAlias) {
        return; //Discard NoAliases
      } else {
        otherLoads.push_back(loadInst);
      }
    };

    for (auto* loadInst : loads) {
      distill(loadInst);
    }
    return std::make_pair(strongLoads, otherLoads);
  }

  Disjunction
  strongUpdate(const Disjunction& disjunction,
               llvm::LoadInst* const loadInst,
               llvm::StoreInst* const storeInst) {
    auto loadExprID = generator->GetOrCreateExprID(loadInst);
    auto operand    = storeInst->getValueOperand();
    auto opExprID   = generator->GetOrCreateExprID(operand);
    return generator->rewrite(disjunction, loadExprID, opExprID);
  }

  Disjunction
  weakUpdate(const Disjunction& disjunction,
             llvm::LoadInst* const loadInst,
             llvm::StoreInst* const storeInst) {
    auto storeValue = storeInst->getValueOperand();
    auto valExprID  = generator->GetOrCreateExprID(storeValue);
    // TODO: Move aliasConjunct creation to the generator
    auto aliasConjunct = [&]() {
      auto* loadPtr  = loadInst->getPointerOperand();
      auto ptrExprID = generator->GetOrCreateExprID(loadPtr);
      ExprKey aliasKey{ptrExprID, OpIDs::aliasOp, valExprID};
      auto aliasExpr = generator->GetOrCreateExprID(aliasKey, storeInst);
      return Conjunct(aliasExpr, true);
    }();
    auto loadExprID = generator->GetOrCreateExprID(loadInst);

    Disjunction forRewrites{};
    Disjunction noRewrites{disjunction};
    for (auto& disjunct : disjunction.disjuncts) {
      auto aliasDisjunct{disjunct};
      auto notAliasDisjunct{disjunct};
      for (auto& conjunct : disjunct.conjunctIDs) {
        if (generator->find(conjunct, loadExprID)) {
          aliasDisjunct.addConjunct(aliasConjunct);
          notAliasDisjunct.addConjunct(!aliasConjunct);
          forRewrites.addDisjunct(aliasDisjunct);
          noRewrites.addDisjunct(notAliasDisjunct);
        }
      }
    }
    auto rewritten  = generator->rewrite(forRewrites, loadExprID, valExprID);
    return Disjunction::unionDisjunctions(rewritten, noRewrites);
  }

  bool
  handleStore(llvm::Value* const value,
              DisjunctionState& state,
              const Context context) {
    auto* storeInst = llvm::dyn_cast<llvm::StoreInst>(value);
    if (!storeInst) {
      return false;
    }
    llvm::errs() << "\n Printing at store";
    //state[nullptr].print(llvm::errs());
    std::vector<llvm::LoadInst*> asLoads = getLoads(state);
    auto [strongLoads, weakLoads]        = seperateLoads(storeInst, asLoads);

    auto localDisjunction = state[nullptr];
    for (auto* strongLoad : strongLoads) {
      localDisjunction = strongUpdate(localDisjunction, strongLoad, storeInst);
    }
    for (auto weakLoad : weakLoads) {
      localDisjunction = weakUpdate(localDisjunction, weakLoad, storeInst);
    }
    state[nullptr] = localDisjunction;
    return true;
  }

  bool
  handlePhiBackEdges(llvm::Value* value, DisjunctionState& state) {
    auto* phi = llvm::dyn_cast<llvm::PHINode>(value);
    if (!phi) {
      return false;
    }
    auto isFromBackEdge =
      [&phi](const llvm::BasicBlock* incomingBlock) -> bool {
      auto* parentBlock    = phi->getParent();
      auto* parentFunction = parentBlock->getParent();
      auto backEdges       = getBackedges(parentFunction);
      auto foundIt         = std::find_if(
          backEdges.begin(), backEdges.end(), [&](const auto& bbPair) {
            return bbPair.first == incomingBlock
                   && bbPair.second == parentBlock;
          });
      return foundIt != backEdges.end();
    };

    bool isLoopPhi = false;
    for (auto& incomingBlock : phi->blocks()) {
      isLoopPhi |= isFromBackEdge(incomingBlock);
    }

    if(!isLoopPhi) {
      return true;
    }
    llvm::errs() << "\n Dropping loop conjunct";
    auto phiExprID = generator->GetOrCreateExprID(value);
    state[nullptr] = generator->pushToTrue(state[nullptr], phiExprID);
    state[nullptr] = state[nullptr].simplifyImplication();

    return true;
  }

  bool
  handleGep(llvm::Value* const value, DisjunctionState& state) {
    auto* gep = llvm::dyn_cast<llvm::GetElementPtrInst>(value);
    if (!gep) {
      return false;
    }
    auto oldExprID = generator->GetOrCreateExprID(value);
    auto gepExprID = generator->GetOrCreateExprID(gep);
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, gepExprID);
    return true;
  }

  bool
  handleRet(llvm::Value* const value, DisjunctionState& state, const Context context) {
    auto* ret = llvm::dyn_cast<llvm::ReturnInst>(value);
    if (!ret) {
      return false;
    }
    //llvm::errs() << "\nPrinting return state";
    //state[ret].print(llvm::errs());
    //llvm::errs() << "\nPrinting return state";

    state[nullptr] = state[ret];
    llvm::Value* callAsValue = nullptr;
    for (auto* inst : context) {
      if (inst != nullptr) {
        callAsValue = llvm::dyn_cast<llvm::Value>(inst);
      }
    }

    if (!generator->isUsed(callAsValue)) {
      return true;
    }


    auto* retValue = ret->getReturnValue();
    auto newExprID = generator->GetOrCreateExprID(retValue);
    auto oldExprID = generator->GetOrCreateExprID(callAsValue);
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, newExprID);

    return true;
  }

  void
  handleUnknown(llvm::Value* const value, DisjunctionState& state) {
    if (!generator->isUsed(value)) {
      return;
    }
    auto oldLeafTableSize = generator->getLeafTableSize();

    llvm::errs() << "Dropping Unknown";
    auto oldExprID = generator->GetOrCreateExprID(value);
    state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);

    assert(oldLeafTableSize == generator->getLeafTableSize() && "New Value at Unknown");
  }

public:
  void
  operator()(llvm::Value& value, DisjunctionState& state, const Context& context) {
    auto* inst = llvm::dyn_cast<llvm::Instruction>(&value);

    llvm::errs() << "\nBefore------------------------------------";
    llvm::errs() << "\nIn function " << inst->getFunction()->getName()
                 << " \nBefore";
    llvm::errs() << *inst;
    llvm::errs() << "\n State:\n";
    state[nullptr].print(llvm::errs());
    llvm::errs() << "\n------------------------------------------";

    bool handled = false;
    handled |= handlePhiBackEdges(&value, state);
    handled |= handleCallSite(llvm::CallSite{&value}, state, context);
    handled |= handleStore(&value, state, context);
    handled |= handleGep(&value, state);
    handled |= handleRet(&value, state, context);

    if (handled) {
      //generator->dumpState();
      //printer->printState(inst, state[nullptr]);
      return;
    }
    if (!generator->isUsed(&value)) {
      //generator->dumpState();
      //printer->printState(inst, state[nullptr]);
      return;
    }
    handled |= handleLoad(&value, state);
    handled |= handleBinaryOperator(&value, state);
    handled |= handleCmpInst(&value, state);

    if (!handled) {
      handleUnknown(&value, state);
    }
    //generator->dumpState();
    //printer->printState(inst, state[nullptr]);
    return;
  }
};


class BuildPromiseTreePass : public llvm::ModulePass {
public:
  BuildPromiseTreePass()
    : llvm::ModulePass{ID}
      { }

  bool runOnModule(llvm::Module& m) override;
  void getAnalysisUsage(llvm::AnalysisUsage &info) const override;
  StringRef getPassName() const override;

  static char ID;
};
char BuildPromiseTreePass::ID = 0;

StringRef
BuildPromiseTreePass::getPassName() const {
  return "BuildPromiseTreePass";
}


bool
BuildPromiseTreePass::runOnModule(llvm::Module& m) {
  auto* mainFunction = m.getFunction("main");
  if (!mainFunction) {
    llvm::report_fatal_error("Unable to find main function.");
  }

  generator = make_unique<Generator>(Generator{});
  resolver  = make_unique<IndirectCallResolver>(IndirectCallResolver{m});
  printer   = make_unique<Printer>(generator.get(), llvm::outs());


  for (auto& f : m) {
    if ( f.isDeclaration()) {
      continue;
    }
    llvm::errs() << "\n Get Dom Tree for " << f.getName() << "\n";
    auto* DT = &getAnalysis<DominatorTreeWrapperPass>(f).getDomTree();
    llvm::errs() << "\n Get AA Tree for " << f.getName() << "\n";
    auto* AAWrapper = &getAnalysis<AAResultsWrapperPass>(f);
    llvm::errs() << "\n Get memSSA Tree for " << f.getName() << "\n";
    auto memSSA = std::make_unique<MemorySSA>(f, &AAWrapper->getAAResults(), DT);
    functionAAs.insert({&f, AAWrapper});

    functionMemSSAs.try_emplace(&f, std::move(memSSA));

    //memSSA->print(llvm::outs());
    Edges backedges;
    llvm::FindFunctionBackedges(f, backedges);
    functionBackEdges.try_emplace(&f, backedges);
  }


  using Value    = Disjunction;
  using Transfer = DisjunctionTransfer;
  using Meet     = DisjunctionMeet;
  using EdgeTransformer = DisjunctionEdgeTransformer;
  using Analysis = analysis::DataflowAnalysis<Value, Transfer, Meet, EdgeTransformer, analysis::Backward>;
  Analysis analysis{m, mainFunction};

  //AnalysisPackage package;
  //package.tmppathResults = tmpanalysis::gettmpAnalysisResults(m);
  //libCHandlers   = getLibCHandlerMap(package);
  auto results  = analysis.computeDataflow();

  return false;
}

void
BuildPromiseTreePass::getAnalysisUsage(llvm::AnalysisUsage &info) const {
  //info.setPreservesAll();
  info.addRequired<DominatorTreeWrapperPass>();
  info.addRequired<AAResultsWrapperPass>();
  info.addPreserved<AAResultsWrapperPass>();
  //info.addRequired<AliasAnalysis>();
  //info.addRequired<PostDominatorTreeWrapperPass>();
}


static void
instrumentPromiseTree(llvm::Module& m) {
  llvm::DebugFlag = true;
  legacy::PassManager pm;
  pm.add(createTypeBasedAAWrapperPass());
  pm.add(createGlobalsAAWrapperPass());
  pm.add(createSCEVAAWrapperPass());
  pm.add(createScopedNoAliasAAWrapperPass());
  pm.add(createCFLSteensAAWrapperPass());
  pm.add(new llvm::LoopInfoWrapperPass());
  //pm.add(createPostDomTree());
  //pm.add(new MemorySSAWrapperPass());
  pm.add(new DominatorTreeWrapperPass());
  pm.add(createBasicAAWrapperPass());
  pm.add(new AAResultsWrapperPass());
  pm.add(new BuildPromiseTreePass());
  pm.run(m);
}


int
main(int argc, char** argv) {
  // This boilerplate provides convenient stack traces and clean LLVM exit
  // handling. It also initializes the built in support for convenient
  // command line option handling.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  llvm::PrettyStackTraceProgram X(argc, argv);
  llvm_shutdown_obj shutdown;
  //cl::HideUnrelatedOptions(futureFunctionsCategory);
  cl::ParseCommandLineOptions(argc, argv);

  // Construct an IR file from the filename passed on the command line.
  SMDiagnostic err;
  LLVMContext context;
  unique_ptr<Module> module = parseIRFile(inPath.getValue(), err, context);

  if (!module.get()) {
    errs() << "Error reading bitcode file: " << inPath << "\n";
    err.print(argv[0], errs());
    return -1;
  }


  instrumentPromiseTree(*module);
  return 0;
}
