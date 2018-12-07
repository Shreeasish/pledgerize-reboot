#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SparseBitVector.h"

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
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
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"

#include "llvm/IRReader/IRReader.h"

#include "llvm/Support/CommandLine.h"
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


#include "CustomDataflowAnalysis.h"
#include "TaintAnalysis.h"
#include "FutureFunctions.h"
#include "ConditionList.h"
#include "Generator.h"
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
// TODO: Store walkers directly
llvm::DenseMap<const llvm::Function*, llvm::MemorySSA*> functionMemSSAs;

using DisjunctionValue  = Disjunction;
using DisjunctionState  = analysis::AbstractState<DisjunctionValue>;
using DisjunctionResult = analysis::DataflowResult<DisjunctionValue>;


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
    return Disjunction::unionDisjunctions(s1,s2)
            .simplifyAdjacentNegation()
            .simplifyNeighbourNegation();
  }
};


class DisjunctionEdgeTransformer {
public:
  Disjunction
  operator()(const Disjunction& toMerge, llvm::Value* branchAsValue, llvm::Value* destination) {
    // Use the label of the basic block of the branch to replace the appropriate operand of the phi

    auto getAssocValue = [&branchAsValue] (const llvm::PHINode* phi) {
      auto* basicBlock = llvm::dyn_cast<llvm::BranchInst>(branchAsValue)->getParent();
      return phi->getIncomingValueForBlock(basicBlock);
    };

    auto isUsedPhi = [] (const llvm::PHINode& phi) -> bool {
      return generator->isUsed(&phi);
    };

    auto handlePhi = [&getAssocValue, &destination, &isUsedPhi](Disjunction destState) {
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

    auto asBinaryExprID = [](llvm::Value* branchCondition) -> ExprID {
      auto* binOp
        = llvm::dyn_cast<llvm::BinaryOperator>(branchCondition);
      if (binOp) { // Does this actually happen?
        return generator->GetOrCreateExprID(binOp);
      }
      auto* cmpInst
        = llvm::dyn_cast<llvm::CmpInst>(branchCondition);
      if (cmpInst) {
        return generator->GetOrCreateExprID(cmpInst);
      }
      if (!cmpInst) { //change to last on the list
        llvm::errs() << *branchCondition;
        assert(false && "condition not handled");
      }
      return 0;
    };

    auto branchInst = llvm::dyn_cast<llvm::BranchInst>(branchAsValue);
    auto destAsBool = [&branchInst](auto* destination) -> bool {
      return destination == branchInst->getOperand(2);
    };
    //llvm::errs() << "Crashing at: \n" << *branchAsValue;
    auto edgeOp = [&branchInst, &asBinaryExprID, &destAsBool, &handlePhi, destination] (Disjunction destState) {
      Disjunction local{destState};
      local = handlePhi(destState);
      if ( branchInst->isUnconditional()) {
        return local;
      }
      auto* branchCondition = branchInst->getCondition();
      local.applyConjunct({asBinaryExprID(branchCondition), destAsBool(destination)});
      return local;
    };
    return edgeOp(toMerge);
  }
};


class DisjunctionTransfer {
private:
  bool
  handleCallSite(const llvm::CallSite& cs, DisjunctionState& state, const Context& context) {
    const auto* fun = getCalledFunction(cs);
    if (!fun) {
      return false;
    }

    if (fun->getName().startswith("llvm.")) {
      return false;
    }

    if (fun->getName().startswith("special")) {
      llvm::errs() << "\n not skipping call" << fun->getName();
      return false;
    }

    auto asDisjunct = [](Conjunct conjunct) -> Disjunct {
      auto d = Disjunct{};
      d.addConjunct(conjunct);
      return d;
    };

    auto vacExpr = generator->GetVacuousExprID();
    state[nullptr].addDisjunct(asDisjunct({vacExpr,true}));
    return true;
  }

  bool
  handleBinaryOperator(const llvm::Value* value, DisjunctionState& state) {
    auto* binOp = llvm::dyn_cast<llvm::BinaryOperator>(value);
    if (!binOp) {
      return false;
    }
    auto oldExprID = generator->GetOrCreateExprID(value);
    // TODO: Move to on-the-fly creation
    auto exprID    = generator->GetOrCreateExprID(binOp);
    if (oldExprID == exprID) {
      return false;
    }
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, exprID);
    return true;
  }

  /// Loads and Stores:
  /// Loads are handled by replacing the Load Value ExprID in the ExprTree
  /// with Value ExprID for the Alloca (pointer operand of the load).
  /// This in turn will be replaced by the Value operand of a store.
  /// The alloca value ExprID is therefore, transient.
  bool
  skipPhi(const llvm::Value* value, DisjunctionState& state) {
    auto* phi = llvm::dyn_cast<llvm::PHINode>(value);
    if (phi) {
      return true;
    }
    return false;
  }


  // Treat loadInsts as Value Nodes
  bool
  handleLoad(const llvm::Value* value, DisjunctionState& state) {
    auto* loadInst = llvm::dyn_cast<llvm::LoadInst>(value);
    if (!loadInst) {
      return false;
    }
    return true;
  }

  bool
  handleStore(const llvm::Value* value, DisjunctionState& state) {
    auto getWalkerAndMSSA = [](const llvm::Instruction* inst) {
      auto* func = inst->getFunction();
      return std::make_pair(functionMemSSAs[func]->getWalker(),
          functionMemSSAs[func]);
    };

    auto* storeInst = llvm::dyn_cast<llvm::StoreInst>(value);
    if (!storeInst) {
      return false;
    }   


    llvm::errs() << "\n Handling store \n" << *value;
    auto [walker, memSSA] = getWalkerAndMSSA(storeInst);
    assert(memSSA != nullptr && "No memSSA for function");


    std::vector<const llvm::LoadInst*> asLoads;
    auto isValueExprID = [](const ExprID exprID) {
      return generator->GetExprType(exprID) == 1;
    };
    auto isBinaryExprID = [](const ExprID exprID) -> bool {
      return generator->GetExprType(exprID) == 2;
    };
    auto insertLoad = [&asLoads] (const ExprID exprID) {
      auto& valueNode = generator->GetValueExprNode(exprID);
      asLoads.push_back(llvm::dyn_cast<llvm::LoadInst>(valueNode.value));
    };
    auto preOrder = [isValueExprID, isBinaryExprID, insertLoad] (const ExprID exprID, auto& preOrder) {
      if (isValueExprID(exprID)) {
        insertLoad(exprID);
        return;
      } else if (isBinaryExprID(exprID)) {
        auto& binaryNode = generator->GetBinaryExprNode(exprID);
        preOrder(binaryNode.lhs, preOrder);
        preOrder(binaryNode.rhs, preOrder);
      }
      return;
    };
    for (auto& disjunct : state[nullptr].disjuncts) {
      for (auto& conjunct : disjunct.conjunctIDs) {
        preOrder(conjunct.exprID, preOrder);
      }
    }

    auto eraseFrom = std::remove_if(asLoads.begin(), asLoads.end(), [ ] (const auto& loadInst) {
        return loadInst == nullptr;
        });
    asLoads.erase(eraseFrom, asLoads.end());

    llvm::errs() << "\n --------------- MEMSSA --------------- \n";

    std::vector<llvm::MemoryAccess*> clobberingAccesses;
    auto toMemoryAccess = [&walker = walker] (const auto& loadInst) {
      return walker->getClobberingMemoryAccess(loadInst);
    };
    std::transform(asLoads.begin(), asLoads.end(), std::back_inserter(clobberingAccesses), toMemoryAccess);

    using memoryDefs = std::vector<llvm::MemoryDef*>;
    std::vector<memoryDefs> clobberingDefs;
    auto toClobberingDefs = [](llvm::MemoryAccess* memAccess) {
      if (auto* memDef = llvm::dyn_cast<llvm::MemoryDef>(memAccess)) {
        return memoryDefs{memDef};
      }
      memoryDefs asMemDefs;
      auto memDefs = [](const auto& memAccess) {
        return llvm::dyn_cast<llvm::MemoryDef>(memAccess);
      };
      std::transform(memAccess->defs_begin(), memAccess->defs_end(), std::back_inserter(asMemDefs), memDefs);
      return asMemDefs;
    };
    std::transform(clobberingAccesses.begin(), clobberingAccesses.end(), std::back_inserter(clobberingDefs), toClobberingDefs);
    llvm::errs() << "\n --------------- MEMSSA --------------- \n";

    auto* storeMemDef = llvm::dyn_cast<llvm::MemoryDef>(memSSA->getMemoryAccess(storeInst));
    for (auto& defs : clobberingDefs) {
      for (auto& def : defs) {
        if (def == storeMemDef) {
          llvm::errs() << "Yippie Kay Ay Mother***!" ;
        }
      }
    }

    auto valueOperandExprID = generator->GetOrCreateExprID(storeInst->getValueOperand());
    //state[nullptr]    = generator->rewrite(state[nullptr], allocaExprID, valueOperandExprID);
    return true;
  }

  // Only reaches this if the llvm::value is actually used somewhere else
  // i.e. GetOrCreateExprID should not create new exprIDs
  // Add an assert?
  void
  handleUnknown(const llvm::Value* value, DisjunctionState& state) {
    auto oldLeafTableSize = generator->getLeafTableSize();

    llvm::errs() << "\n Handling as Unknown" << *value;
    auto oldExprID = generator->GetOrCreateExprID(value);
    state[nullptr] = generator->dropConjunct(state[nullptr], oldExprID);

    assert(oldLeafTableSize == generator->getLeafTableSize() && "New Value at Unknown");
  }

public:
  void
  operator()(llvm::Value& value, DisjunctionState& state, const Context& context) {
    auto debugAfter = [&]() {
      llvm::errs() << "\n-----------------------Debugging----------------- ";
      llvm::errs() << "After Transfer \n";
      if (state.count(nullptr)) {
        state[nullptr].print();
      }
      generator->dumpState();
      llvm::errs() << "\n----------------------------Debugging End -------------------------\n" ;
      llvm::errs().flush();
    };

    auto debugBefore = [&]() {
      llvm::errs() << "\n-----------------------Debugging----------------- ";
      llvm::errs() << "Before Transfer \n";
      llvm::errs() << value;
      if (state.count(nullptr)) {
        state[nullptr].print();
      }
      generator->dumpState();
      llvm::errs() << "\n----------------------------Debugging End -------------------------\n" ;
      llvm::errs().flush() ;
    };

    debugBefore();
    bool handled = false;
    handled |= handleCallSite(llvm::CallSite{&value}, state, context);
    handled |= handleStore(&value, state);
    if(handled) {
      return;
    }
    if (!generator->isUsed(&value)) { //Global Check
      debugAfter();
      return;
    }
    handled |=  skipPhi(&value, state);
    handled |=  handleLoad(&value, state);
    handled |= handleBinaryOperator(&value, state);
    // handleUnknown(&value, state);
    if (!handled) {
      handleUnknown(&value, state);
    }
    debugAfter();

    return;
  }
};


static void
printLineNumber(llvm::raw_ostream& out, llvm::Instruction& inst) {
  if (const llvm::DILocation* debugLoc = inst.getDebugLoc()) {
    out << "At " << debugLoc->getFilename()
        << " line " << debugLoc->getLine()
        << ":\n";
  } else {
    out << "At an unknown location:\n";
  }
}


class BuildPromiseTreePass : public llvm::ModulePass {
public:
  BuildPromiseTreePass()
    : llvm::ModulePass{ID}
      { }

  bool runOnModule(llvm::Module& m) override;
  void getAnalysisUsage(llvm::AnalysisUsage &info) const override;

private:
  static char ID;
};
char BuildPromiseTreePass::ID = 0;


bool
BuildPromiseTreePass::runOnModule(llvm::Module& m) {
  auto* mainFunction = m.getFunction("main");
  if (!mainFunction) {
    llvm::report_fatal_error("Unable to find main function.");
  }

  generator = make_unique<Generator>(Generator{});
  resolver  = make_unique<IndirectCallResolver>(IndirectCallResolver{m});
  for (auto& f : m) {
    if ( f.isDeclaration()) {
      continue;
    }
    auto* memSSA = &getAnalysis<MemorySSAWrapperPass>(f).getMSSA();
    memSSA->print(llvm::outs());
    functionMemSSAs.insert({&f, memSSA});
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
  auto results   = analysis.computeDataflow();

  return false;
}

void
BuildPromiseTreePass::getAnalysisUsage(llvm::AnalysisUsage &info) const {
  info.addRequired<MemorySSAWrapperPass>();
  //info.addRequired<PostDominatorTreeWrapperPass>();
  //info.addRequired<LoopInfoWrapperPass>();
}

static void
instrumentPromiseTree(llvm::Module& m) {
  legacy::PassManager pm;
  pm.add(new llvm::LoopInfoWrapperPass());
  //pm.add(createBasicAAWrapperPass());
  //pm.add(createTypeBasedAAWrapperPass());
  //pm.add(createGlobalsAAWrapperPass());
  //pm.add(createSCEVAAWrapperPass());
  //pm.add(createScopedNoAliasAAWrapperPass());
  //pm.add(createCFLSteensAAWrapperPass());
  //pm.add(createPostDomTree());
  //pm.add(new DominatorTreeWrapperPass());
  pm.add(new MemorySSAWrapperPass());
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
  cl::HideUnrelatedOptions(futureFunctionsCategory);
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
