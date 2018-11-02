#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SparseBitVector.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"


#include <bitset>
#include <memory>
#include <string>
#include <iostream>
#include <variant>

#include "CustomDataflowAnalysis.h"
#include "FutureFunctions.h"
#include "IndirectCallResolver.h"
#include "TaintAnalysis.h"
#include "ConditionList.h"
#include "Generator.h"

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


//LibC map here
std::unordered_map<std::string, FunctionPledges> libCHandlers;

using DisjunctionValue  = Disjunction;
using DisjunctionState  = analysis::AbstractState<DisjunctionValue>;
using DisjunctionResult = analysis::DataflowResult<DisjunctionValue>;

std::unique_ptr<Generator> generator;

static void
setRequiredPrivileges(Privileges requiredPrivileges, llvm::CallSite cs, const Context& context) {
  auto fun = cs.getCalledFunction();
  if(!fun){
    return;
  }

  auto functionName = fun->getName().str();
  auto found = libCHandlers.find(functionName);
  if(found != libCHandlers.end()){
    //Use the context of the current callsite
    auto promisesBitset = found->second.getPromisesBitset(cs, context);
    requiredPrivileges |= promisesBitset;
  }
}


class DisjunctionMeet : public analysis::Meet<DisjunctionValue, DisjunctionMeet> {
public:
  DisjunctionValue
  meetPair(DisjunctionValue& s1, DisjunctionValue& s2) const {
    llvm::errs() << "\n Simple Meet Pair Called \n";
    return Disjunction::unionDisjunctions(s1,s2);
  }

  DisjunctionValue
  meetPairCustomized(DisjunctionValue& s1, DisjunctionValue& s2, //TODO: Branch Awareness
      llvm::Value* value1, llvm::Value* value2, llvm::Value* value3) const {
    llvm::errs() << "\n Meet Op \n";
    auto valuePrint = [](llvm::Value* value) -> void {
      if (value) {
        llvm::errs() << *value;
      }
      else {
        llvm::errs() << value;
      }
    };
    auto printall = [&value1,&value2,&value3,&valuePrint]() {
      llvm::errs() << "\n Print Value At Meet";
      llvm::errs() << "\nvalue1 "; valuePrint(value1);
      llvm::errs() << "\nvalue2 "; valuePrint(value2);
      llvm::errs() << "\nvalue3 "; valuePrint(value3);
    };
    // BinaryExpr dispatcher
    auto asBinaryExpr = [](llvm::BranchInst* branch) -> ExprID { //TODO: Handle Negation
      auto* binOp = llvm::dyn_cast<BinaryOperator>(branch->getCondition());
      if (binOp) {
        return generator->GetOrCreateExprID(binOp);
      }
      auto* cmpInst = llvm::dyn_cast<llvm::CmpInst>(branch->getCondition());
      if (cmpInst) {
        return generator->GetOrCreateExprID(cmpInst);
      }
      if (!cmpInst) { //change to last on the list
        llvm::errs() << *(branch->getCondition());
        assert(false && "condition not handled");
      }
      return 0;
    };

    assert(value1 == nullptr && "value1 in meet is not a nullptr");
    assert(value2 == nullptr && "value2 in meet is not a nullptr");
    auto* branch   = llvm::dyn_cast<BranchInst>(value3); //TODO: Switch Cases?
    assert(branch != nullptr && "Branch is a nullptr");

    if (branch->isUnconditional()) {
      return s1 + s2;
    }

    if ( s1.empty() && s2.empty()) {
      return DisjunctionValue{ }; // New disjunction at every inst
    }
    if (!s1.empty()) {
      s1.addConjunct(asBinaryExpr(branch));
    }
    if (!s2.empty()) {
      s2.addConjunct(asBinaryExpr(branch));
    }
    return s1 + s2;
  }
};


class DisjunctionTransfer {
private:
  void
  handleBinaryOperator(llvm::Value* value, DisjunctionState& state) {
    auto* bOp = llvm::dyn_cast<llvm::BinaryOperator>(value);
    if (!bOp) {
      return;
    }

    auto newExpr = generator->GetOrCreateExprID(bOp);
    // do work with newExpr
    return;
  }

public:
  void
  operator()(llvm::Value& v, DisjunctionState& state, const Context& context) {
    const CallSite cs{&v};
    const auto* fun = getCalledFunction(cs);
    // Pretend that indirect calls & non calls don't exist for this analysis
    if (!fun) {
      return;
    }

/* setting privileges don't seem to be working for now */
//    Privileges newPrivileges;
//    setRequiredPrivileges(newPrivileges, cs, context);
//    if (newPrivileges != 16) {
//      llvm::errs() << "\nFunction name" << fun->getName()
//                   << " " << newPrivileges.to_ulong();
//      return;
//    }

    auto asDisjunct = [](ExprID conjunct) -> Disjunct {
      auto d = Disjunct{};
      d.addConjunct(conjunct);
      return d;
    };

    auto vacExpr = generator->GetVacuousExprID();
    state[nullptr].addDisjunct(asDisjunct(vacExpr));
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
  using Value    = Disjunction;
  using Transfer = DisjunctionTransfer;
  using Meet     = DisjunctionMeet;
  using Analysis = analysis::DataflowAnalysis<Value, Transfer, Meet, analysis::Backward>;
  Analysis analysis{m, mainFunction};

  AnalysisPackage package;
  package.tmppathResults = tmpanalysis::gettmpAnalysisResults(m);
  libCHandlers = getLibCHandlerMap(package);
  auto results = analysis.computeDataflow();


  return false;
}

void
BuildPromiseTreePass::getAnalysisUsage(llvm::AnalysisUsage &info) const {
  info.addRequired<PostDominatorTreeWrapperPass>();
  info.addRequired<LoopInfoWrapperPass>();
}


static void
instrumentPromiseTree(llvm::Module& m) {
  legacy::PassManager pm;
  pm.add(llvm::createPostDomTree());
  pm.add(new llvm::LoopInfoWrapperPass());
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
