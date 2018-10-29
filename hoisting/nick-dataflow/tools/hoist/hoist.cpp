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

size_t Generator::exprCounter = 0;
std::deque<ExprNode*> Generator::slab;
llvm::DenseMap<llvm::Value*, ExprID> Generator::leafTable;
llvm::DenseMap<ExprKey, ExprID> Generator::exprTable;        

using DisjunctionValue  = Disjunction;
using DisjunctionState  = analysis::AbstractState<DisjunctionValue>;
using DisjunctionResult = analysis::DataflowResult<DisjunctionValue>;

class DisjunctionMeet : public analysis::Meet<DisjunctionValue, DisjunctionMeet> {
public:
  DisjunctionValue
  meetPair(DisjunctionValue& s1, DisjunctionValue& s2) const {
    return s1 + s2;
  }

  DisjunctionValue
  meetPairCustomized(DisjunctionValue& s1, DisjunctionValue& s2,
      llvm::Value* value1, llvm::Value* value2, llvm::Value* value3) const {

    auto valuePrint = [](llvm::Value* value) -> void {
      if (value) {
        llvm::outs() << *value;
      }
      else {
        llvm::outs() << value;
      }
    };

    auto printall = [&value1,&value2,&value3,&valuePrint]() {
      llvm::outs() << "\n Print Value At Meet";
      llvm::outs() << "\nvalue1 "; valuePrint(value1);
      llvm::outs() << "\nvalue2 "; valuePrint(value2);
      llvm::outs() << "\nvalue3 "; valuePrint(value3);
    };
    
    return s1 + s2;
  }
};


class DisjunctionTransfer {

private:
  void
  handleBinaryInstruction(llvm::Value* value, DisjunctionState& state) {
    auto* bi = llvm::dyn_cast<llvm::BinaryOperator>(value);
    if (!bi) {
      return;
    }

    auto newExpr = Generator::GetOrCreateExprID(bi);
    // do work with newExpr
    return;
  }

public:
  void
  operator()(llvm::Value& v, DisjunctionState& state, const Context& context) {
    handleBinaryInstruction(&v, state);
    
    const CallSite cs{&v};
    const auto* fun = getCalledFunction(cs);
    // Pretend that indirect calls & non calls don't exist for this analysis
    if (!fun) {
      return;
    }

    

    // FunctionsValue requiredPrivileges{};
    Privileges newPrivileges;
    setRequiredPrivileges(newPrivileges, cs, context);

    Disjunct vacuousConjunct{ }; // Generate a vacuously true expr
    auto temp = Disjunction{ };
    state[nullptr] = temp;

    //Rewrites
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
