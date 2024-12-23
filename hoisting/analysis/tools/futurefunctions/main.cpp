#include "llvm/ADT/SparseBitVector.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/DebugInfo.h"
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

#include "DataflowAnalysis.h"
#include "FutureFunctions.h"
#include "TaintAnalysis.h"

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


// LibC map here
std::unordered_map<std::string, FunctionPledges> libCHandlers;


static const llvm::Function*
getCalledFunction(const llvm::CallSite cs) {
  if (!cs.getInstruction()) {
    return nullptr;
  }
  const llvm::Value* called = cs.getCalledValue()->stripPointerCasts();
  if (called->getName().contains("llvm")) {
    return nullptr;
  }
  // llvm::errs() << called->getName() << "\n";
  return llvm::dyn_cast<llvm::Function>(called);
}


static void
setRequiredPrivileges(FunctionsValue& requiredPrivileges,
                      llvm::CallSite cs,
                      const Context& context) {
  auto functionName = getCalledFunction(cs)->getName().str();
  auto found        = libCHandlers.find(functionName);

  if (found != libCHandlers.end()) {
    auto promisesBitset = found->second.getPromisesBitset(cs, context);
    requiredPrivileges |= promisesBitset;
  } else if (syscallBitsetMap.count(functionName)) {
    requiredPrivileges |= syscallBitsetMap[functionName];
  } else {
    return;
  }
}


class FunctionsMeet : public analysis::Meet<FunctionsValue, FunctionsMeet> {
public:
  FunctionsValue
  meetPair(FunctionsValue& s1, FunctionsValue& s2) const {
    return s1 | s2;
  }
};


class FunctionsTransfer {
public:
  void
  operator()(llvm::Value& v, FunctionsState& state, const Context& context) {
    const CallSite cs{&v};
    const auto* fun = getCalledFunction(cs);

    // Pretend that indirect calls & non calls don't exist for this analysis
    if (!fun) {
      return;
    }

    // FunctionsValue requiredPrivileges{};
    setRequiredPrivileges(state[nullptr], cs, context);
    auto [found, inserted] = functionIDs.insert({fun, functions.size()});
    if (inserted) {
      functions.push_back(fun);
    }
  }
};


template <typename OutIterator>
static void
collectFollowers(FunctionsResult& followerStates, OutIterator followers) {
  for (auto& [value, state] : followerStates) {
    auto* inst = llvm::dyn_cast<llvm::Instruction>(value);
    if (!inst) {
      continue;
    }

    llvm::CallSite cs{inst};
    auto* fun = getCalledFunction(cs);
    if (!fun) {
      continue;
    }

    *followers++ = std::make_pair(inst, state[nullptr]);
  }
}


static void
printLineNumber(llvm::raw_ostream& out, llvm::Instruction& inst) {
  if (const llvm::DILocation* debugLoc = inst.getDebugLoc()) {
    out << "At " << debugLoc->getFilename() << " line " << debugLoc->getLine()
        << ":\n";
  } else {
    out << "At an unknown location:\n";
  }
}


static void
printFollowers(
    llvm::ArrayRef<std::pair<llvm::Instruction*, FunctionsValue>> followers) {
  for (auto& [callsite, after] : followers) {
    // llvm::outs().changeColor(raw_ostream::Colors::RED);
    printLineNumber(llvm::outs(), *callsite);

    auto* called = getCalledFunction(llvm::CallSite{callsite});
    // llvm::outs().changeColor(raw_ostream::Colors::YELLOW);
    llvm::outs() << "After call to \"" << called->getName() << "\" ";


    for (int i = 0; i < COUNT; i++) {
      if (after[i]) {
        llvm::outs() << PromiseNames[i] << " ";
      }
    }
    llvm::outs() << "\n";
  }

  if (followers.empty()) {
    // llvm::outs().changeColor(raw_ostream::Colors::GREEN);
    llvm::outs() << "No followers collected\n";
  }
  // llvm::outs().resetColor();
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

  auto* mainFunction = module->getFunction("main");
  if (!mainFunction) {
    llvm::report_fatal_error("Unable to find main function.");
  }

  using Value    = FunctionsValue;
  using Transfer = FunctionsTransfer;
  using Meet     = FunctionsMeet;
  using Analysis =
      analysis::DataflowAnalysis<Value, Transfer, Meet, analysis::Backward>;
  Analysis analysis{*module, mainFunction};

  // Get all forward analyses here
  AnalysisPackage package;
  package.tmppathResults = tmpanalysis::gettmpAnalysisResults(*module);
  // Get the library handler map here
  libCHandlers = getLibCHandlerMap(package);

  auto results = analysis.computeDataflow();

  std::vector<std::pair<llvm::Instruction*, FunctionsValue>> followers;
  for (auto& [context, contextResults] : results) {
    for (auto& [function, functionResults] : contextResults) {
      collectFollowers(functionResults, std::back_inserter(followers));
      for (auto& [location, state] : functionResults) {
        auto* inst = llvm::dyn_cast<llvm::Instruction>(location);
        if (!inst) {
          continue;
        }
        printLineNumber(llvm::outs(), *inst);
        for (int i = 0; i < COUNT; i++) {
          if (state[nullptr][i]) {
            llvm::outs() << PromiseNames[i] << " ";
          }
        }
        llvm::outs() << "\n";
      }
    }
  }
  printFollowers(followers);
  return 0;
}
