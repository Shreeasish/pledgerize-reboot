
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
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/Constants.h"

#include <bitset>
#include <memory>
#include <string>
#include <iostream>
#include <variant>
#include <unordered_map>

#include "FutureFunctions.h"
#include "DataflowAnalysis.h"


// Headers included for checking flags
#include <netinet/in.h>  //mcast


static llvm::Function*
getCalledFunction(llvm::CallSite cs) {


  if (!cs.getInstruction()) {
    return nullptr;
  }

  llvm::Value* called = cs.getCalledValue()->stripPointerCasts();
  return llvm::dyn_cast<llvm::Function>(called);
}

// class  : public PledgeCheckerBase {
//   tmpanalysis::tmppathResultsTy& tmpResults;

// public:
//   Handlefread(int n, tmpanalysis::tmppathResultsTy& tresults)
//     : PledgeCheckerBase(n), tmpResults{tresults} {};

//   FunctionsValue
//   operator()(const llvm::CallSite cs, const Context& context) override {
//     auto& contextResults = tmpResults[context];
//     auto* instr          = cs.getInstruction();
//     auto& functionResults = contextResults[instr->getFunction()];
//     auto state            = analysis::getIncomingState(functionResults,
//     *instr); auto arg              = cs.getArgument(getArgPosition()); auto
//     isTmp            = state[arg];

//     if (isTmp.test(0)) {
//       return std::bitset<COUNT>{1 << (PLEDGE_TMPPATH - 1) };
//     }

//     return 0;
//   };
// };

class CheckTMPPATH : public PledgeCheckerBase {
public:
  CheckTMPPATH(int ap) : PledgeCheckerBase(ap) {}

  // TODO: Correct tmppath logic -> RW not needed if in /tmp/
  FunctionsValue
  operator()(const llvm::CallSite cs,
             const Context& context,
             AnalysisPackage* analysisPackage) override {
    auto& contextResults  = analysisPackage->tmppathResults[context];
    auto* instr           = cs.getInstruction();
    auto& functionResults = contextResults[instr->getFunction()];
    auto state            = analysis::getIncomingState(functionResults, *instr);
    auto arg              = cs.getArgument(getArgPosition());
    auto isTmp            = state[arg];

    if (isTmp.test(0)) {
      return 1 << (PLEDGE_TMPPATH);
    }

    return 0;
  };
};

class CheckMCAST : public PledgeCheckerBase {
public:
  CheckMCAST(int ap) : PledgeCheckerBase(ap) {}

  FunctionsValue
  operator()(const llvm::CallSite cs,
             const Context& context,
             AnalysisPackage* AnalysisPackage) override {
    auto* arg   = cs.getArgument(getArgPosition());
    auto* argInt = llvm::dyn_cast<llvm::ConstantInt>(arg);
    // llvm::outs() << *argInt;
    if (argInt->getSExtValue() == IP_MULTICAST_IF) {
      return 1 << PLEDGE_MCAST;
    }

    return 0;
  };
};

std::unordered_map<std::string, FunctionPledges>
getLibCHandlerMap(AnalysisPackage& package) {
  std::unordered_map<std::string, FunctionPledges> libCHandlers;

  libCHandlers.emplace("fread",
                       FunctionPledgesBuilder(16, &package)
                           .add(std::make_unique<CheckTMPPATH>(3))
                           .build());
  libCHandlers.emplace(
      "setsockopt",
      FunctionPledgesBuilder(16).add(std::make_unique<CheckMCAST>(2)).build());
  libCHandlers.emplace("__sclose", 16);
  libCHandlers.emplace("__smakebuf", 16);
  libCHandlers.emplace("__swhatbuf", 16);
  libCHandlers.emplace("__sseek", 16);
  libCHandlers.emplace("__sread", 16);
  libCHandlers.emplace("__srefill", 16);
  libCHandlers.emplace("__srget", 16);
  libCHandlers.emplace("__svfscanf", 16);
  libCHandlers.emplace("__swrite", 16);
  libCHandlers.emplace("__swsetup", 16);
  libCHandlers.emplace("__vfprintf", 16);
  libCHandlers.emplace("__vfwprintf", 16);
  libCHandlers.emplace("_mktemp", 62);
  libCHandlers.emplace("mktemp_internal", 62);
  libCHandlers.emplace("asprintf", 16);
  libCHandlers.emplace("dprintf", 16);
  libCHandlers.emplace("vdprintf", 16);
  libCHandlers.emplace("fdopen", 16);
  libCHandlers.emplace("fgetln", 16);
  libCHandlers.emplace("fgets", 16);
  libCHandlers.emplace("fopen", 16);
  libCHandlers.emplace("fprintf", 16);
  libCHandlers.emplace("vfprintf", 16);
  libCHandlers.emplace("freopen", 16);
  libCHandlers.emplace("fscanf", 16);
  libCHandlers.emplace("vfscanf", 16);
  libCHandlers.emplace("fseek", 16);
  libCHandlers.emplace("fseeko", 16);
  libCHandlers.emplace("fsetpos", 16);
  libCHandlers.emplace("fwprintf", 16);
  libCHandlers.emplace("vfwprintf", 16);
  libCHandlers.emplace("getdelim", 16);
  libCHandlers.emplace("getline", 16);
  libCHandlers.emplace("getw", 16);
  libCHandlers.emplace("mkdtemp", 62);
  libCHandlers.emplace("mkostemp", 62);
  libCHandlers.emplace("mkostemps", 62);
  libCHandlers.emplace("mkstemp", 62);
  libCHandlers.emplace("mkstemps", 62);
  libCHandlers.emplace("mktemp", 62);
  libCHandlers.emplace("perror", 16);
  libCHandlers.emplace("printf", 16);
  libCHandlers.emplace("remove", 46);
  libCHandlers.emplace("rewind", 16);
  libCHandlers.emplace("scanf", 16);
  libCHandlers.emplace("setbuf", 16);
  libCHandlers.emplace("setvbuf", 16);
  libCHandlers.emplace("setbuffer", 16);
  libCHandlers.emplace("setlinebuf", 16);
  libCHandlers.emplace("snprintf", 16);
  libCHandlers.emplace("sprintf", 16);
  libCHandlers.emplace("sscanf", 16);
  libCHandlers.emplace("swprintf", 16);
  libCHandlers.emplace("vswprintf", 16);
  libCHandlers.emplace("tempnam", 62);
  libCHandlers.emplace("tmpnam", 62);
  libCHandlers.emplace("vasprintf", 16);
  libCHandlers.emplace("vprintf", 16);
  libCHandlers.emplace("vscanf", 16);
  libCHandlers.emplace("vsnprintf", 16);
  libCHandlers.emplace("vsprintf", 16);
  libCHandlers.emplace("vsscanf", 16);
  libCHandlers.emplace("vwprintf", 16);
  libCHandlers.emplace("wprintf", 16);
  return libCHandlers;
};
