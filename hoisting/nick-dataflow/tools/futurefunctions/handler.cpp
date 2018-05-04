
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

#include <bitset>
#include <memory>
#include <string>
#include <iostream>
#include <variant>
#include <unordered_map>

#include "FutureFunctions.h"
#include "DataflowAnalysis.h"

// Undefined reference issues so placed here
static llvm::Function*
getCalledFunction(llvm::CallSite cs) {
  if (!cs.getInstruction()) {
    return nullptr;
  }

  llvm::Value* called = cs.getCalledValue()->stripPointerCasts();

  return llvm::dyn_cast<llvm::Function>(called);
}


//================== Handler==================== //
Handler::Handler(unsigned long bitString)
  : promisesBitset{bitString} {  // Use '1000010' generated from python
  llvm::outs().changeColor(llvm::raw_ostream::Colors::GREEN);
};

Handler::Handler(HandlerFunctor&& hf) : handlerFunctor{std::move(hf)} {};

FunctionsValue
Handler::getPromisesBitset(const llvm::CallSite& cs, const Context& context) {
  if (handlerFunctor != nullptr) {
    return (*handlerFunctor)(cs, context);
  } else {
    return promisesBitset;
  }
}
//================== Handler==================== //

//=============== Custom Handler================ //
CustomHandler::CustomHandler(int ap) : argposition{ap} {};
CustomHandler::~CustomHandler(){};

// The handler should never trigger the analysis. The analysis should be
// performed once in the beginning. All analyses that are required for handlers
// should have their results passed into the function that returns the handlers.


// Custom Handler Derived Class
class Handlefread : public CustomHandler {
  tmpanalysis::tmppathResultsTy& tmpResults;

public:
  Handlefread(int n, tmpanalysis::tmppathResultsTy& tresults)
    : CustomHandler(n), tmpResults{tresults} {};

  FunctionsValue
  operator()(const llvm::CallSite cs, const Context& context) override {
    // llvm::outs() << "Handler \n";

    // llvm::outs() << tmpResults.count(context) << " tmpResults \n";
    auto& contextResults = tmpResults[context];
    auto* instr          = cs.getInstruction();
    // llvm::outs() << contextResults.count(instr->getFunction()) << "
    // contextResults \n";

    auto& functionResults = contextResults[instr->getFunction()];
    auto state            = analysis::getIncomingState(functionResults, *instr);
    auto arg              = cs.getArgument(getArgPosition());
    auto isTmp            = state[arg];

    if (isTmp.test(0)) {
        // llvm::outs() << "TMPPATH \n" << std::bitset<COUNT>{1 << PLEDGE_TMPPATH}.to_ulong();
      return std::bitset<COUNT>{1 << PLEDGE_TMPPATH - 1};
    }


    return 0;
  };
};  //=============== Custom Handler================ //


std::unordered_map<std::string, Handler>
getLibCHandlerMap(tmpanalysis::tmppathResultsTy& tmpResults) {
  std::unordered_map<std::string, Handler> libCHandlers;
//   libCHandlers.emplace("fopen", "1");
  libCHandlers.emplace("fread",
                       std::make_unique<Handlefread>(Handlefread(
                           3, tmpResults)));  // Argument position supplied here
  libCHandlers.emplace("__sclose", 8);
  libCHandlers.emplace("__smakebuf", 8);
  libCHandlers.emplace("__sread", 8);
  libCHandlers.emplace("__srefill", 8);
  libCHandlers.emplace("__srget", 8);
  libCHandlers.emplace("__sseek", 8);
  libCHandlers.emplace("__svfscanf", 8);
  libCHandlers.emplace("__swhatbuf", 8);
  libCHandlers.emplace("__swrite", 8);
  libCHandlers.emplace("__swsetup", 8);
  libCHandlers.emplace("__vfprintf", 8);
  libCHandlers.emplace("__vfwprintf", 8);
  libCHandlers.emplace("_mktemp", 268435499);
  libCHandlers.emplace("asprintf", 8);
  libCHandlers.emplace("dprintf", 8);
  libCHandlers.emplace("fdopen", 8);
  libCHandlers.emplace("fgetln", 8);
  libCHandlers.emplace("fgets", 8);
  libCHandlers.emplace("fopen", 8);
  libCHandlers.emplace("fprintf", 8);
  libCHandlers.emplace("fread", 8);
  libCHandlers.emplace("freopen", 8);
  libCHandlers.emplace("fscanf", 8);
  libCHandlers.emplace("fseek", 8);
  libCHandlers.emplace("fseeko", 8);
  libCHandlers.emplace("fsetpos", 8);
  libCHandlers.emplace("fwprintf", 8);
  libCHandlers.emplace("getdelim", 8);
  libCHandlers.emplace("getline", 8);
  libCHandlers.emplace("getw", 8);
  libCHandlers.emplace("mkdtemp", 268435499);
  libCHandlers.emplace("mkostemp", 268435499);
  libCHandlers.emplace("mkostemps", 268435499);
  libCHandlers.emplace("mkstemp", 268435499);
  libCHandlers.emplace("mkstemps", 268435499);
  libCHandlers.emplace("mktemp", 268435499);
  libCHandlers.emplace("perror", 8);
  libCHandlers.emplace("printf", 8);
  libCHandlers.emplace("remove", 268435491);
  libCHandlers.emplace("rewind", 8);
  libCHandlers.emplace("scanf", 8);
  libCHandlers.emplace("setbuf", 8);
  libCHandlers.emplace("setbuffer", 8);
  libCHandlers.emplace("setlinebuf", 8);
  libCHandlers.emplace("setvbuf", 8);
  libCHandlers.emplace("snprintf", 8);
  libCHandlers.emplace("sprintf", 8);
  libCHandlers.emplace("sscanf", 8);
  libCHandlers.emplace("swprintf", 8);
  libCHandlers.emplace("tempnam", 268435499);
  libCHandlers.emplace("tmpfile", 301989931);
  libCHandlers.emplace("tmpnam", 268435499);
  libCHandlers.emplace("vasprintf", 8);
  libCHandlers.emplace("vdprintf", 8);
  libCHandlers.emplace("vfprintf", 8);
  libCHandlers.emplace("vfscanf", 8);
  libCHandlers.emplace("vfwprintf", 8);
  libCHandlers.emplace("vprintf", 8);
  libCHandlers.emplace("vscanf", 8);
  libCHandlers.emplace("vsnprintf", 8);
  libCHandlers.emplace("vsprintf", 8);
  libCHandlers.emplace("vsscanf", 8);
  libCHandlers.emplace("vswprintf", 8);
  libCHandlers.emplace("vwprintf", 8);
  libCHandlers.emplace("wprintf", 8);

  return libCHandlers;
};
