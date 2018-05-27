#ifndef FUTUREFUNCTIONS_H
#define FUTUREFUNCTIONS_H

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
#include "TaintAnalysis.h"
#include "PromiseDeclarations.h"

using FunctionsValue  = std::bitset<COUNT>;
using FunctionsState  = analysis::AbstractState<FunctionsValue>;
using FunctionsResult = analysis::DataflowResult<FunctionsValue>;
using Context = std::array<llvm::Instruction*, 2ul>;

class CustomHandler {
public:
    CustomHandler(int ap) : argposition{ap} {};
    virtual ~CustomHandler(){};
    
    virtual FunctionsValue operator()(llvm::CallSite, const Context& context) = 0;


    int getArgPosition() {
      return argposition;
    }

private:
    int argposition;

}; // end CustomHandler

using HandlerFunctor  = std::unique_ptr<CustomHandler>;

static std::vector<const llvm::Function*> functions;
static llvm::DenseMap<const llvm::Function*,size_t> functionIDs;

class Handler {
public:
  Handler(unsigned long bitString) : promisesBitset{bitString} {};
  Handler(HandlerFunctor&& hf) : handlerFunctor{std::move(hf)} {};

  FunctionsValue
  getPromisesBitset(const llvm::CallSite& cs, const Context& context) {
    if (handlerFunctor != nullptr) {
      return (*handlerFunctor)(cs, context);
    } else {
      return promisesBitset;
    }
  }

private:
  FunctionsValue promisesBitset;
  HandlerFunctor handlerFunctor;

};

//==================Functions======================================//
std::unordered_map<std::string, Handler>
getLibCHandlerMap(
        tmpanalysis::tmppathResultsTy& tmpanalysisResults
        );

#endif