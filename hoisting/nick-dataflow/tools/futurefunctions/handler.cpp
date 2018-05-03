
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
static llvm::Function *
getCalledFunction(llvm::CallSite cs) {
  if (!cs.getInstruction()) {
    return nullptr;
  }

  llvm::Value *called = cs.getCalledValue()->stripPointerCasts();

  return llvm::dyn_cast<llvm::Function>(called);
}


//================== Handler==================== //
Handler::Handler(std::string bitString) : promisesBitset{bitString} { //Use '1000010' generated from python
    llvm::outs().changeColor(llvm::raw_ostream::Colors::GREEN);
};

Handler::Handler(HandlerFunctor&& hf) : handlerFunctor{ std::move(hf) } {};

FunctionsValue 
Handler::getPromisesBitset(const llvm::CallSite& cs, const Context& context) {
    
    if (handlerFunctor != nullptr) {
        return (*handlerFunctor)(cs, context);
    }
    else {
        return promisesBitset;
    }
}
//================== Handler==================== //

//=============== Custom Handler================ //
CustomHandler::CustomHandler(int ap) : argposition{ap} {};
CustomHandler::~CustomHandler(){};

// The handler should never trigger the analysis. The analysis should be performed once in the beginning. 
// All analyses that are required for handlers should have their results passed into the function that returns the handlers.


// Custom Handler Derived Class 
class Handlefread : public CustomHandler {  

tmpanalysis::tmppathResultsTy& tmpResults;

public:
    Handlefread(int n, tmpanalysis::tmppathResultsTy& tresults) : CustomHandler(n), tmpResults{tresults} {};

    FunctionsValue
    operator()(const llvm::CallSite cs, const Context& context)  override {        
        llvm::outs() << "Handler \n";

        // llvm::outs() << tmpResults.count(context) << " tmpResults \n";
        auto& contextResults = tmpResults[context];
        auto* instr = cs.getInstruction();
        // llvm::outs() << contextResults.count(instr->getFunction()) << " contextResults \n";
        
        auto& functionResults = contextResults[instr->getFunction()];
        auto state = analysis::getIncomingState(functionResults, *instr);
        auto arg = cs.getArgument(getArgPosition());
        auto isTmp = state[arg];

        
        if(isTmp.test(0)){
            return std::bitset<COUNT-1>{1<<tmppath};
        }


        return 0;
    };
}; //=============== Custom Handler================ //


std::unordered_map<std::string, Handler>
getLibCHandlerMap(
        tmpanalysis::tmppathResultsTy& tmpResults
        ) {    

    std::unordered_map<std::string, Handler> libCHandlers;
    libCHandlers.emplace("fopen", "1");
    libCHandlers.emplace("fread", std::make_unique<Handlefread>(Handlefread(3, tmpResults) )); // Argument position supplied here
    
    return libCHandlers;
};
