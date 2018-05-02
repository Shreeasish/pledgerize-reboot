
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

        auto& contextResults = tmpResults[context];

        auto* function = getCalledFunction(cs);

        for (auto& [context, contextResults] : tmpResults) {
                llvm::outs() << "For loop outer\n";
            for (auto& [function, functionResults] : contextResults) {
                
                llvm::outs() << "For loop inner\n";
                llvm::outs() << "Function" << function->getName();
            }
        }
        
        auto& functionsResults = contextResults[function]; //Need to use non const key here

        for (auto& [a,b] : functionsResults) {
            llvm::outs() << *a << "Value of a" << "\n";
        }

        auto argument = cs.getArgument(getArgPosition());

        // llvm::outs() << "Argument " << *argument << "\n";

        auto argState = functionsResults.find(argument);

        for (auto& [one,two] : functionsResults) {
            llvm::outs() << *one << "Value" << "\n";
        }
        
        if(argState != functionsResults.end()){
            llvm::outs () << "Found the state for argposition " << getArgPosition() << "\n";
        }
        return 0;
    };
}; //=============== Custom Handler================ //


std::unordered_map<std::string, Handler>
getLibCHandlerMap(
        tmpanalysis::tmppathResultsTy& tmpResults
        ) {    

    std::unordered_map<std::string, Handler> libCHandlers;
    libCHandlers.emplace("fopen", "10001");
    libCHandlers.emplace("fread", std::make_unique<Handlefread>(Handlefread(3, tmpResults) )); // Argument position supplied here
    
    return libCHandlers;
};
