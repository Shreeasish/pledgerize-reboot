
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


// Handler //
Handler::Handler(std::string bitString) : promisesBitset{bitString} { //Use '1000010' generated from python
    llvm::outs().changeColor(llvm::raw_ostream::Colors::GREEN);
    llvm::outs() << "From BitString Constructor \n";
};

Handler::Handler(HandlerFunctor&& hf) : handlerFunctor{ std::move(hf) } {};

FunctionsValue 
Handler::getPromisesBitset(llvm::CallSite cs) {
    if (handlerFunctor != nullptr) {
        return (*handlerFunctor)(cs);
    }
    else {
        return promisesBitset;
    }
}

// Custom Handler //
CustomHandler::CustomHandler(int ap) : argposition{ap} {};
CustomHandler::~CustomHandler(){};

// The handler should never trigger the analysis. The analysis should be performed once in the beginning. 
// All analyses that are required for handlers should have their results passed into the function that returns the handlers.


// Custom Handler Derived Class 
class Handlefread : public CustomHandler {  

tmpanalysis::tmppathResultsTy& tmpResults;

public:
    Handlefread(int n, tmpanalysis::tmppathResultsTy& tresults) : CustomHandler(n), tmpResults{tresults} {};

    int operator()(llvm::CallSite cs)  override {
        llvm::outs() << "Handlerfread initialized with argposition " << getArgPosition() << "\n";

        // for (auto& [context, contextResults] : results) {
        //   for (auto& [function, functionResults] : contextResults) {
        //     collectFileRights(functionResults, std::back_inserter(errors));
        //   }
        // }

        for (auto& [context, contextResults] : tmpResults) {
            for (auto& [function, functionResults] : contextResults){
                
            }
        }
        return 2;
    };
};


std::unordered_map<std::string, Handler>
getLibCHandlerMap(
        tmpanalysis::tmppathResultsTy& tmpResults
        ) {    

    std::unordered_map<std::string, Handler> libCHandlers;
    libCHandlers.emplace("fopen", "10001");
    libCHandlers.emplace( "fread", std::make_unique<Handlefread>(Handlefread(2, tmpResults) )); // Argument position supplied here
    
    return libCHandlers;
}
