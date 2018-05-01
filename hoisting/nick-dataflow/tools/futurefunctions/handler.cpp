
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

class Handlefread : public CustomHandler {

public:
    Handlefread(int n) : CustomHandler(n){};

    int operator()(llvm::CallSite cs)  override {
        //Do things with the callsite
        return 2;
    };
};


std::unordered_map<std::string, Handler>
getLibCHandlerMap(){    
    std::unordered_map<std::string, Handler> libCHandlers;

    libCHandlers.emplace("fread", "10001");
    libCHandlers.emplace("fopen", std::make_unique<Handlefread>(Handlefread(2)));


    return libCHandlers;
}
