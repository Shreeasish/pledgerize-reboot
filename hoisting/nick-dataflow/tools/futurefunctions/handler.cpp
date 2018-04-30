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
#include "llvm/ADT/StringMap.h"

#include <bitset>
#include <memory>
#include <string>
#include <iostream>
#include <variant>
#include <unordered_map>

#include "FutureFunctions.h"


std::unordered_map<std::string, Handler> libcHandlers =
{ 
    {"fopen", Handler{"00011"}},
    {"fread", Handler{&handle_fread}}
};

Handler::Handler(std::string bitString) : promisesBitset{bitString} { //Use '1000010' generated from python
    llvm::outs().changeColor(llvm::raw_ostream::Colors::GREEN);
    llvm::outs() << "From BitString Constructor \n";
};

Handler::Handler(HandlerFunctor hf) : handlerFunctor{ hf } {};

FunctionsValue 
Handler::getPromisesBitset(llvm::CallSite cs) {

    if (handlerFunctor != nullptr) {
        return handlerFunctor(cs);
    }
    else
    {
        return promisesBitset;
    }
}

static FunctionsValue
handle_fread(const llvm::CallSite cs) {
    
    llvm::outs().changeColor(llvm::raw_ostream::Colors::GREEN);
    llvm::outs() << "From handler \n";

    return FunctionsValue{"000"};
} //handle_ function prefix
