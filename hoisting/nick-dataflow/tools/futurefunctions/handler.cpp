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

#include "futurefunctions.h"

std::unordered_map<std::string, Handler> libcHandlers { 
    "fread", Handler("01101")
};

Handler::Handler(std::string bitString, const llvm::CallSite cs) : promisesBitset{bitString} { //Use '1000010' generated from python
    for(int i = 0; i < COUNT-1; i++) {
        if(promisesBitset[i])
            llvm::outs() << PromiseNames[i];
            }
    };

Handler::Handler(HandlerFunctor hf, const llvm::CallSite cs) : handlerFunctor{ hf } {
    promisesBitset = handlerFunctor(cs);
    };

FunctionsValue
Handler::getPromisesBitset(llvm::CallSite cs) {
    return promisesBitset;
}

