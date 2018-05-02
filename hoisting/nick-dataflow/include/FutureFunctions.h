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


enum 
Promises {stdio, rpath, wpath, cpath, dpath, 
 tmppath, inet, mcast, fattr, chown, flock, promise_unix, //unix is set as a #define
 dns, getpw, sendfd, recvfd, tape, 
 tty, proc, exec, prot_exec, settime, ps,
 vminfo, id, pf,
 COUNT //For now
 };

const llvm::StringRef
PromiseNames[] {"stdio", "rpath", "wpath", "cpath", "dpath", 
 "tmppath", "inet", "mcast", "fattr", "chown", "flock", "unix" //unix is set as a #define
 "dns", "getpw", "sendfd", "recvfd", "tape", 
 "tty", "proc", "exec", "prot_exec", "settime", "ps",
 "vminfo", "id", "pf"
 };

// using FunctionsValue  = llvm::SparseBitVector<>;

//Custom Handler declared here, Should be moved to it's own file?

using FunctionsValue  = std::bitset<COUNT-1>;
using FunctionsState  = analysis::AbstractState<FunctionsValue>;
using FunctionsResult = analysis::DataflowResult<FunctionsValue>;
using Context = std::array<llvm::Instruction*, 2ul>;



class CustomHandler {

public:
    
    CustomHandler(int ap);  
    virtual ~CustomHandler();
  
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

  FunctionsValue promisesBitset;
  HandlerFunctor handlerFunctor;

public:
  //Constructors
  Handler(std::string bitString);
  Handler(HandlerFunctor&& hf);
  
  //Methods
  FunctionsValue getPromisesBitset(const llvm::CallSite& cs, const Context& context);

};

//==================Functions======================================//
std::unordered_map<std::string, Handler>
getLibCHandlerMap(
        tmpanalysis::tmppathResultsTy& tmpanalysisResults
        );

#endif