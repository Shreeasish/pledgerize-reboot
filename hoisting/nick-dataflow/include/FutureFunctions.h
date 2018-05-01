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

class CustomHandler {
public:
  
    CustomHandler(int ap);  
    virtual ~CustomHandler();
  
    virtual int operator()(llvm::CallSite) = 0;

private:
    int argposition;

};

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
  FunctionsValue getPromisesBitset(llvm::CallSite cs);

};

std::unordered_map<std::string, Handler>
getLibCHandlerMap();


#endif