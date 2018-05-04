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


enum Promises {
  PLEDGE_ALWAYS,
  PLEDGE_RPATH,
  PLEDGE_WPATH,
  PLEDGE_CPATH,
  PLEDGE_STDIO,
  PLEDGE_TMPPATH,
  PLEDGE_DNS,
  PLEDGE_INET,
  PLEDGE_FLOCK,
  PLEDGE_UNIX,
  PLEDGE_ID,
  PLEDGE_TAPE,
  PLEDGE_GETPW,
  PLEDGE_PROC,
  PLEDGE_SETTIME,
  PLEDGE_FATTR,
  PLEDGE_PROTEXEC,
  PLEDGE_TTY,
  PLEDGE_SENDFD,
  PLEDGE_RECVFD,
  PLEDGE_EXEC,
  PLEDGE_ROUTE,
  PLEDGE_MCAST,
  PLEDGE_VMINFO,
  PLEDGE_PS,
  PLEDGE_DISKLABEL,
  PLEDGE_PF,
  PLEDGE_AUDIO,
  PLEDGE_DPATH,
  PLEDGE_DRM,
  PLEDGE_VMM,
  PLEDGE_CHOWN,
  PLEDGE_CHOWNUID,
  PLEDGE_BPF,
  PLEDGE_ERROR,
  COUNT
};


const llvm::StringRef PromiseNames[] {
  "rpath", "wpath", "cpath", "stdio", "tmppath", "dns", "inet", "flock", "unix",
      "id", "tape", "getpw", "proc", "settime", "fattr", "protexec", "tty",
      "sendfd", "recvfd", "exec", "route", "mcast", "vminfo", "ps", "disklabel",
      "pf", "audio", "dpath", "drm", "vmm", "chown", "bpf", "error"
};


// using FunctionsValue  = llvm::SparseBitVector<>;

//Custom Handler declared here, Should be moved to it's own file?

using FunctionsValue  = std::bitset<COUNT>;
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
  Handler(unsigned long bitString);
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