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


using FunctionsValue = std::bitset<COUNT-1>;
using FunctionsState  = analysis::AbstractState<FunctionsValue>;
using FunctionsResult = analysis::DataflowResult<FunctionsValue>;
using HandlerFunctor = FunctionsValue (*)(const llvm::CallSite);


// Forward Declarations
static std::vector<const llvm::Function*> functions;
static llvm::DenseMap<const llvm::Function*,size_t> functionIDs;


static void 
handle_fread(FunctionsValue& requiredPrivileges, const llvm::Function * fun);


#ifndef Handler_H
#define Handler_H
class Handler {

  FunctionsValue promisesBitset;
  HandlerFunctor handlerFunctor;

public:
  Handler(std::string bitString);
  Handler(HandlerFunctor hf);

  FunctionsValue getPromisesBitset(llvm::CallSite(cs));
};
#endif