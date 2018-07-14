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
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Constants.h"

// headers added for flag checking
#include <fcntl.h>
#include <sys/ioctl.h>

// STL libraries
#include <vector>
#include <bitset>
#include <memory>
#include <string>
#include <iostream>
#include <variant>
#include <queue>
#include <deque>
#include <algorithm>
#include <stack>
#include <bitset>
#include <set>

// #include "DataflowAnalysis.h"
// #include "FutureFunctions.h"
// #include "TaintAnalysis.h"
#include "CallGraphAnalyzer.h"

using namespace llvm;


using std::string;
using std::unique_ptr;

using CallGraph = llvm::DenseMap<llvm::Function*, std::vector<llvm::Function*>>;
using FunctionDeque  = std::deque<llvm::Function*>;
using FuncPrivMap =
    llvm::DenseMap<llvm::Function*, std::bitset<Promises::COUNT>>;

using FunctionSet = llvm::DenseSet<llvm::Function *>;

static cl::OptionCategory callgraphAnalyzerCategory{"Graph analyzer options"};

static cl::opt<string> inPath{cl::Positional,
                              cl::desc{"<Module to analyze>"},
                              cl::value_desc{"bitcode filename"},
                              cl::init(""),
                              cl::Required,
                              cl::cat{callgraphAnalyzerCategory}};

static cl::opt<bool> exitOpt{"x",
                             cl::desc{"<Exit after building callgraph (default false)>"},
                             cl::init(false),
                             cl::cat{callgraphAnalyzerCategory}};

static llvm::Function*
getCalledFunction(llvm::CallSite cs) {
  if (!cs.getInstruction()) {
    return nullptr;
  }

  llvm::Value* called = cs.getCalledValue()->stripPointerCasts();

  if (called->getName().contains("llvm")) {
    return nullptr;
  }

  return llvm::dyn_cast<llvm::Function>(called);
}

  [[maybe_unused]]
  auto printFunctionLocation = [](llvm::Function* function) {
    llvm::outs().changeColor(raw_ostream::Colors::GREEN);
    llvm::outs() << function->getSubprogram()->getFilename() << ":";
    llvm::outs() << function->getSubprogram()->getLine() << "\n";
  };

  [[maybe_unused]] auto printCallSiteLocation = [](llvm::CallSite cs) {
    if (DILocation* Loc = cs->getDebugLoc()) {  // Here I is an LLVM instruction
      unsigned Line  = Loc->getLine();
      StringRef File = Loc->getFilename();
      StringRef Dir  = Loc->getDirectory();
      llvm::outs() << Dir << " "<< File << ":";
      llvm::outs() << Line << "\n";
    }
  };


  [[maybe_unused]] auto printFunctionAttributes = [](llvm::Function* function) {
    llvm::outs().changeColor(raw_ostream::Colors::GREEN);
    for (auto& attribute : function->getAttributes()) {
      llvm::outs() << attribute.getAsString();
      llvm::outs() << "\n";
    }
    llvm::outs() << "\n";
    llvm::outs().changeColor(raw_ostream::Colors::WHITE);
  };



template <typename lambda>
static void
printFuncPrivMap(FuncPrivMap& funcPrivs, lambda& stripFunctionName) {
  for (auto [function, bitv] : funcPrivs) {
    if (function->isDeclaration()) {
      continue;
    }
    llvm::outs() << stripFunctionName(function->getName()) << ", "
                 << bitv.to_ulong() << ", ";
    printBitset(bitv, llvm::outs());
    llvm::outs() << "\n";
  }
}

template <typename lambda>
static void
writeCallGraphFiles(CallGraph& callGraph, FuncPrivMap& funcPrivs, lambda stripFunctionName) {
  FunctionSet seenSet;
  llvm::errs() << "\ndfs function graph\n";

  auto dfsviz = [&stripFunctionName, &callGraph, &seenSet, &funcPrivs](
                    llvm::Function* function,
                    llvm::raw_fd_ostream& outs,
                    auto& dfsviz) -> void {
    if (!seenSet.insert(function).second) {
      return;
    }
    auto callees = callGraph[function];
    if (callees.empty()) {
      return;
    }

    for (auto* callee : callees) {
      dfsviz(callee, outs, dfsviz);

      outs << stripFunctionName(function->getName()) << " [label = \""
           << stripFunctionName(function->getName()) << " | ";
      printBitset(funcPrivs[function], outs);
      outs << "\" ]"
           << "\n"
           << stripFunctionName(callee->getName()) << " [label = \""
           << stripFunctionName(callee->getName()) << " | ";
      printBitset(funcPrivs[callee], outs);
      // fixme: set intersection of privs between caller and callee
      // outs << " | " ;
      // printBitset(funcPrivs[callee] & funcPrivs[function], outs);
      outs << "\" ] \n";

      outs << stripFunctionName(function->getName()) << " -> "
           << stripFunctionName(callee->getName()) << " \n";
      outs.flush();
    }

    outs.flush();
    outs << "rank = same;";
    for (auto* callee : callees) {
      outs << " " << stripFunctionName(callee->getName()) << " ;";
    }
    outs << "\n";
  };

  for (auto& [function, unused_callees] : callGraph) {
    seenSet.clear();

    std::error_code ec;
    auto fouts = raw_fd_ostream(
        ("graphs/" + stripFunctionName(function->getName())).str(),
        ec,
        sys::fs::F_Text);

    fouts << "digraph " << function->getName() << " {\n"
          << "node [shape = record];\n"
          << "rankdir = lr;\n";
    dfsviz(function, fouts, dfsviz);
    fouts << "}\n";

    fouts.flush();
  }
}

std::vector<int>
getioctlCSPromises(llvm::CallSite& cs) {
  auto* arg    = cs.getArgument(1);
  std::vector<int> returnVec;

  if (auto* argInt = llvm::dyn_cast<llvm::ConstantInt>(arg); argInt) {
    auto argSExtValue = argInt->getSExtValue();
    // llvm::outs() << "argInt: " << argSExtValue << "\n";

    switch (argSExtValue) {
      // /home/ska196/src/lib/libc/stdlib/posix_pty.c 47
      case 1076392961: {  // PTMGET
        // Requires PLEDGE_TTY and either of PLEDGE_RPATH or PLEDGE_WPATH
        returnVec.push_back(PLEDGE_INET);
        returnVec.push_back(PLEDGE_RPATH);
        returnVec.push_back(PLEDGE_WPATH);
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/net/sockatmark.c 37
      case 1074033415: {  // SIOCATMARK
        returnVec.push_back(PLEDGE_INET);
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcdrain.c 42
      case 536900702: {  // TIOCDRAIN
        // Not handled in kern_pledge. Noted in Readme. No special pledges added
        // Used to drain the tty socket. Probably not handled since the program
        // will need tty before this anyway Does it fit in our model of
        // privilege dropping?
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcdrain.c 42
      // case 536900702:{
      //   break;
      // }

      // /home/ska196/src/lib/libc/termios/tcflow.c 44
      case 536900719: {  // TIOCSTOP
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcflow.c 46
      case 536900718: {  // TIOCSTART
        returnVec.push_back(PLEDGE_TTY);
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcflush.c 55
      case 2147775504: {  // TIOCFLUSH
        returnVec.push_back(PLEDGE_TTY);
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcgetattr.c 37
      case 1076655123: {  // TIOCGETA
        returnVec.push_back(PLEDGE_TTY);
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcgetpgrp.c 39
      case 1074033783: {  // TIOCGPGRP
        returnVec.push_back(PLEDGE_TTY);
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcgetsid.c 39
      case 1074033763: {  // TIOCGSID
        // Gets the session id to the currently connected tty terminal
        // Similar to TIOCDRAIN
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcsendbreak.c 43
      case 536900731: {  // TIOCSBRK
        returnVec.push_back(PLEDGE_TTY);
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcsendbreak.c 46
      case 536900730: {  // TIOCCBRK
        returnVec.push_back(PLEDGE_TTY);
        return returnVec;
        break;
      }

      // All three are treated the same in kern_pledge.c:1147
      // /home/ska196/src/lib/libc/termios/tcsetattr.c 47
      case 2150396948: {  // TIOCSETA
        returnVec.push_back(PLEDGE_TTY);
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcsetattr.c 49
      case 2150396949: {  // TIOCSETAW
        returnVec.push_back(PLEDGE_TTY);
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcsetattr.c 51
      case 2150396950: {  // TIOCSETAF
        returnVec.push_back(PLEDGE_TTY);
        return returnVec;
        break;
      }
      // /home/ska196/src/lib/libc/termios/tcsetpgrp.c 40
      case 2147775606: {  // TIOCSPGRP
        returnVec.push_back(PLEDGE_PROC);
        returnVec.push_back(PLEDGE_TTY);
        return returnVec;
        break;
      }
      default: { return returnVec;}
    }
  }
  else {
    llvm::errs() << "\nioctl arg SExtValue error";
    return returnVec;
  }
}

std::vector<int>
getsysctlCSPromises(llvm::CallSite& cs) {
  std::vector<int> returnVec;

    // llvm::outs() << "\n"
    //              << "FunctionName " << getCalledFunction(cs)->getName() 
    //              << "\n";
    // printCallSiteLocation(cs);

    if ( cs->getParent()->getParent()->getName().equals("_libc_getifaddrs" )){
      returnVec.push_back(PLEDGE_ROUTE); /* Promise requirement for _libc_getifaddrs set to PLEDGE_ROUTE for now */
      return returnVec;
    }
  return returnVec;
}


std::vector<int>
getfcntlCSPromises(llvm::CallSite& cs) {
  auto* arg    = cs.getArgument(1);
  std::vector<int> returnVec;

  if (auto* argInt = llvm::dyn_cast<llvm::ConstantInt>(arg); argInt) {
    auto argSExtValue = argInt->getSExtValue();
    // llvm::outs() << "\n"
    //              << "FunctionName " << getCalledFunction(cs)->getName() << " argInt:" << argSExtValue
    //              << "\n";
    // printCallSiteLocation(cs);
    switch (argSExtValue) {
      //   fcntl_cancel argInt:3
      case 3: {
        // F_GETFL
        // returnVec.push_back();
        break;
      }
      // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/stdio/fdopen.c //
      // 58

      // fcntl_cancel argInt:1
      case 1: {
        // F_GETFD
        break;
      }
      // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/stdio/fdopen.c //
      // 81

      // fcntl_cancel argInt:2
      case 2: {
        // F_SETFD
        break;
      }
      // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/stdio/fdopen.c //
      // 82

      // fcntl_cancel argInt:11
      case 11: {
        // F_ISATTY
        break;
      }
      // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/gen/isatty.c // 37

      // fcntl_cancel argInt:7
      case 7: {  // Handle lockf.c seperately
        // F_GETLK
        returnVec.push_back(PLEDGE_FLOCK);
        break;
      }
      // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/gen/lockf.c // 63

      // fcntl_cancel argInt:6
      case 6: {
        // F_SETOWN
        returnVec.push_back(PLEDGE_PROC);
        break;
      }
      // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/net/rcmd.c // 137

      // fcntl argInt:9
      case 9: {
        // F_SETLKW
        returnVec.push_back(PLEDGE_FLOCK);
        break;
      }
      // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/sys/w_fcntl.c // 49

      default: {
        llvm::errs() << "Default Reached on fcntl constant switch case";
      }
    }

    // Same Flags Different CallSites
    // fcntl_cancel argInt:3
    // case 3: {
    //   //F_GETFL
    //   break;
    // }
    // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/gen/opendir.c // 71

    // fcntl_cancel argInt:2
    // case 2: {
    //   //F_SETFD
    //   break;
    // }
    // // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/gen/opendir.c //
    // 86

    // fcntl_cancel argInt:1
    // case 1: {
    //   // F_GETFD
    //   break;
    // }
    // // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/gen/popen.c // 103

    // fcntl_cancel argInt:2
    // case 2: {
    //   // F_SETFD
    //   break;
    // }
    // // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/gen/popen.c // 105

    // // fcntl_cancel argInt:1
    // case 1: {
    //   // F_GETFD
    //   break;
    // }
    // // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/gen/popen.c // 133

    // fcntl_cancel argInt:2
    // case 2: {
    //   // F_SETFD
    //   break;
    // }
    // // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/gen/popen.c // 135

    // fcntl_cancel argInt:1
    // case 1: {
    //   // F_GETFD
    //   break;
    // }
    // // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/gen/posix_spawn.c
    // // 161

    // // fcntl_cancel argInt:2
    // case 2: {
    //   //F_SETFD
    //   break;
    // }
    // // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/gen/posix_spawn.c
    // // 165

    // // fcntl_cancel argInt:2
    // case 2: {
    //   // F_SETFD
    //   break;
    // }
    // // /home/ska196/src/lib/libc /home/ska196/src/lib/libc/yp/yp_bind.c //
    // 240
  } else {
    /* Non Constants are only induced by lockf.c. Assert for parent function
     * name == lockf and insert flock privilege to be handled at upper level */

    // llvm::outs() << "\n NonConstantInt " << cs->getParent()->getParent()->getName()
    //              << " FunctionName " << getCalledFunction(cs)->getName()
    //              << "\n";
    // printCallSiteLocation(cs);

    if( cs->getParent()->getParent()->getName().equals("lockf") ){
      returnVec.push_back(PLEDGE_FLOCK);
    }
    else {
      llvm::errs() << "\nAssert Failed: lockf assert failed\n";
    }

  }
    return returnVec;
}

/* Accepts a CallSite and Bitset(funcPrivs) and appends privileges associated
 * with the callsite to the calling function 
 * WAS (before fcntl) appending the privilege to the callee at the callsite. Referred to in comments as C1 */
template <typename lambda>
void
addPrivilege(FuncPrivMap& funcPrivs,
             lambda stripFunctionName,
             llvm::Function* callee,  // Change to callee
             llvm::Function* caller,
             llvm::CallSite& cs) {
  auto& bitsetMap = syscallManMap;
  auto calleeName = stripFunctionName(callee->getName());

  if(calleeName.contains("gethostid")) {
    llvm::outs() << "Callee Name: " << callee->getName() << "\n";
    llvm::outs() << "Caller Name: " << caller->getName() << "\n";
    printCallSiteLocation(cs);
  }

  if (calleeName.equals("sysctl")) {
    for (auto promise : getsysctlCSPromises(cs)) {
      funcPrivs[caller] |= 1 << promise;  // C1
    }
  }

  else if (calleeName.equals("fcntl_cancel")) {
    // Handle wrapper for fcntl only, flock is a simple seed, lockf is to be
    // handled at upper level analysis
    for (auto promise : getfcntlCSPromises(cs)) {
      funcPrivs[caller] |= 1 << promise;  // C1
    }
  } else if (calleeName.equals("ioctl")) {
    for (auto promise : getioctlCSPromises(cs)) {
      funcPrivs[caller] |= 1 << promise;  // C1
    }
  } else {
    funcPrivs[caller] |= bitsetMap[calleeName];  // C1
  }
};

int
main(int argc, char** argv) {
  // This boilerplate provides convenient stack traces and clean LLVM exit
  // handling. It also initializes the built in support for convenient
  // command line option handling.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  llvm::PrettyStackTraceProgram X(argc, argv);//libc function called xdr_wrap exists
  llvm_shutdown_obj shutdown;
  cl::HideUnrelatedOptions(callgraphAnalyzerCategory);
  cl::ParseCommandLineOptions(argc, argv);

  // Construct an IR file from the filename passed on the command line.
  SMDiagnostic err;
  LLVMContext context;
  unique_ptr<Module> module = parseIRFile(inPath.getValue(), err, context);

  if (!module.get()) {
    errs() << "Error reading bitcode file: " << inPath << "\n";
    err.print(argv[0], errs());
    return -1;
  }

  CallGraph callGraph;
  FuncPrivMap funcPrivs;

  auto addToMap = [&callGraph](llvm::Function* caller, llvm::Function* callee) {
    callGraph[caller].push_back(callee);
  };

  auto stripFunctionName = [](llvm::StringRef functionName) -> llvm::StringRef{
    if(functionName.startswith("_libc_")) {
      functionName = functionName.split("_libc_").second;
    }
    if(functionName.endswith("_wrap")) { //libc function called xdr_wrap exists
      functionName = functionName.split("_wrap").first; 
    }
    return functionName;
  };

  for (auto& caller : *module) {
    /* fcntl (syscall) is only called from fcntl_cancel. Since I'm handling
     * fcntl_cancel, I don't need to handle the calls to fcntl within
     * fcntl_cancel. 
     * TLDR: Skips calls within fcntl wrapper */
    if (caller.getName().equals("_libc_fcntl_cancel")) { continue; }

    /* sysconf has several calls to sysctl within a switch case.
     * sysconf has a single use inside libc (getdtablesize)
     * which doesn't need any privileges */
    if (caller.getName().equals("_libc_sysconf")) { continue; }

    for (auto& bb : caller) {
      for (auto& i : bb) {
        // TODO: Function Pointer Resolver
        llvm::CallSite cs{&i};
        if (!cs) {
          continue;
        }

        auto* callee = getCalledFunction(cs);
        if (!callee) {
          continue;
        }
        addToMap(&caller,callee);
        addPrivilege(funcPrivs, stripFunctionName, callee, &caller, cs);
      }
    }
  }

  if(exitOpt.getValue()){
    llvm::outs().flush();
    llvm::errs().flush();
    exit(0);
  }
  

  llvm::DenseMap<llvm::Function*, llvm::DenseSet<Function*>> inverseCallGraph;
  for (auto& [caller, callees] : callGraph){
    for(auto* callee : callees){
      inverseCallGraph[callee].insert(caller);
    }
  }

  llvm::outs().flush();
  llvm::errs() << "Building Transitive Closures\n";

  FunctionDeque functionWorklist;
  FunctionSet seenSet;
  auto dfs = [&functionWorklist, &seenSet, &callGraph](llvm::Function* function, auto& dfs) -> void {
    if(!seenSet.insert(function).second){
      return;
    }
    for (auto& callee : callGraph[function]) {
      dfs(callee, dfs);
    }
    functionWorklist.push_back(function);
  };

  for (auto& f : *module) {
    dfs(&f, dfs);
  }

  while(!functionWorklist.empty()){
    auto* front = functionWorklist.front();
    functionWorklist.pop_front();
    // llvm::errs() << front->getName() << "\n";
    auto frontPrivs = funcPrivs[front]; 
    for(auto* callee : callGraph[front]) {
        funcPrivs[front] |= funcPrivs[callee];
    }
    if( funcPrivs[front] != frontPrivs) {
      auto& callers = inverseCallGraph[front];
      functionWorklist.insert(functionWorklist.end(), callers.begin(), callers.end());
    }
  }

  printFuncPrivMap(funcPrivs, stripFunctionName); 
  writeCallGraphFiles(callGraph, funcPrivs, stripFunctionName);
  
  return 0;
}


void printBFS(CallGraph callGraph){
  using FunctionQueue = std::queue<llvm::Function*>;
  FunctionQueue bfsQueue;
  FunctionDeque bfsPath;
  auto initializeQueue = [&callGraph](FunctionQueue& bfsQueue, FunctionDeque&
  bfsPath) {
    for (auto& [caller, callee] : callGraph) {
      if (caller->getVisibility()
          == llvm::GlobalValue::VisibilityTypes::HiddenVisibility) {
        continue;
      }
      bfsQueue.push(caller);
    }

    bfsQueue.push(nullptr);
    return;
  };

  llvm::outs().flush();
  llvm::errs() << "Commence BFS\n";

  initializeQueue(bfsQueue, bfsPath);
  int depth = 0;
  llvm::DenseMap<int,std::set<llvm::Function*>> depthMap;
  

  llvm::outs() << "digraph G {\n\t rankdir=LR;\n";
  while (!bfsQueue.empty()) {
    auto* front = bfsQueue.front();

    if(front == nullptr){
      bfsQueue.pop();
      if(bfsQueue.empty()){
        llvm::outs() <<  " } \n";
        continue;
      }
      depth++;
      continue;
    }

    if(std::find(bfsPath.begin(), bfsPath.end(), front) != bfsPath.end()) {
      bfsQueue.pop();
      continue;
    }

    bfsQueue.pop();
    depthMap[depth].insert(front);

    auto& callees = callGraph[front];
    for( auto* callee : callees) {
      bfsQueue.push(callee);
      bfsPath.push_back(front);
      llvm::outs() << front->getName() << " -> "; 
      llvm::outs() << callee->getName() << ";\n"; 
    }
  }

  for(auto [depth, functions] : depthMap){
    llvm::outs() << "{ rank = same; ";
    const auto seperator = ", ";
    const auto* sep =  "";
    for(auto* item : functions) {
      llvm::outs() << sep << item->getName();
      sep = seperator;
    }
    llvm::outs() << " }\n ";
  }

  llvm::outs() << "}";
  llvm::outs().flush();
  llvm::errs() << "BFS Finished\n";
}