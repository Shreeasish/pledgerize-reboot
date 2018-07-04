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

static cl::OptionCategory futureFunctionsCategory{"Graph analyzer options"};

static cl::opt<string> inPath{cl::Positional,
                              cl::desc{"<Module to analyze>"},
                              cl::value_desc{"bitcode filename"},
                              cl::init(""),
                              cl::Required,
                              cl::cat{futureFunctionsCategory}};

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
      llvm::outs() << Dir << " "<< File << "\n";
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

template <typename lambda>
void
addPrivilege(FuncPrivMap& funcPrivs,
             lambda stripFunctionName,
             llvm::Function* function,
             llvm::Function& caller,
             llvm::CallSite& cs) {
  auto& bitsetMap = syscallManMap;
  auto functionName = stripFunctionName(function->getName());
  if (bitsetMap.count(functionName)) {
    if (functionName.equals("ioctl")) {
      // llvm::outs() << "\n" << functionName << "\n";
      // llvm::outs() << "\n" <<  arg.
      auto* arg    = cs.getArgument(1);
      auto* argInt = llvm::dyn_cast<llvm::ConstantInt>(arg);

      if (argInt; auto argSExtValue = argInt->getSExtValue()) {
        // llvm::outs() << "argInt: " << argSExtValue << "\n";

        switch (argSExtValue) {
          // /home/ska196/src/lib/libc/stdlib/posix_pty.c 47
          case 1076392961: { // PTMGET
            // Requires PLEDGE_TTY and either of PLEDGE_RPATH or PLEDGE_WPATH
            funcPrivs[function] |= 1 << PLEDGE_INET;
            funcPrivs[function] |= 1 << PLEDGE_RPATH;
            funcPrivs[function] |= 1 << PLEDGE_WPATH;
            break;
          }
          // /home/ska196/src/lib/libc/net/sockatmark.c 37
          case 1074033415: { // SIOCATMARK
            funcPrivs[function] |= 1 << PLEDGE_INET;
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcdrain.c 42
          case 536900702: { // TIOCDRAIN
            // Not handled in kern_pledge. Noted in Readme. No special pledges added
            // Used to drain the tty socket. Probably not handled since the program will need tty before this anyway
            // Does it fit in our model of privilege dropping?
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcdrain.c 42
          // case 536900702:{
          //   break;
          // }

          // /home/ska196/src/lib/libc/termios/tcflow.c 44
          case 536900719: { // TIOCSTOP
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcflow.c 46
          case 536900718: { // TIOCSTART
            funcPrivs[function] |= 1 << PLEDGE_TTY;
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcflush.c 55
          case 2147775504: { // TIOCFLUSH
            funcPrivs[function] |= 1 << PLEDGE_TTY;
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcgetattr.c 37
          case 1076655123: { //TIOCGETA
            funcPrivs[function] |= 1 << PLEDGE_TTY;
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcgetpgrp.c 39
          case 1074033783: { // TIOCGPGRP
            funcPrivs[function] |= 1 << PLEDGE_TTY;
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcgetsid.c 39
          case 1074033763: { // TIOCGSID
            // Gets the session id to the currently connected tty terminal
            // Similar to TIOCDRAIN
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcsendbreak.c 43
          case 536900731: {// TIOCSBRK
            funcPrivs[function] |= 1 << PLEDGE_TTY;
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcsendbreak.c 46
          case 536900730: {// TIOCCBRK
            funcPrivs[function] |= 1 << PLEDGE_TTY;
            break;
          }

          // All three are treated the same in kern_pledge.c:1147
          // /home/ska196/src/lib/libc/termios/tcsetattr.c 47
          case 2150396948: { // TIOCSETA
            funcPrivs[function] |= 1 << PLEDGE_TTY;
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcsetattr.c 49
          case 2150396949: { // TIOCSETAW
            funcPrivs[function] |= 1 << PLEDGE_TTY;
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcsetattr.c 51
          case 2150396950: { // TIOCSETAF
            funcPrivs[function] |= 1 << PLEDGE_TTY;
            break;
          }
          // /home/ska196/src/lib/libc/termios/tcsetpgrp.c 40
          case 2147775606: {// TIOCSPGRP 
            funcPrivs[function] |= 1 << PLEDGE_PROC;
            funcPrivs[function] |= 1 << PLEDGE_TTY;
            break;
          }
          default: {
            break;
          }
        }
      }
      // printCallSiteLocation(cs);
    }
    funcPrivs[function] |= bitsetMap[functionName];
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
  cl::HideUnrelatedOptions(futureFunctionsCategory);
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
        addPrivilege(funcPrivs, stripFunctionName, callee, caller, cs);
      }
    }
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