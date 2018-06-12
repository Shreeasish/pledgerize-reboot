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

  static auto printBitset = [](std::bitset<COUNT> bitv){
    llvm::outs() << bitv.to_ulong() << ", ";
    for(int i = 38; i>=0; i--){
      llvm::outs() << bitv[i];
    }
    llvm::outs() << "\n";
  };
  
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


  auto addPrivilege = [&funcPrivs, &stripFunctionName](llvm::StringRef functionName, llvm::Function* function) {
    auto& bitsetMap = syscallBitsetMap;

    functionName = stripFunctionName(functionName);
    if(bitsetMap.count(functionName)){
      funcPrivs[function] |= bitsetMap[functionName];
    }
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
        addPrivilege(callee->getName(), callee);
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
    // if(f.getVisibility() ==
    // llvm::GlobalValue::VisibilityTypes::HiddenVisibility) {
    //   continue;
    // }
    // if(f.isDeclaration()) {
    //   continue;
    // }

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


  auto printFunctionLocation = [](llvm::Function* function) {
    llvm::outs().changeColor(raw_ostream::Colors::GREEN);
    llvm::outs() << function->getSubprogram()->getFilename() << ":";
    llvm::outs() << function->getSubprogram()->getLine() << "\n";
  };

  auto printFunctionAttributes = [](llvm::Function* function) {
    llvm::outs().changeColor(raw_ostream::Colors::GREEN);
    for (auto& attribute : function->getAttributes()) {
      llvm::outs() << attribute.getAsString();
      llvm::outs() << "\n";
    }
    llvm::outs() << "\n";
    llvm::outs().changeColor(raw_ostream::Colors::WHITE);
  };

  for (auto [function, bitv] : funcPrivs) {
    // if(function->getVisibility() ==
    // llvm::GlobalValue::VisibilityTypes::HiddenVisibility) {
    //   continue;
    // }
    if(function->isDeclaration()) {
      continue;
    }
    llvm::outs() << stripFunctionName(function->getName()) << ", ";
    printBitset(bitv);
    // printFunctionLocation(function);
    // printFunctionAttributes(function);
  }

  llvm::errs() << "\n  DFS Function Graph \n" ;
  auto dfsviz = [&callGraph, &seenSet](llvm::Function* function,
                                       auto& dfsviz) -> void {
    if (!seenSet.insert(function).second) {
      return;
    }
    auto callees = callGraph[function];

    for (auto* callee : callees) {
      dfsviz(callee, dfsviz);
      llvm::outs() << function->getName() << "->" << callee->getName() << "\n";
    }
  };

  for (auto& [function, callees] : callGraph){
    seenSet.clear();
    llvm::outs() << "digraph " << function->getName() << " {"<< "\n";
    dfsviz(function, dfsviz);
    llvm::outs() << "}\n" ; 
  }

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