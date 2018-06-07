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

using FunctionStack = std::stack<llvm::Function*>;
using CallGraph = llvm::DenseMap<llvm::Function*, std::vector<llvm::Function*>>;
using CallPath  = std::deque<llvm::Function*>;
using MatrixKey = std::pair<llvm::Function*, llvm::Function*>;
using GraphMatrix = llvm::DenseMap<MatrixKey, int>;
using FuncPrivMap =
    llvm::DenseMap<llvm::Function*, std::bitset<Promises::COUNT>>;
using FunctionQueue = std::queue<llvm::Function*>;
using FunctionWorklist = std::vector<llvm::Function*>;

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
  llvm::PrettyStackTraceProgram X(argc, argv);
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
  
  FunctionStack functionStack;
  CallGraph callGraph;
  CallPath callPath;
  GraphMatrix graphMatrix;
  FuncPrivMap funcPrivs;

  auto printEdgeList = [](GraphMatrix& graphMatrix ) {
    llvm::outs () << "digraph G {\n";
        for(auto& [functions, called] : graphMatrix){
          if(called)
          {
           llvm::outs () << functions.first->getName() << " -> " << functions.second->getName() << ";";
           llvm::outs() << "\n";
          }
        }
    llvm::outs () << "}";
      };

  auto addToMap = [&callGraph, &graphMatrix](llvm::Function* caller, llvm::Function* callee) {
    callGraph[caller].push_back(callee);
    MatrixKey mKey{caller, callee};
    graphMatrix[mKey] = 1;
  };

  auto addPrivilege = [&funcPrivs](llvm::StringRef functionName, llvm::Function* function) {
    if(functionName.startswith("_libc_")) {
      functionName = functionName.split("_libc_").second;
    }
    if(syscallWebBitsetMap.count(functionName)){
      funcPrivs[function] |= syscallWebBitsetMap[functionName];
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

  // BFS
  // FunctionQueue bfsQueue;
  // CallPath bfsPath;
  // auto initializeQueue = [&callGraph](FunctionQueue& bfsQueue, CallPath& bfsPath) {
  //   for (auto& [caller, callee] : callGraph) {
  //     if (caller->getVisibility()
  //         == llvm::GlobalValue::VisibilityTypes::HiddenVisibility) {
  //       continue;
  //     }
  //     bfsQueue.push(caller);
  //   } 

  //   bfsQueue.push(nullptr);
  //   return;
  // };

  // llvm::outs().flush();
  // llvm::errs() << "Commence BFS\n";

  // initializeQueue(bfsQueue, bfsPath);
  // int depth = 0;
  // llvm::DenseMap<int,std::set<llvm::Function*>> depthMap;
  

  // llvm::outs() << "digraph G {\n\t rankdir=LR;\n";
  // while (!bfsQueue.empty()) {
  //   auto* front = bfsQueue.front();

  //   if(front == nullptr){
  //     bfsQueue.pop();
  //     if(bfsQueue.empty()){
  //       llvm::outs() <<  " } \n";
  //       continue;
  //     }
  //     depth++;
  //     continue;
  //   }

  //   if(std::find(bfsPath.begin(), bfsPath.end(), front) != bfsPath.end()) {
  //     bfsQueue.pop();
  //     continue;
  //   }

  //   bfsQueue.pop();
  //   depthMap[depth].insert(front);

  //   auto& callees = callGraph[front];
  //   for( auto* callee : callees) {
  //     bfsQueue.push(callee);
  //     bfsPath.push_back(front);
  //     llvm::outs() << front->getName() << " -> "; 
  //     llvm::outs() << callee->getName() << ";\n"; 
  //   }
  // }

  // for(auto [depth, functions] : depthMap){
  //   llvm::outs() << "{ rank = same; ";
  //   const auto seperator = ", ";
  //   const auto* sep =  "";
  //   for(auto* item : functions) {
  //     llvm::outs() << sep << item->getName();
  //     sep = seperator;
  //   }
  //   llvm::outs() << " }\n ";
  // }

  // llvm::outs() << "}";
  // llvm::outs().flush();
  // llvm::errs() << "BFS Finished\n";

  llvm::outs().flush();
  llvm::errs() << "Building Transitive Closures\n";

  CallPath poFunctions;
  llvm::DenseSet<llvm::Function *> seenSet;
  auto dfs = [&poFunctions, &seenSet, &callGraph](llvm::Function* function, auto& dfs) -> void {
    if(!seenSet.insert(function).second){
      return;
    }
    for (auto& callee : callGraph[function]) {
      dfs(callee, dfs);
    }
    poFunctions.push_back(function);
  };

  for (auto& f : *module) {
    if(f.getVisibility() ==
    llvm::GlobalValue::VisibilityTypes::HiddenVisibility) {
      continue;
    }
    dfs(&f, dfs);
  }

  while(!poFunctions.empty()){
    auto* front = poFunctions.front();
    poFunctions.pop_front();
    // llvm::errs() << front->getName() << "\n";
    auto frontPrivs = funcPrivs[front]; 
    for(auto* callee : callGraph[front]) {
        funcPrivs[front] |= funcPrivs[callee];
    }
    if( funcPrivs[front] != frontPrivs) {
      auto& callers = inverseCallGraph[front];
      poFunctions.insert(poFunctions.end(), callers.begin(), callers.end());
    }
  }


  // for (auto& [k,kv] : callGraph) {
  //   // if(isllvmFunction(&k)) { continue; }
  //   for(auto& [i,iv] : callGraph){
  //     // if(isllvmFunction(&i)) { continue; }
  //     for(auto& [j,jv] : callGraph){

  //       MatrixKey ijKey{k,i};
  //       MatrixKey ikKey{i,k};
  //       MatrixKey kjKey{k,j};

  //       if(graphMatrix[ikKey] && graphMatrix[kjKey]){
  //         graphMatrix[ijKey] = 1;
  //       }
  //     }
  //   }
  // }

  // for(auto& [function_pair,called] :graphMatrix){
  //   if(called){
  //     if (funcPrivs.count(function_pair.second)) {
  //       funcPrivs[function_pair.first] |= funcPrivs[function_pair.second];
  //     }
  //   }
  // }

  // printEdgeList(graphMatrix);

  for (auto [function, bitv] : funcPrivs) {
    if(function->getVisibility() ==
    llvm::GlobalValue::VisibilityTypes::HiddenVisibility) {
      continue;
    }
    llvm::outs() << function->getName() << ", ";
    printBitset(bitv);
  }

  return 0;
}