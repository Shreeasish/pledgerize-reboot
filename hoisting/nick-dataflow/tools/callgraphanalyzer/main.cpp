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
#include <stack>
#include <algorithm>
#include <bitset>

// #include "DataflowAnalysis.h"
// #include "FutureFunctions.h"
// #include "TaintAnalysis.h"
#include "CallGraphAnalyzer.h"

using namespace llvm;


using std::string;
using std::unique_ptr;

using FunctionStack = std::stack<llvm::Function*>;
using CallGraph = llvm::DenseMap<llvm::Function*, std::vector<llvm::Function*>>;
using CallPath  = std::vector<llvm::Function*>;
using MatrixKey = std::pair<llvm::Function*, llvm::Function*>;
using GraphMatrix = llvm::DenseMap<MatrixKey, int>;
using FuncPrivMap =
    llvm::DenseMap<llvm::Function*, std::bitset<Promises::COUNT>>;

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

  auto addToMap = [&callGraph,&graphMatrix]
          (llvm::Function* caller, llvm::Function* callee) {
            callGraph[caller].push_back(callee);
            MatrixKey mKey{caller,callee};
            graphMatrix[mKey] = 1;
        };

  auto addPrivilege = [&funcPrivs](llvm::StringRef functionName, llvm::Function* function){
    if(functionName.startswith("_libc_")){
      functionName = functionName.split("_libc_").second;
    }
    if(syscallWebBitsetMap.count(functionName)){
      funcPrivs[function] |= syscallWebBitsetMap[functionName];
    }
  };

  llvm::outs() << "funcPrivs size" << funcPrivs.size()<< "\n";

  for (auto& caller : *module) {
    for (auto& bb : caller) {
      for (auto& i : bb) {
        llvm::CallSite cs{&i};
        if(!cs) {
          continue;
        }
        
        auto* callee = getCalledFunction(cs);
        if(!callee) {
          continue; 
        }

        addToMap(&caller,callee);
        addPrivilege(callee->getName(), callee);
      }
    }
  }
  llvm::outs().flush();
  llvm::errs() << "Building Callgraph \n";

  for (auto& [k,kv] : callGraph) {
    // if(isllvmFunction(&k)) { continue; }
    for(auto& [i,iv] : callGraph){
      // if(isllvmFunction(&i)) { continue; }
      for(auto& [j,jv] : callGraph){
        
        MatrixKey ijKey{k,i};
        MatrixKey ikKey{i,k};
        MatrixKey kjKey{k,j};

        if(graphMatrix[ikKey] && graphMatrix[kjKey]){
          graphMatrix[ijKey] = 1;
        }
      }
    }
  }

  for(auto& [function_pair,called] :graphMatrix){
    if(called){
      if (funcPrivs.count(function_pair.second)) {
        funcPrivs[function_pair.first] |= funcPrivs[function_pair.second];
      }
    }
  }

  for (auto [function, bitv] : funcPrivs) {
    if(function->getVisibility() == llvm::GlobalValue::VisibilityTypes::HiddenVisibility) {
      continue;
    }
    llvm::outs() << function->getName() << ", ";
    printBitset(bitv);
  }
  return 0;
}