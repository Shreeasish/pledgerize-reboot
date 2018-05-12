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

#include <vector>
#include <bitset>
#include <memory>
#include <string>
#include <iostream>
#include <variant>
#include <stack>
#include <algorithm>

#include "DataflowAnalysis.h"
#include "FutureFunctions.h"
#include "TaintAnalysis.h"

using namespace llvm;


using std::string;
using std::unique_ptr;

using FunctionStack = std::stack<llvm::Function*>;
using CallGraph     = llvm::DenseMap<llvm::Function*, std::vector<llvm::Function*>>;
using CallPath      = std::vector<llvm::Function*>;
using MatrixKey     = std::pair<llvm::Function*,llvm::Function*>;
using GraphMatrix 
                    = llvm::DenseMap<MatrixKey,int>;


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

  FunctionStack functionStack;
  CallGraph callGraph;
  CallPath callPath;
  GraphMatrix graphMatrix;

  auto addToMap = [&callGraph,&graphMatrix]
          (llvm::Function* target, llvm::Function* package) {
            callGraph[target].push_back(package);
            
            MatrixKey mKey{target,package};
            graphMatrix[mKey] = 1;
        };

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
      }
    }
  }

  //Attach Syscalls to dictionary

  auto isllvmFunction = [](auto* check){
    if(check->getName().contains("llvm"))
      return true;
    return false;
  };

  
  for (auto& k : *module) {
    if(isllvmFunction(&k)) { continue; }

    
    for(auto& i : *module){
      if(isllvmFunction(&i)) { continue; }
      

      for(auto& j : *module){
        if(isllvmFunction(&j)) { continue; }
        

        MatrixKey ijKey{&k,&i};
        MatrixKey ikKey{&i,&k};
        MatrixKey kjKey{&k,&j};

        if(graphMatrix.count(ijKey)){
          continue;
        }

        if(graphMatrix[ikKey] && graphMatrix[kjKey]){
          graphMatrix[ijKey] = 1;
        }
      }
    }
  }
  






  return 0;
}