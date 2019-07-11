#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
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

#include "DataflowAnalysisBackup.h"
#include "FutureFunctions.h"
#include "TaintAnalysis.h"

#include "LibEventAnalyzer.h"

#include "llvm/IR/InstVisitor.h"

#include <vector>
#include <optional>
#include <unordered_set>

namespace libEventAnalyzer {

// Path specific list of functions which may be called from event_dispatch
class CallBackList {
public:
  void
  add(llvm::Function* incomingFunction) {
    functionList.insert(incomingFunction);
  }

  bool
  erase(llvm::Function* incomingFunction) {
    if (functionList.find(incomingFunction) == functionList.end()) {
      functionList.erase(incomingFunction);
      return true;
    }
    llvm_unreachable("Does not exist");
    return false;
  }

  bool
  operator==(const CallBackList& other) const {
    return 
      this->functionList == other.functionList;
  }

  CallBackList
  operator|(const CallBackList& other) const {
    return *this;
  }

private:
  std::unordered_set<llvm::Function*> functionList;
};

using State          = analysisOriginal::AbstractState<CallBackList>;
using CallBackResult = analysisOriginal::DataflowResult<State>;

class Aggregator : public analysisOriginal::Meet<CallBackList, Aggregator> {
public:
  CallBackList
  meetPair(CallBackList& s1, CallBackList& s2) const {
    return s1;
  }
};

template <typename llvmTyOne, typename llvmTyTwo>
void
spitAndDie(llvmTyOne* typeOne, llvmTyTwo* typeTwo) {
  llvm::errs() << "\n TypeOne"
               << *typeOne;
  llvm::errs() << "\n TypeTwo"
               << *typeTwo;
  llvm_unreachable("spitAndDie");
  return;
}



class TransferBase {
public:
  virtual ~TransferBase() {}
  virtual void operator()(llvm::Instruction*) = 0;
};

class AddEvent : public TransferBase {
  void operator()(llvm::Instruction*) { 
    llvm::errs() << "addevent";
    return; }
};

class DelEvent : public TransferBase {
  void operator()(llvm::Instruction*) { return; }
};

class InitEvent : public TransferBase {
  void operator()(llvm::Instruction*) { return; }
};

class DispatchEvent : public TransferBase {
  void operator()(llvm::Instruction*) { return; }
};

class ITransferFunction {
public:
  void
  operator()(llvm::Instruction* inst) {
    (*transferFunction)(inst);
    return;
  }

  template <typename TransferTy>
  ITransferFunction&&
  buildFunction() {
    this->transferFunction = std::make_unique<TransferTy>();
    return std::move(*this);
  }

private:
  std::unique_ptr<TransferBase> transferFunction;
};

//class ITransferFunctionBuilder {
//public:
//  ITransferFunction
//
//}

class CallBackTransfer {
public:
  CallBackTransfer() {
    transferMap.try_emplace("event_add",      ITransferFunction{}.buildFunction<AddEvent>());
    transferMap.try_emplace("event_del",      ITransferFunction{}.buildFunction<DelEvent>());
    transferMap.try_emplace("event_init",     ITransferFunction{}.buildFunction<InitEvent>());
    transferMap.try_emplace("event_dispatch", ITransferFunction{}.buildFunction<DispatchEvent>());
  }

  void
  operator()(llvm::Value& v, State state/*, const Context& context*/) {
    llvm::errs() << "\n" << v;
    llvm::CallSite cs{&v};
    state[nullptr] = CallBackList{};

    auto& transfer = getTransferFunctionInterface("event_add");
    transfer(cs.getInstruction());

    if (auto eventName = getEventName(cs)) {
      auto& transfer = getTransferFunctionInterface("event_add");
      //auto& transfer = getTransferFunctionInterface(*eventName);
      transfer(cs.getInstruction());
      return;
    }
    return;
  }

private:
  llvm::StringMap<ITransferFunction> transferMap;

  llvm::Function*
  getCalledFunction(llvm::CallSite cs) {
    auto* calledValue = cs.getCalledValue()->stripPointerCasts();
    return llvm::dyn_cast<llvm::Function>(calledValue);
  }

  std::optional<llvm::StringRef>
  getFilteredName(llvm::Function* function) {
    if (!function) {
      return std::nullopt;
    }
    if (auto found = transferMap.find(function->getName())
        ; found != transferMap.end()) {
      return function->getName();
    } else {
      return std::nullopt;
    }
  }
  
  std::optional<llvm::StringRef>
  getEventName(llvm::CallSite cs) {
    auto* function = cs.getInstruction() 
                       ? getCalledFunction(cs)
                       : nullptr;
    return getFilteredName(function);
  }
  
  ITransferFunction&
  getTransferFunctionInterface(llvm::StringRef name) {
    if (auto found = transferMap.find(name)
        ; found != transferMap.end()) {
      return found->second;
    } 
    return transferMap.find("")->second; // Get a no-op transfer function
  }

  //struct Tra : public llvm::InstVisitor<phiInstVisitor> {
  //  void
  //  visitPHINode(llvm::PHINode& Inst) {
  //    llvm::errs() << "\nAt a PhiNOde";
  //  }
  //  void
  //  visitCallSite(CallSite cs) {
  //    llvm::err() << "\nAt a call site";
  //  }
  //}
};

using Value = CallBackList;
using Transfer = CallBackTransfer;
using Meet = Aggregator;
using Analysis = analysisOriginal::DataflowAnalysis<Value, Transfer, Meet>;

void
getResults(llvm::Module& module) {
  auto* mainFunction = module.getFunction("main");
  if (!mainFunction) {
    llvm::report_fatal_error("Unable to find main function.");
  }


  Analysis analysis{module, mainFunction};
  auto results = analysis.computeDataflow();
  //return ResultChomper{results}.chomp();
  return;
}


} // end namespace
