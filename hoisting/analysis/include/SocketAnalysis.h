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

#include "sys/socket.h"

enum class SocketType {
  NONE,
  UNIX = 1, // TODO: Switch to macros
  INET = 2,
  INET6 = 24,
  UNKNOWN
};

/* Simply hold on to the domain type for each callsite*/
using SocketResults = llvm::DenseMap<llvm::Instruction*, SocketType>; 

class SocketAnalyzer {
public:
  SocketAnalyzer(llvm::Module& m) : module{m} { buildTable(); }

  SocketType // Pre-checked for socket relevant functions only
  inferSocketType(llvm::CallSite& cs) {
    auto domainFlag  = cs.getArgument(0);
    auto* asllvmCInt = llvm::dyn_cast<llvm::ConstantInt>(domainFlag);
    auto asInt       = asllvmCInt->getSExtValue(); // Fail if incorrect

    switch (asInt) { // TODO: Switch to MACRO defs
      case (int) SocketType::UNIX: {
        return SocketType::UNIX;
        break;
      }
      case (int) SocketType::INET: {
        return SocketType::INET;
        break;
      }
      case (int) SocketType::INET6: {
        return SocketType::INET6;
        break;
      }
      default: { return SocketType::NONE; }
    }
  }

  SocketType
  getSocketResults(const llvm::CallSite& cs) {
    auto asInst = cs.getInstruction();
    if (results.count(asInst)) {
      return results[asInst];
    }
    return SocketType::UNKNOWN;
  }

  void
  buildTable() {
    for (auto& function : module) {
      for (auto& bb : function) {
        for (auto& inst : bb) {
          handleCallSite(llvm::CallSite{&inst});
        }
      }
    }
  }

  void
  handleCallSite(llvm::CallSite cs) {
    if (!cs.getInstruction()) {
      return;
    }
    auto* function = getCalledFunction(cs);
    if (!function || !function->isDeclaration()) {  // looking for externals only
      return;
    }
  
    auto processUsers = [this](llvm::CallSite& origin, auto domain) {
      std::deque<llvm::Instruction*> workList;
      workList.push_back(origin.getInstruction());

      llvm::errs() << "\nUsers for "<< *origin.getInstruction();
      while (!workList.empty()) {
        auto* inst = workList.front();
        workList.pop_front();
        this->results[inst] = domain;
        
        for (auto* user : inst->users()) {
          llvm::errs() << "\n\t " << *user;
          auto asCallSite = llvm::CallSite{user};
          auto* asInst = llvm::dyn_cast<llvm::Instruction>(user);
          if (asCallSite.getInstruction() && this->results.count(asInst) == 0) {
            this->results[asInst] = domain;
            workList.push_back(asInst);
          }
        }
      }
    };

    if (function->getName().startswith("socket")) {
      auto domain = inferSocketType(cs);
      processUsers(cs, domain);
    }
    return;
  }

private:
  llvm::Function*
  getCalledFunction(llvm::CallSite& cs) {
    auto* calledValue = cs.getCalledValue()->stripPointerCasts();
    return llvm::dyn_cast<llvm::Function>(calledValue);
  }

  llvm::Module& module;
  SocketResults results;
};




