#ifndef HANDLER_H
#define HANDLER_H

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
#include <functional>

#include "TaintAnalysis.h"
#include "PromiseDeclarations.h"
#include "CallGraphAnalyzer.h"

using Context = std::array<llvm::Instruction*, 2ul>;

struct AnalysisPackage {
public:
  // Point of Configuration for turning on or off analysis
  AnalysisPackage(llvm::Module& m) { };
private:
  tmpanalysis::tmppathResultsTy tmppathResults;
};

class PrivilegeCheckerBase {
public:
  PrivilegeCheckerBase(int ap) : argposition{ap} {}
  virtual ~PrivilegeCheckerBase() {}

  virtual Privileges operator()(const llvm::CallSite,
                                const Context& context,
                                AnalysisPackage* package) = 0;

  int
  getArgPosition() {
    return argposition;
  }

private:
  int argposition;
};

using AnalysisVector = std::vector<std::unique_ptr<PrivilegeCheckerBase>>;
class FunctionPrivileges {
public:
  FunctionPrivileges(unsigned long bitString) : privileges{bitString} {}

  FunctionPrivileges(unsigned long bitString,
                     AnalysisPackage* package,
                     AnalysisVector avector)
    : privileges{bitString},
      analysisPackage{package},
      analysisVector{std::move(avector)} {}

  Privileges
  getPrivileges(const llvm::CallSite& cs, const Context& context) {
    for (auto const& checker : analysisVector) {
      privileges |= (*checker)(cs, context, analysisPackage);
    }
    return privileges;
  }

private:
  Privileges privileges;
  AnalysisPackage* analysisPackage;
  AnalysisVector analysisVector;
};


class FunctionPrivilegesBuilder {
public:
  FunctionPrivilegesBuilder(unsigned long privilegeValue)
    : privileges{privilegeValue} {}

  FunctionPrivilegesBuilder(unsigned long privilegeValue,
                            AnalysisPackage* package)
    : privileges{privilegeValue} {}

  FunctionPrivilegesBuilder&
  add(std::unique_ptr<PrivilegeCheckerBase> privilegeChecker) {
    analysisVector.push_back(std::move(privilegeChecker));
    return *this;
  }

  FunctionPrivileges
  build() {
    return FunctionPrivileges(
        privileges.to_ulong(), analysisPackage, std::move(analysisVector));
  }

private:
  Privileges privileges;
  AnalysisVector analysisVector;
  AnalysisPackage* analysisPackage;
};

using Handlers = llvm::StringMap<FunctionPrivileges>;
struct LibCHandlersMap {
public:
  LibCHandlersMap(AnalysisPackage& package) {
    buildLibCHandlers(package);
  }
  
  auto
  find(llvm::StringRef functionName) {
    return libCHandlers.find(functionName);
  }

  auto
  end() {
    return libCHandlers.end();
  }

private:
  void buildLibCHandlers(AnalysisPackage& package);
  Handlers libCHandlers;
}; // end class LibCHandlersMap



class PrivilegeResolver {
public:
  PrivilegeResolver(llvm::Module& m)
    : module{m},
      analysisPackage{std::make_unique<AnalysisPackage>(m)},
      mapInterface{std::make_unique<MapInterface>(*analysisPackage.get())} { }

  template <Promises promise>
  bool
  hasPrivilege(llvm::CallSite cs, const Context& context) {
    Privileges privs{};
    setPrivileges(privs, cs, context);
    return privs.test(promise);
  }

  bool
  setPrivileges(Privileges& requiredPrivileges,
                llvm::CallSite cs,
                const Context& context) {
    if (auto function = getCalledFunction(cs)) {
      requiredPrivileges |= mapInterface->getPrivilegesFor(cs, context);
      return true;
    }
    return false;
  }
  
  template <typename Worker>
  void
  handleStubs(Worker& worker, llvm::CallSite& cs) {
    /* bool isWhiteListed  = mapInterface->isWhiteListed(cs);
     * bool isValueClobber = !isWhiteListed && mapInterface->isValueClobber(cs);
     * worker(isWhiteListed, isValueClobber); */
    bool isWhiteListed = mapInterface->hasSpecifications(cs); /*||hasWhiteListEntry();*/
    worker(isWhiteListed); 
    return;
  }
  
  auto
  dumpToFile(llvm::Module& m) {
    mapInterface->dumpUnknowns(m);
  }

private:
  LibCHandlersMap& buildLibCHandlers(AnalysisPackage&); // Handler.cpp
  
  static
  llvm::Function*
  getCalledFunction(llvm::CallSite cs) {
    auto* calledValue = cs.getCalledValue()->stripPointerCasts();
    return llvm::dyn_cast<llvm::Function>(calledValue);
  }

  class MapInterface {
  public:
    MapInterface(AnalysisPackage& package) 
      : libCHandlers{package} {}

    Privileges
    getPrivilegesFor(const llvm::CallSite& cs, const Context& context) {
      // Point of configuration for specifications
      if (auto privileges =
              getPrivilegesForImpl(cs, context, syscallBitsetMap)) {
        return *privileges;
      } else if (auto privileges =
                     getPrivilegesForImpl(cs, context, libCHandlers)) {
        return *privileges;
      } else {
        addToUnknowns(cs);
        //llvm::errs() << "\nNo function found for CallSite "
        //             << *cs.getInstruction();
        return {};
      }
    }

    bool
    hasSpecifications(llvm::CallSite& cs) {
      auto name = getFunctionName(cs);
      return libCHandlers.find(name) != libCHandlers.end()
          || syscallBitsetMap.find(name) != syscallBitsetMap.end();
    }

    void
    dumpUnknowns(llvm::Module& module) {
      auto ec = std::error_code{};
      auto [modulePath, _]  = module.getName().split(".");
      auto [__, moduleName] = modulePath.rsplit("/");
      auto baseString = "/home/shreeasish/pledgerize-reboot/hoisting/logs/"
                        + moduleName.str();
      std::string unknownsFileName = baseString + "/ext_unknown";
      auto unknownsFile = llvm::raw_fd_ostream{unknownsFileName, ec};
      dumpUnknownFunctions(unknownsFile);
      unknownsFile.close();
    }

  private:
    template <typename StringMapTy>
    std::optional<Privileges>
    getPrivilegesForImpl(const llvm::CallSite& cs, const Context& context,
                                                    StringMapTy stringMap) {
      auto functionName = getFunctionName(cs);
      //return stringMap.find(functionName) == stringMap.end()
      //           ? std::optional(stringMap.find(functionName)->second)
      //           : std::nullopt;
      if (auto found = stringMap.find(functionName); found != stringMap.end()) {
        return found->second;
      } 
      return {};
    }

    std::optional<Privileges>
    getPrivilegesForImpl(const llvm::CallSite& cs, const Context& context,
                                                LibCHandlersMap& handlers) {
      auto functionName = getFunctionName(cs);
      if (auto found = handlers.find(functionName); found != handlers.end()) {
        return found->second.getPrivileges(cs, context);
      } 
      return {};
    }

    llvm::StringRef
    getFunctionName(const llvm::CallSite& cs) const {
      return getCalledFunction(cs)->getName();
    }
    

    LibCHandlersMap libCHandlers;
    
    // Logging Helpers
    void
    dumpUnknownFunctions(llvm::raw_ostream& outs) {
      outs << "name, callingInstruction, caller";
      auto dump = [&outs](auto* inst, llvm::StringRef name) {
        auto* caller = inst->getParent()->getParent();
        outs << "\n" << name << "|:" << *inst << "|:" << caller->getName();
      };

      for (auto [inst, name] : unknownFunctions) {
        dump(inst, name);
      }
      return;
    }

    void
    addToUnknowns(const llvm::CallSite& cs) {
      auto* value = cs.getInstruction();
      unknownFunctions[value] = getFunctionName(cs);
    }
    llvm::DenseMap<llvm::Instruction*, llvm::StringRef> unknownFunctions;
  }; // End Class MapInterface

  llvm::Module& module;
  std::unique_ptr<AnalysisPackage> analysisPackage;
  std::unique_ptr<MapInterface>    mapInterface;

};

// std::unordered_map<std::string, FunctionPledges>
#endif