#ifndef FUTUREFUNCTIONS_H
#define FUTUREFUNCTIONS_H

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

#include "DataflowAnalysis.h"
#include "TaintAnalysis.h"
#include "PromiseDeclarations.h"
#include "CallGraphAnalyzer.h"

using PromiseBitset    = std::bitset<COUNT>;
using FunctionsValue  = PromiseBitset;
using FunctionsState  = analysis::AbstractState<FunctionsValue>;
using FunctionsResult = analysis::DataflowResult<FunctionsValue>;
using Context = std::array<llvm::Instruction*, 2ul>;


struct
AnalysisPackage{
  tmpanalysis::tmppathResultsTy tmppathResults;
};


class PledgeCheckerBase {
public:
    PledgeCheckerBase(int ap) : argposition{ap} {}
    virtual ~PledgeCheckerBase(){}
    
    virtual FunctionsValue operator()(const llvm::CallSite, const Context& context, AnalysisPackage* package) = 0;

    int getArgPosition() {
      return argposition;
    }
private:
    int argposition;
};


using PledgeCheckerBaseUPtr  = std::unique_ptr<PledgeCheckerBase>;
using AnalysisVector  = std::vector<PledgeCheckerBaseUPtr>;

static std::vector<const llvm::Function*> functions;
static llvm::DenseMap<const llvm::Function*,size_t> functionIDs;


class FunctionPledges { 
public:
  FunctionPledges(unsigned long bitString) : promisesBitset{bitString} {}
  FunctionPledges(unsigned long bitString, AnalysisPackage* package, AnalysisVector avector) 
                                           : promisesBitset{bitString},
                                             analysisPackage{package},
                                             analysisVector{std::move(avector)} {}

  FunctionsValue
  getPromisesBitset(const llvm::CallSite& cs, const Context& context) {
   for (auto const& checker : analysisVector) {
      promisesBitset |= (*checker)(cs, context, analysisPackage);
    }
    return promisesBitset;
  }

private:
  AnalysisVector analysisVector;
  FunctionsValue promisesBitset;
  AnalysisPackage* analysisPackage;
};


class FunctionPledgesBuilder {
public:
  FunctionPledgesBuilder(unsigned long bitString) : promisesBitset  {bitString} {}
  FunctionPledgesBuilder(unsigned long bitString, AnalysisPackage* package)
                                                  : promisesBitset  {bitString},
                                                    analysisPackage {package} {}

  FunctionPledgesBuilder& add(PledgeCheckerBaseUPtr pledgeChecker) {
    analysisVector.push_back(std::move(pledgeChecker));
    return *this;
  }

  FunctionPledges build() {
    return FunctionPledges(promisesBitset.to_ulong(), analysisPackage, std::move(analysisVector));
  }

private:
  AnalysisVector analysisVector;
  AnalysisPackage* analysisPackage;
  FunctionsValue promisesBitset;
}; 


std::unordered_map<std::string, FunctionPledges>
getLibCHandlerMap(AnalysisPackage& package);



#endif
