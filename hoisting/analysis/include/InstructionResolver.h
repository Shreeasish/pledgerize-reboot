#ifndef INSTRUCTIONRESOLVER_H
#define INSTRUCTIONRESOLVER_H

#include "ConditionList.h"
#include "Generator.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "CustomDataflowAnalysis.h"

#include <unordered_map>
#include <string>
#include <memory>
#include <vector>
#include <deque>

namespace lowering {
  class InstructionResolver;
  struct LocationState;
}

using InstructionResolver = lowering::InstructionResolver;

struct lowering::LocationState {
public:
  const Context& context;
  llvm::Instruction* const parentCallSite;
  llvm::Function* const callee;
  llvm::Instruction* const location;
  const Disjunction& state; /*state[nullptr]*/
};

class lowering::InstructionResolver {
public:
  InstructionResolver(const llvm::Module& m, Generator* const g) 
    : module{m}, generator{g} { }

  Disjunction
  operator()(const LocationState& locationState) {
    auto toFix = getOutOfScopeValues(locationState);
    return locationState.state; //!!! Remove
  }
  
private:
  [[maybe_unused]]
  const llvm::Module& module;
  Generator* const generator;
  struct OutOfScopeFinder;

  std::vector<llvm::Value*>
  getOutOfScopeValues(const LocationState& locationState) const;
};


struct InstructionResolver::OutOfScopeFinder {
public:
  OutOfScopeFinder(const llvm::Function* parent, std::vector<llvm::Value*>& retVec) 
    : locationParent{parent}, outOfScopeValues{retVec} {}

  void
  operator()(ExprID id, ConstantExprNode node) {
    return;
  }

  void
  operator()(ExprID id, BinaryExprNode node) {
    return;
  }

  void
  operator()(ExprID id, ValueExprNode node) {
    if (!node.value) {
      return;
    }
    auto* asInst = llvm::dyn_cast<llvm::Instruction>(node.value);
    const llvm::Function* valueParent = asInst->getParent()->getParent();
    if (valueParent != locationParent) {
      outOfScopeValues.emplace_back(node.value);
   }
    return;
  }
  

private:
  const llvm::Function* locationParent;
  std::vector<llvm::Value*>& outOfScopeValues;
};

std::vector<llvm::Value*>
InstructionResolver::getOutOfScopeValues(const LocationState& locationState) const {
  const llvm::Function* loweringFunction =
      locationState.location->getFunction();
  std::vector<llvm::Value*> outOfScopeValues;
  auto visitor = OutOfScopeFinder{loweringFunction, outOfScopeValues};
  generator->preOrderFor(locationState.state, visitor);
  return outOfScopeValues;
}

#endif
