#ifndef CONDITIONLIST_H
#define CONDITIONLIST_H

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>
#include <queue>

using std::shared_ptr;
using std::queue;

using namespace llvm;

using Privileges = std::bitset<COUNT>;

static llvm::Function*
getCalledFunction(llvm::CallSite cs) {
  if (!cs.getInstruction()) return nullptr;

  auto* called = cs.getCalledValue()->stripPointerCasts();
  if (called->getName().contains("llvm")) return nullptr;

  return llvm::dyn_cast<llvm::Function>(called);
}

// Stub Class for Expression Trees
class Expr {
public:
  Expr (size_t id, llvm::Instruction* condition = nullptr)
    : condition{condition},
      id{id} { };

  bool operator==(Expr*) const;

  llvm::Instruction* condition;

  size_t
  getId () {
    return id;
  }

private:
  const size_t id;
};


// Conjunctions should hold pointers to Exprs
// Should work as copyable instances
class Conjunct {
public:
  Conjunct(Conjunct* conjunct)
    :exprs{conjunct->exprs} {}

  Conjunct(Expr* expr, Privileges privileges)
    : exprs{expr},
      privileges{privileges} {}


  Conjunct& operator=(Conjunct*);

  std::vector<Expr *> exprs;
  Privileges privileges;
};

// Class for handling the abstract state.
// Contains a vector of vectors of conjuncts
// Should also work as copyable instances
class State {
public:
  State(State* state)
    : conjuncts{state->conjuncts} {}

  //Operator Overloads
  State& operator=(State*);
  bool  operator==(State ) const;
  void   operator+(State );

  std::vector<Conjunct *> conjuncts;
};

// Method Definitions // ------------------ //


bool
Expr::operator==(Expr* expr) const {
  return this->id == expr->id;
}

Conjunct&
Conjunct::operator=(Conjunct* rhs) {
  this->exprs = rhs->exprs;
  return *this;
}

State&
State::operator=(State* rhs) {
  this->conjuncts = rhs->conjuncts;
  return *this;
}

bool
State::operator==(State rhs) const {
  if (rhs.conjuncts.size() != this->conjuncts.size()) return false;

  // Move to member function
  auto checkExprs = [](auto conjunct1, auto conjunct2) -> bool {
    if (conjunct1.size() != conjunct2.size()) return false;

    for (auto iter1 = conjunct1.begin(), iter2 = conjunct2.begin();
        iter1 != conjunct1.end() && iter2 != conjunct2.end();
        ++iter1, ++iter2)
    {
        if (*iter1 != *iter2) {
          return false;
        }

    }

    return true;
  };

}

void
State::operator+(State rhs) {
  this->conjuncts
    .insert(this->conjuncts.begin(), rhs.conjuncts.begin(), rhs.conjuncts.end());
  // TODO: Prune Away
  return;
}

#endif
