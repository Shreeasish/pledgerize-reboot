#ifndef CONDITIONLIST_H
#define CONDITIONLIST_H

#include "llvm/ADT/Hashing.h"
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

#include <algorithm>
#include <memory>
#include <queue>

using std::queue;
using std::shared_ptr;

using namespace llvm;

using Privileges    = std::bitset<COUNT>;
using ExprID        = size_t;
using LLVMBinaryOps = llvm::Instruction::BinaryOps; // NOT llvm::BinaryOperator
using ExprKey       = std::tuple<ExprID, LLVMBinaryOps, ExprID>;

// template specialization for ExprKey
template<>
struct DenseMapInfo<ExprKey> {
  static inline ExprKey getEmptyKey() {
    return {ExprID(-1), LLVMBinaryOps(0), ExprID(-1)};
  }

  static inline ExprKey getTombstoneKey() {
    return {ExprID(-2), LLVMBinaryOps(0), ExprID(-2)};
  }

  static unsigned getHashValue(const ExprKey& exprKey) {
    ExprID asArray[] = {std::get<0>(exprKey), std::get<1>(exprKey), std::get<2>(exprKey)};
    return llvm::hash_combine_range(std::begin(asArray), std::end(asArray));
  }

  static bool isEqual(const ExprKey &LHS, const ExprKey &RHS) {
    return LHS == RHS;
  }
};


static llvm::Function*
getCalledFunction(llvm::CallSite cs) {
  if (!cs.getInstruction()) return nullptr;

  auto* called = cs.getCalledValue()->stripPointerCasts();
  if (called->getName().contains("llvm")) return nullptr;

  return llvm::dyn_cast<llvm::Function>(called);
}


class ConstantExprNode {
  const llvm::Constant* constant;
public:
  ConstantExprNode (const llvm::Constant* constant)
    : constant{constant} { }
};

class ValueExprNode {
  const llvm::Value* value;
public:
  ValueExprNode (const llvm::Value* value)
    : value{value} { }
};

class BinaryExprNode {
  const ExprID lhs;
  const ExprID rhs;
  const LLVMBinaryOps binOp;
  // 'Operator' is reserved by llvm
public:
  BinaryExprNode (ExprID lhs, ExprID rhs, LLVMBinaryOps binOperator)
    : lhs{lhs}, rhs{rhs},
      binOp{binOperator} { }
};
using ConjunctIDs = std::vector<ExprID>; //Should this be at the top?

class Disjunct { // Or a Conjunction
private:
  ConjunctIDs conjunctIDs; // Conjuncts = Exprs
public:
  Disjunct() = default;

  bool operator==(const Disjunct& other) const;
  void operator=(Disjunct);

  void print() const;
  void addConjunct(ExprID);
  
  bool
  operator<(const Disjunct& other) const {
    return std::lexicographical_compare (
                   this->conjunctIDs.cbegin(), this->conjunctIDs.cend(),
                   other.conjunctIDs.cbegin(), other.conjunctIDs.cend());
  }
};
using Disjuncts = std::vector<Disjunct>;

// Class for handling the abstract state.
// Contains a vector of vectors of conjunctions/disjuncts
class Disjunction {
  // Vector of conjunctions
private:
  Disjuncts disjuncts;

public:
  Disjunction() = default;

  explicit Disjunction (Disjuncts otherDisjuncts)
    : disjuncts{otherDisjuncts} { }

  //Operator Overloads
  void operator=(Disjunction);
  bool operator==(const Disjunction&) const;
  //Member Functions
  Disjunction operator+(Disjunction) const;
  void addDisjunction(const Disjunction&);
  void addConjunct(const ExprID exprID);
  void addDisjunct(const Disjunct&);
  // Helpers
  void print() const;
  bool empty() const;
};

//------------------- Method Definitions -------------------//
bool 
Disjunct::operator==(const Disjunct& other) const {
  return conjunctIDs == other.conjunctIDs;
}

void
Disjunct::operator=(Disjunct other) {
  this->conjunctIDs = other.conjunctIDs;
  return;
}

//Invariant: The conjunctions will be sorted
void
Disjunct::addConjunct(const ExprID exprID) {
  auto binary_insert = [ ](auto conjunctIDs, auto first, auto last, auto exprID) {
    first = std::lower_bound(first, last, exprID);
    if (first == last) {
      conjunctIDs.push_back(exprID);
      return;
    }
    if (*first == exprID) return;
    conjunctIDs.insert(first - 1, exprID);
    return;
  };

  binary_insert(conjunctIDs, conjunctIDs.begin(), conjunctIDs.end(), exprID);
  return;
}

void
Disjunct::print() const {
  llvm::outs() << "\n" ;
  for (auto id : conjunctIDs) {
    llvm::outs() << id << "-";
  }
  return;
}

void
Disjunction::operator=(Disjunction other) {
  this->disjuncts = other.disjuncts;
  return;
}

bool
Disjunction::operator==(const Disjunction& other) const {
  llvm::errs() << "\n Evaluated True";
  return this->disjuncts == other.disjuncts;
}

Disjunction
Disjunction::operator+(Disjunction other) const {
  Disjuncts tempDisjuncts{this->disjuncts};
  tempDisjuncts.
    insert(tempDisjuncts.end(), other.disjuncts.begin(), other.disjuncts.begin());
  return Disjunction{tempDisjuncts};
}


void
Disjunction::addConjunct(const ExprID exprID) {
  for (auto disjunct : disjuncts) {
    disjunct.addConjunct(exprID);
  }
  return;
}

void 
Disjunction::addDisjunct(const Disjunct& disjunct) {
  disjuncts.push_back(disjunct); //TODO: Insert into order
  return;
}

void
Disjunction::addDisjunction(const Disjunction& other) {
  Disjuncts newDisjuncts{ };
  std::merge(disjuncts.begin(),         disjuncts.end(),
       other.disjuncts.begin(), other.disjuncts.begin(),
       newDisjuncts.begin()); // TODO: Fix Duplication
  disjuncts = newDisjuncts;
  return;
}

bool
Disjunction::empty() const { 
  return disjuncts.empty();
}

void
Disjunction::print() const {
  for (auto disjunct : disjuncts) {
    llvm::outs() << "\n";
    disjunct.print();
  }
  return;
}

#endif
