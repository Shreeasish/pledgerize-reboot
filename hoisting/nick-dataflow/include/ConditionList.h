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

#include <memory>
#include <queue>

using std::shared_ptr;
using std::queue;

using namespace llvm;

using Privileges    = std::bitset<COUNT>;
using ExprID        = size_t;
using LLVMBinaryOps = llvm::Instruction::BinaryOps;
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

// Expression Trees
class ExprNode {
protected:
  const ExprID id;

public:
  ExprNode (ExprID id)
    : id{id} { };

  bool operator==(ExprNode) const;

  ExprID
  getId () const {
    return id;
  }

  virtual ~ExprNode() {}
};

class ConstantExprNode : public ExprNode {
  const llvm::Constant* constant;

public:
  ConstantExprNode (ExprID id, llvm::Constant* constant)
    : ExprNode{id},
      constant{constant} { }
};

class ValueExprNode : public ExprNode {
  const llvm::Value* value;

public:
  ValueExprNode (ExprID id, llvm::Value* value)
    : ExprNode{id},
      value{value} { }
};

class BinaryExprNode : public ExprNode {
  const ExprID lhs;
  const ExprID rhs;
  const LLVMBinaryOps binOp; // 'Operator' is reserved by llvm

public:
  BinaryExprNode (ExprID id, ExprID lhs, ExprID rhs, LLVMBinaryOps binOperator)
    : ExprNode{id},
      lhs{lhs}, rhs{rhs},
      binOp{binOperator} { }
};
using ConjunctIDs = std::vector<ExprID>; //Should this be at the top?

class Disjunct { // Or a Conjunction
  ConjunctIDs conjunctIDs; // Conjuncts = Exprs

public:
  Disjunct() = default;

  explicit Disjunct (ExprID exprID)
    : conjunctIDs{exprID} { }

  void operator=(Disjunct);
  void print() const;
};
using Disjuncts = std::vector<Disjunct>;

// Class for handling the abstract state.
// Contains a vector of vectors of conjunctions/disjuncts
class Disjunction {
  Disjuncts disjuncts; // Vector of conjunctions

public:
  Disjunction() = default;

  explicit Disjunction (Disjuncts otherDisjuncts)
    : disjuncts{otherDisjuncts} { }

  //Operator Overloads
  void operator=(Disjunction);
  bool operator==(Disjunction) const;

  Disjunction operator+(Disjunction);

  void print() const;
};

// Method Definitions // ------------------ //

// ExprNodeClass

bool
ExprNode::operator==(ExprNode other) const {
  return this->id == other.id;
}

// Disjunct Class
void
Disjunct::operator=(Disjunct other) {
  this->conjunctIDs = other.conjunctIDs;
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

// Disjunction Class
void
Disjunction::operator=(Disjunction other) {
  this->disjuncts = other.disjuncts;
  return;
}

bool
Disjunction::operator==(Disjunction other) const {
  return false;
}

Disjunction
Disjunction::operator+(Disjunction other) {
  Disjuncts tempDisjuncts{this->disjuncts};
  tempDisjuncts.
    insert(tempDisjuncts.end(), other.disjuncts.begin(), other.disjuncts.begin());
  return Disjunction{tempDisjuncts};
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
