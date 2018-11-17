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

using namespace llvm;

using Privileges = std::bitset<COUNT>;
using ExprID     = int16_t;  // Deterministic size
using OpKey      = int16_t;  // Cannot be unsigned
constexpr int typeSize  = 16;
using ExprKey    = std::tuple<ExprID, OpKey, ExprID>;

// template specialization for ExprKey
template<>
struct DenseMapInfo<ExprKey> {
  static inline ExprKey getEmptyKey() {
    return {ExprID(-1), OpKey(0), ExprID(-1)};
  }

  static inline ExprKey getTombstoneKey() {
    return {ExprID(-2), OpKey(0), ExprID(-2)};
  }

  static unsigned getHashValue(const ExprKey& exprKey) {
    int16_t lhsAsInt     = std::get<0>(exprKey);
    int16_t opCodeAsInt  = std::get<1>(exprKey);
    int16_t rhsAsInt     = std::get<2>(exprKey);
    int16_t asArray[] = {lhsAsInt, opCodeAsInt, rhsAsInt};
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


class ExprOp {
public:
  const OpKey opCode;
  ExprOp() = default;

  explicit ExprOp(OpKey opCode)
    : opCode{opCode} { }
};

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
public:
  const ExprID lhs;
  const ExprOp op;
  const ExprID rhs;
  BinaryExprNode (ExprID lhs, OpKey op, ExprID rhs)
    : lhs{lhs}, op{op},
      rhs{rhs} { }
};



class Conjunct {
public:
  ExprID exprID;
  bool notNegated;

  Conjunct(ExprID exprID, bool notNegated) //explicit?
    : exprID{exprID},
      notNegated{notNegated} { }

  bool operator==(const Conjunct& other) const {
    return exprID == other.exprID
      && notNegated == other.exprID;
  }

  bool operator<(const Conjunct& other) const {
    if (exprID < other.exprID) {
      return true;
    }

    if (exprID == other.exprID) {
      return !(notNegated < other.notNegated);
    }
    return false;
  }

  bool operator==(const ExprID other) const {
    return exprID == other;
  }

  bool operator<(const ExprID other) const {
    return exprID < other;
  }

  void operator=(const Conjunct& other) {
    exprID     = other.exprID;
    notNegated = other.notNegated;
  }

  void print() const {
    if (!notNegated) {
      llvm::errs() << "!";
    }
    llvm::errs() << exprID;
    return;
  }
};
//Rename to conjuncts



using ConjunctIDs = std::vector<Conjunct>;
class Disjunct { // Or a Conjunction
public:
  ConjunctIDs conjunctIDs; // Conjuncts = Exprs
  Disjunct() = default;

  bool operator==(const Disjunct& other) const;
  void operator=(Disjunct);

  void print() const;
  void addConjunct(const Conjunct&);
  auto findExprID(const ExprID&) const;

  bool
  findAndReplace(const ExprID target, ExprID newID);

  bool
  operator<(const Disjunct& other) const {
    return std::lexicographical_compare (
                   this->conjunctIDs.cbegin(), this->conjunctIDs.cend(),
                   other.conjunctIDs.cbegin(), other.conjunctIDs.cend());

  }
};

// Class for handling the abstract state.
// Contains a vector of vectors of conjunctions/disjuncts
using Disjuncts = std::vector<Disjunct>;
class Disjunction {
public:
  Disjuncts disjuncts;
  Disjunction() = default;

  explicit Disjunction (Disjuncts otherDisjuncts)
    : disjuncts{otherDisjuncts} { }
  //Operator Overloads
  void operator=(Disjunction);
  bool operator==(const Disjunction&) const;
  //Member Functions
  void applyConjunct(const Conjunct&);
  void addDisjunct(const Disjunct&);
  bool hasExprID(const ExprID&) const;
  void findAndReplace(const ExprID, const ExprID);
  // Helpers
  void print() const;
  bool empty() const;

  static Disjunction
  unionDisjunctions(const Disjunction& lhs, const Disjunction& rhs) {
    //Invariant: Disjuncts are sets
    Disjunction asDisjunction;

    auto& dest_disjunct = asDisjunction.disjuncts;
    auto& disjuncts1    = lhs.disjuncts;
    auto& disjuncts2    = rhs.disjuncts;
    std::set_union(disjuncts1.begin(), disjuncts1.end(),
                   disjuncts2.begin(), disjuncts2.end(),
                   std::back_inserter(dest_disjunct));
    return asDisjunction;
  }

  // Rename to Horizontal/Vertical Negation

  Disjunction&
  simplifyAdjacentNegation() {
    auto isNegatedPair = [](const Conjunct& a, const Conjunct& b) ->  bool {
      // assert(a != b && "Failed Set property");

      llvm::errs() << "\n";
      a.print();
      llvm::errs() << "a - b";
      b.print();
      llvm::errs() << (a.exprID == b.exprID && a.notNegated == !b.notNegated );
      llvm::errs() << "\n";
      // The second check is redundant since there should never be adjacent 
      // conjuncts with the same exprID
      // Add in as assert. Remove later
      return a.exprID == b.exprID && a.notNegated == !b.notNegated;
    };
    auto hasNegatedPair = [&isNegatedPair](Disjunct& disjunct) {
      disjunct.print();
      auto found
        = std::adjacent_find(disjunct.conjunctIDs.begin(), disjunct.conjunctIDs.end(), isNegatedPair);
      return found != disjunct.conjunctIDs.end();
    };
    std::remove_if(disjuncts.begin(), disjuncts.end(), hasNegatedPair);
    return *this;
  }


  // Refactor Later
  Disjunction&
  simplifyNeighbourNegation() {

    auto isNegatedPair = [](const Conjunct& a, const Conjunct& b) ->  bool {
      // assert(a != b && "Failed Set property");

      llvm::errs() << "\n";
      a.print();
      llvm::errs() << "a - b";
      b.print();
      llvm::errs() << (a.exprID == b.exprID && a.notNegated == !b.notNegated );
      llvm::errs() << "\n";
      // The second check is redundant since there should never be adjacent 
      // conjuncts with the same exprID
      // Add in as assert. Remove later
      return a.exprID == b.exprID && a.notNegated == !b.notNegated;
    };

    auto simplify = [&isNegatedPair](auto& conjuncts1, auto& conjuncts2) {
      auto first     = conjuncts1.begin();
      auto second    = conjuncts2.begin();
      auto firstEnd  = conjuncts1.end();
      auto secondEnd = conjuncts2.end();

      while (first != firstEnd  && second != secondEnd) {
        if (isNegatedPair(*first, *second)) {
          llvm::errs() << "\n Found negated pair\n";
          // kill conjunct
          second++;
          first++;
        }
        if (second->exprID < first->exprID) {
          second++;
        }
        else {
          first++;
        }
      }
    };
    
    auto first = disjuncts.begin();
    auto second = ++first;

    for ( ; second != disjuncts.end(); first++, second++) {
      simplify(first->conjunctIDs, second->conjunctIDs);
    }
    return *this;
  }

  void // Should
  simplifyImplication() {
    llvm_unreachable("Function not Implemented");
  }


};

//---------------Method Definitions----------------//
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
Disjunct::addConjunct(const Conjunct& conjunct) {
  auto binary_insert = [](auto& conjunctIDs, auto first, auto last, auto& conjunct) {
    first = std::lower_bound(first, last, conjunct);
    if (first == last) {
      conjunctIDs.push_back(conjunct);
      return;
    }
    if (*first == conjunct) {
      return;
    }
    conjunctIDs.insert(first - 1, conjunct);
    return;
  };

  binary_insert(conjunctIDs, conjunctIDs.begin(), conjunctIDs.end(), conjunct);
  return;
}

auto
Disjunct::findExprID(const ExprID& target) const {
//  auto position
//    = std::lower_bound(conjunctIDs.begin(), conjunctIDs.end(), target,
//        [](const Conjunct& conjunct, const ExprID& target) -> bool {
//          return conjunct.exprID < target;
//        });
//  if ( position != conjunctIDs.end() && position->exprID == target) {
//    return true;
//  }
//  return false;
  return std::lower_bound(conjunctIDs.begin(), conjunctIDs.end(), target,
        [](const Conjunct& conjunct, const ExprID& target) -> bool {
          return conjunct.exprID < target;
        });
}


// Deprecate?
bool
Disjunct::findAndReplace(const ExprID target, const ExprID newID) {
  llvm::errs() << "\n---------------------------------------";
  llvm::errs() << "\nReplacing " << target << "with" << newID;
  llvm::errs() << "\n---------------------------------------";
  auto position = std::lower_bound(conjunctIDs.begin(), conjunctIDs.end(), target,
        [](Conjunct conjunct, ExprID target) -> bool {
          return conjunct.exprID < target;
        });
  if ( position != conjunctIDs.end() && position->exprID == target) {
    Conjunct asConjunct({newID, position->notNegated});
    *position = asConjunct;
    return true; //might be useful later
  }
  return false;
}


void
Disjunct::print() const {
  for (auto conjunct : conjunctIDs) {
    llvm::errs() << "(";
    conjunct.print();
    llvm::errs() << ") ";
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
  return this->disjuncts == other.disjuncts;
}

void
Disjunction::applyConjunct(const Conjunct& conjunct) {
  for (auto& disjunct : disjuncts) {
    disjunct.addConjunct(conjunct);
  }
  return;
}


//TODO: Required before dropping to Trees
//TODO: Insert disjuncts into a disjunction in order
void
Disjunction::addDisjunct(const Disjunct& disjunct) {
  disjuncts.push_back(disjunct);
  return;
}


bool
Disjunction::hasExprID(const ExprID& exprID) const {
  for (auto& disjunct : disjuncts) {
    if (disjunct.findExprID(exprID) != disjunct.conjunctIDs.end()) {
      return true;
    }
  }
  return false;
}

void
Disjunction::findAndReplace(const ExprID target, const ExprID replacement) {
  for (auto& disjunct : disjuncts) {
    disjunct.findAndReplace(target, replacement);
  }
}

bool
Disjunction::empty() const {
  return disjuncts.empty();
}

void
Disjunction::print() const {
  for (auto& disjunct : disjuncts) {
    llvm::errs() << "\n";
    disjunct.print();
  }
  return;
}

#endif
