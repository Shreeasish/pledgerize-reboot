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
#include "PromiseDeclarations.h"

using Privileges = std::bitset<COUNT>;
using ExprID     = int16_t;  // Deterministic size
using OpKey      = int16_t;  // Negatives used to hash sentinels
using ExprKey    = std::tuple<ExprID, OpKey, ExprID>;

constexpr int typeSize{16};
namespace OpIDs {
constexpr OpKey Alias{100};
constexpr OpKey Cast{101};
constexpr OpKey last{102};
}  // namespace OpIDs

namespace ReservedExprIDs {
constexpr ExprID vacuousExprID = 0;
constexpr ExprID emptyExprID   = 1;
}  // namespace ReservedExprIDs

using Predicate = llvm::CmpInst::Predicate;

template<>
struct llvm::DenseMapInfo<ExprKey> {
  static inline ExprKey
  getEmptyKey() {
    return {ExprID(-1), OpKey(0), ExprID(-1)};
  }

  static inline ExprKey
  getTombstoneKey() {
    return {ExprID(-2), OpKey(0), ExprID(-2)};
  }

  static unsigned
  getHashValue(const ExprKey& exprKey) {
    int16_t lhsAsInt    = std::get<0>(exprKey);
    int16_t opCodeAsInt = std::get<1>(exprKey);
    int16_t rhsAsInt    = std::get<2>(exprKey);
    int16_t asArray[]   = {lhsAsInt, opCodeAsInt, rhsAsInt};
    return llvm::hash_combine_range(std::begin(asArray), std::end(asArray));
  }

  static bool
  isEqual(const ExprKey& LHS, const ExprKey& RHS) {
    return LHS == RHS;
  }
};

/// FIXME: Why is this here?
static llvm::Function*
getCalledFunction(llvm::CallSite cs) {
  if (!cs.getInstruction()) {
    return nullptr;
  }

  auto* called = cs.getCalledValue()->stripPointerCasts();
  if (called->getName().contains("llvm")) {
    return nullptr;
  }
  return llvm::dyn_cast<llvm::Function>(called);
}

class ExprOp {
public:
  const OpKey opCode;
  const Predicate predicate;
  ExprOp() = default;

  explicit ExprOp(OpKey opCode, Predicate pred)
    : opCode{opCode}, predicate{pred} {}

  bool
  operator==(OpKey opCode) const {
    return this->opCode == opCode;
  }

  bool
  operator==(int opCode) const {
    return this->opCode == (OpKey) opCode;
  }
};

class ConstantExprNode {
public:
  llvm::Constant* const constant;
  ConstantExprNode (llvm::Constant* const constant)
    : constant{constant} { }
};

class ValueExprNode {
public:
  llvm::Value* const value;
  ValueExprNode (llvm::Value* const value)
    : value{value} { }
};

class BinaryExprNode {
public:
  const ExprID lhs;
  const ExprOp op;
  const ExprID rhs;
  llvm::Value* const value = nullptr;
  BinaryExprNode(ExprID lhs,
                 OpKey op,
                 ExprID rhs,
                 Predicate pred,
                 llvm::Value* const val)
    : lhs{lhs}, op{op, pred}, rhs{rhs}, value{val} {}
};



class Conjunct {
public:
  ExprID exprID;
  bool notNegated;

  Conjunct(ExprID exprID, bool notNegated) //explicit?
    : exprID{exprID},
      notNegated{notNegated} { }

  bool
  operator==(const Conjunct& other) const {
    return exprID == other.exprID
      && !(notNegated xor other.notNegated);
  }

  bool
  operator<(const Conjunct& other) const {
    if (exprID < other.exprID) {
      return true;
    }
    if (exprID == other.exprID) {
      return notNegated < other.notNegated;
    }
    return false;
  }

  Conjunct
  operator!() const {
    Conjunct newConjunct = *this;
    newConjunct.notNegated = !notNegated;
    return newConjunct;
  }

  //void
  //operator=(const Conjunct& other) {
  //  exprID     = other.exprID;
  //  notNegated = other.notNegated;
  //}

  bool
  operator<(const ExprID other) const {
    return exprID < other;
  }

  bool
  operator==(const ExprID other) const {
    return exprID == other;
  }

  void print(llvm::raw_ostream& out) const {
    if (!notNegated) {
      out << "-";
    }
    out << exprID;
    return;
  }
};
//Rename to conjuncts



using ConjunctIDs = std::vector<Conjunct>;
class Disjunct { // Or a Conjunction
public:
  ConjunctIDs conjunctIDs; // Conjuncts = Exprs
  //Disjunct() = default;
  auto findExprID(const ExprID&) const;  // returns an iterator
  bool findAndReplace(const ExprID target, ExprID newID);
  bool operator<(const Disjunct& other) const;
  bool operator==(const Disjunct& other) const;

  void addConjunct(const Conjunct&);
  //void operator=(const Disjunct);
  //Disjunct& operator=(Disjunct&&);
  void print(llvm::raw_ostream&) const;

  auto
  size() const {
    return conjunctIDs.size();
  };

  auto
  begin() const {
    return conjunctIDs.begin();
  }
  auto
  end() const {
    return conjunctIDs.end();
  }

  auto
  begin() {
    return conjunctIDs.begin();
  }
  auto
  end() {
    return conjunctIDs.end();
  }

  auto
  rbegin() const {
    return conjunctIDs.rbegin();
  }
  auto
  rend() const {
    return conjunctIDs.rend();
  }

  auto
  rbegin() {
    return conjunctIDs.rbegin();
  }
  auto
  rend() {
    return conjunctIDs.rend();
  }

};

// Class for handling the abstract state.
// Contains a vector of vectors of conjunctions/disjuncts
using Disjuncts = std::vector<Disjunct>;
class Disjunction {
public:
  Disjuncts disjuncts;

  explicit Disjunction (Disjuncts otherDisjuncts)
    : disjuncts{otherDisjuncts} { }
  Disjunction() = default;

  //Operator Overloads
  //bool operator=(Disjunct&&);
  bool operator==(const Disjunction&) const;
  bool operator<(const Disjunction&) const;
  //Member Functions
  void applyConjunct(const Conjunct&);
  void addDisjunct(const Disjunct&);
  bool hasExprID(const ExprID&) const;
  void findAndReplace(const ExprID, const ExprID);
  // Helpers
  void print(llvm::raw_ostream&) const;

  auto begin() const { return disjuncts.begin(); }
  auto end() const { return disjuncts.end(); }
  
  auto begin() { return disjuncts.begin(); }
  auto end() { return disjuncts.end(); }

  bool isEmpty() const { return disjuncts.begin() == disjuncts.end(); }
  bool isVacuouslyTrue() const;
  
  size_t conjunctCount() const {
    return 
      std::accumulate(disjuncts.begin(), disjuncts.end(), 0, 
          [](const auto& count, const auto& disjunct) {
          return disjunct.size() + count;
        });
  }

  size_t disjunctCount() const {
    return disjuncts.size();
  }

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
  simplifyComplements() {
    auto isNegatedPair = [](const Conjunct& a, const Conjunct& b) ->  bool {
      // The second check is redundant since there should never be adjacent
      // conjuncts with the same exprID
      // Add in as assert. Remove later
      return a.exprID == b.exprID && (a.notNegated xor b.notNegated);
    };
    auto hasNegatedPair = [&isNegatedPair](Disjunct& disjunct) {
      auto found
        = std::adjacent_find(disjunct.conjunctIDs.begin(), disjunct.conjunctIDs.end(), isNegatedPair);
      return found != disjunct.conjunctIDs.end();
    };
    auto eraseIt = std::remove_if(disjuncts.begin(), disjuncts.end(), hasNegatedPair);
    disjuncts.erase(eraseIt, disjuncts.end());
    std::sort(disjuncts.begin(),disjuncts.end());
    return *this;
  }


  Disjunction&
  simplifyRedundancies(Conjunct vacuous) {

    auto isComplementary = [](auto& a, auto& b) -> bool {
      return a->notNegated xor b->notNegated;
    };

    auto checkPrev = [&](auto& iter1, auto& iter2, auto& first, auto& second) {
      return isComplementary(iter1, iter2) 
        && std::equal(iter1 + 1,  first.end(),
                      iter2 + 1, second.end());
    };

    bool removed = false;
    auto prevDisjunct = disjuncts.begin();
    auto isRedundant = [&] (auto& disjunct) {
      if (prevDisjunct->size() != disjunct.size()) {
        prevDisjunct++;
        return false;
      }
      auto iter1 = prevDisjunct->begin();
      auto iter2 = disjunct.begin();
      while (iter2 < disjunct.end()) {
        if (*iter1 == *iter2) {
          iter1++;
          iter2++;
        } else if (checkPrev(iter1, iter2, *prevDisjunct, disjunct)) {
          prevDisjunct->conjunctIDs.erase(iter1);
          removed |= true;
          return true; // This removes the current disjunct
        } else {
          prevDisjunct++;
          return false;
        }
      }
      prevDisjunct++;
      return false;
    };

    auto from = std::remove_if(disjuncts.begin() + 1, disjuncts.end(), isRedundant);
    disjuncts.erase(from, disjuncts.end());
    this->removeEmpties();
    if (this->isEmpty() && removed) {
      Disjunct vacuousDisjunct{};
      vacuousDisjunct.addConjunct(vacuous);
      this->addDisjunct(vacuousDisjunct);
    }

    return *this;
  }

  Disjunction& // Should
  simplifyImplication() {
    std::sort(disjuncts.begin(), disjuncts.end(),
         [ ] (const auto& lhs, const auto& rhs) {
           return lhs.conjunctIDs.size() < rhs.conjunctIDs.size();
         });

    auto smaller = disjuncts.begin();
    auto end  = disjuncts.end();
    auto isSubset = [&smaller](const Disjunct larger) { //iter1 is subset of iter2
      auto it1    = larger.begin();
      auto it2    = smaller->begin();
      while (it2 < smaller->end()) {
        while (it1 < larger.end() && *it1 < *it2) {
          it1++;
        }
        if (it1 == larger.end() || !(*it1 == *it2)) {
          return false;
        }
        it2++;
      }
      return true;
    };
    while (smaller + 1 < end) {
      end = std::remove_if(smaller + 1, end, isSubset);
      smaller++;
    }

    disjuncts.erase(end, disjuncts.end());
    std::sort(disjuncts.begin(), disjuncts.end());

    return *this;
  }

  Disjunction&
  simplifyUnique() {
    auto eraseIt = std::unique(disjuncts.begin(), disjuncts.end());
    disjuncts.erase(eraseIt, disjuncts.end());
    return *this;
  }

  Disjunction&
  simplifyTrues() {
    if (!isVacuouslyTrue()) {
      return *this;
    }
    disjuncts.erase(this->begin() + 1, this->end());
    //llvm::errs() << "\nAfter simplifications" ;
    //this->print(llvm::errs());
    return *this;
  }

private:
  void
  stripTrues() {
    bool isUnary = false;
    auto isVacExpr  = [&isUnary](auto& conjunct) {
      return !isUnary && conjunct.exprID == ReservedExprIDs::vacuousExprID;
    };
    for (auto& disjunct : disjuncts) {
      isUnary = disjunct.conjunctIDs.size() == 1;
      auto eraseIt = std::remove_if(
          disjunct.conjunctIDs.begin(), disjunct.conjunctIDs.end(), isVacExpr);
      disjunct.conjunctIDs.erase(eraseIt, disjunct.conjunctIDs.end());
    }
    return;
  }

  void
  removeEmpties() {
    auto eraseIt = std::remove_if(disjuncts.begin(), disjuncts.end(), [] (const auto& disjunct) {
          return disjunct.size() == 0;
        });
    disjuncts.erase(eraseIt, disjuncts.end());
  }
};

//---------------Method Definitions----------------//


/// ---------------------------------------- ///
/// -----------Disjuncts-------------------- ///
/// ---------------------------------------- ///


bool
Disjunct::operator<(const Disjunct& other) const {
  return std::lexicographical_compare (
      this->conjunctIDs.cbegin(), this->conjunctIDs.cend(),
      other.conjunctIDs.cbegin(), other.conjunctIDs.cend());
}

bool
Disjunct::operator==(const Disjunct& other) const {
  return conjunctIDs == other.conjunctIDs;
}

//void
//Disjunct::operator=(Disjunct&& other) {
//  
//  return 
//}

/// Invariant: The conjunctions will be sorted
/// Maintains  uniqueness
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
    conjunctIDs.insert(first, conjunct);
    return;
  };

  binary_insert(conjunctIDs, conjunctIDs.begin(), conjunctIDs.end(), conjunct);
  return;
}

auto
Disjunct::findExprID(const ExprID& target) const {
return std::lower_bound(
    conjunctIDs.begin(),
    conjunctIDs.end(),
    target,
    [](const Conjunct& conjunct, const ExprID& target) -> bool {
      return conjunct.exprID < target;
    });
}


bool
Disjunct::findAndReplace(const ExprID target, const ExprID newID) {
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
Disjunct::print(llvm::raw_ostream& out) const {
  if (conjunctIDs.empty()) {
    out << "empty disjunct";
  }
  for (auto conjunct : conjunctIDs) {
    //out << "(";
    conjunct.print(out);
    //out << ")";
    out << ",";
  }
  return;
}

/// ---------------------------------------- ///
/// ---------- Disjunctions ---------------- ///
/// ---------------------------------------- ///

bool
Disjunction::operator==(const Disjunction& other) const {
  return this->disjuncts == other.disjuncts;
}

void
Disjunction::applyConjunct(const Conjunct& conjunct) {
  for (auto& disjunct : disjuncts) {
    disjunct.addConjunct(conjunct);
  }
  this->stripTrues();
  return;
}

// TODO: Clean
void
Disjunction::addDisjunct(const Disjunct& disjunct) {
  auto upper_bound = std::upper_bound(disjuncts.begin(), disjuncts.end(), disjunct);
  if (upper_bound == disjuncts.end()) {
    disjuncts.insert(upper_bound, disjunct);
    return;
  }
  if (*upper_bound == disjunct) {
    return;
  }
  disjuncts.insert(upper_bound, disjunct);
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

void
Disjunction::print(llvm::raw_ostream& out) const {
  for (auto& disjunct : disjuncts) {
    out << "\n";
    disjunct.print(out);
  }
  return;
}

bool
Disjunction::isVacuouslyTrue() const {
  if (this->isEmpty()) {
    return false;
  }
  const Disjunct& firstDisjunct{*(this->begin())};
  const Conjunct& firstConjunct{*(firstDisjunct.begin())};
  return firstConjunct == ReservedExprIDs::vacuousExprID
         && firstDisjunct.size() == 1;
}
    /* Enable Debug
     * llvm::errs() << "\n =====================";
     * llvm::errs() << "\n BEFORE transfer at -- " << value;
     * state[nullptr].print(llvm::errs());
     */

    /* Enable Debug
     * llvm::errs() << "\n AFTER transfer at -- " << value;
     * state[nullptr].print(llvm::errs());
     *  generator->dumpState();
     * llvm::errs() << "\n =====================";
     *  printer->insertIR(inst, state[nullptr]); */



#endif
