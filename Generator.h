#ifndef GENERATOR_H
#define GENERATOR_H

#include "ConditionList.h"

static
class Generator {
public:
  //-- Methods --//
  static State    generateState(State, llvm::CallSite*);
  static Conjunct generateConjunct(Expr*, Privileges);
  static Expr*    generateExpr(llvm::Instruction* Condition);
  
  //-- Members --//
  static size_t expr_counter = 0;
private:
  std::deque<Expr*> slab;
};

static State
Generator::generateState(State oldState, llvm::CallSite* cs) {
  Privileges newPrivileges{16}; //Stub

  auto* newExpr = generateExpr(expr_counter);
  Conjunct newConjunct{newExpr, newPrivileges};

  // Make this statement work
  auto newState = oldState + newPrivileges;

  return newState;
};

#endif



// Get Conjunct + Expr To Work
// Conjunct + Conjunct also useful
