#ifndef PRINTER_H
#define PRINTER_H 

#include "ConditionList.h"
#include "Generator.h"

class Printer {
public:
  Printer(Generator* g, llvm::raw_ostream& out) 
    : generator{g},
      out{out} {}

  void // Point of configuration for async queue
  printState(llvm::Instruction* const location, const Disjunction& disjunction) {
    if (disjunction.isEmpty()) {
      return;
    }
    printStateTrees(location, disjunction);
  }

private: 
  Generator* generator;
  unsigned int counter = 0;
  llvm::raw_ostream& out;

  std::string
  getDebugData(llvm::Instruction* const location) const {
    return location->getName();
  }

  void
  printLineNumber(llvm::Instruction* const inst) const {
    if (const llvm::DILocation* debugLoc = inst->getDebugLoc()) {
      out << "At " << debugLoc->getFilename() << " line "
          << debugLoc->getLine();
    } else {
      out << "At an unknown location"
          << *inst
          << "\n";
    }       
  }

  template <class... Ts>
  struct overloaded : Ts... {
    using Ts::operator()...;
  };
  template <class... Ts>
  overloaded(Ts...)->overloaded<Ts...>;

  void
  printTree(const Conjunct conjunct) const {
    auto visitor = overloaded {
      [this](ExprID exprID, const ConstantExprNode node) {
        out << "\t" << exprID;
        if (node.constant) {
          out << " [shape=box,label=\"" << *(node.constant) << "\"]\n";
        } else {
          out << " [shape=box,label=\"" << " nullptr " << "\"]\n";
        }
      },
      [this](ExprID exprID, const BinaryExprNode node) {
        auto lhsID = node.lhs;
        auto rhsID = node.rhs;
        out << "\t" << exprID 
            << " -> { " << lhsID << "; " << rhsID << " }\n";
      },
      [this](ExprID exprID, const ValueExprNode node) {
        out << "\t" << exprID
            << " [shape=box,label=\"" << *(node.value) << "\"]\n";
      }
    };
    generator->for_each(conjunct, visitor);
  }

  void
  printStateTrees(llvm::Instruction* const location, const Disjunction disjunction) {
    for (auto disjunct : disjunction) {
      for (auto conjunct : disjunct) {
        out << "\nDigraph " << counter++ << " {\n"
            << "\tlabel = <";
        printLineNumber(location);
        out << ">\n"
            << "\tlabelloc = \"t\";\n";
        printTree(conjunct);
        out << "}\n";
      }
    }
  }


//Digraph 00 {
//
//  label = <The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>;
//  labelloc = "t"; // place the label at the top (b seems to be default)
//
//  node [shape=plaintext]
//
//  FOO -> {BAR, BAZ};
//}
};

#endif
