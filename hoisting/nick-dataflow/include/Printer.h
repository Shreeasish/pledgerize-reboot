#ifndef PRINTER_H
#define PRINTER_H

#include "ConditionList.h"
#include "Generator.h"

#include <unordered_map>
#include <string>


class Printer {
public:
  Printer(Generator* g, llvm::raw_ostream& out) : generator{g}, out{out} {
    initializeMap();
  }

  void  // Point of configuration for async queue
  printState(llvm::Instruction* const location,
             const Disjunction& disjunction) {
    if (disjunction.isEmpty()) {
      return;
    }
    printStateTrees(location, disjunction);
  }

  void  // Point of configuration for async queue
  printIR(llvm::Instruction* const location, const Disjunction& disjunction) {
    if (disjunction.isEmpty()) {
      return;
    }
    printBasicIR(location, disjunction);
  }

private:
  int counter = 0;
  Generator* generator;
  llvm::raw_ostream& out;
  const char* delimiter = "\\x";

  llvm::DenseMap<OpKey, const char*> opMap;

  // Visitor Helpers
  template <class... Ts>
  struct overloaded : Ts... {
    using Ts::operator()...;
  };
  template <class... Ts>
  overloaded(Ts...)->overloaded<Ts...>;
  //


  std::string
  getDebugData(llvm::Instruction* const location) const {
    return location->getName();
  }

  void
  printLineNumber(llvm::Instruction* const inst) const {
    //out.changeColor(llvm::raw_ostream::Colors::RED);
    if (const llvm::DILocation* debugLoc = inst->getDebugLoc()) {
      out << "At " << debugLoc->getFilename() << " line " << debugLoc->getLine()
          << "  " << *inst << "\n";
    } else {
      out << "At an unknown location" << *inst << "\n";
    }
    //out.changeColor(llvm::raw_ostream::Colors::WHITE);
  }

  void
  printTree(const Conjunct conjunct,
            const int disjunctCount,
            const int conjunctCount) const {
    auto visitor = overloaded {
      [&, this](ExprID exprID, const ConstantExprNode node) {
        out << "\t" << exprID << delimiter << disjunctCount << delimiter
            << conjunctCount;
        if (node.constant) {
          out << " [shape=box,label=\"" << *(node.constant) << "\"]\n";
        } else {
          out << " [shape=box,label=\""
              << " nullptr "
              << "\"]\n";
        }
      },
      [&, this](ExprID exprID, const BinaryExprNode node) {
        auto lhsID = node.lhs;
        auto rhsID = node.rhs;
        out << "\t" << exprID << delimiter << disjunctCount << delimiter
            << conjunctCount << " -> { " << lhsID << delimiter
            << disjunctCount << delimiter << conjunctCount << "; " << rhsID
            << delimiter << disjunctCount << delimiter << conjunctCount
            << " }\n";
      },
      [&, this](ExprID exprID, const ValueExprNode node) {
        out << "\t" << exprID << delimiter << disjunctCount << delimiter
            << conjunctCount << " [shape=box,label=\"" << *(node.value)
            << "\"]\n";
      }};
    generator->preOrderFor(conjunct, visitor);
  }

  void
  printStateTrees(llvm::Instruction* const location,
                  const Disjunction disjunction) {
    int disjunctCount = 0;
    out << "\nDigraph " << counter++ << " {\n"
        << "\tlabel = <";
    printLineNumber(location);
    out << ">\n"
        << "\tlabelloc = \"t\";\n";
    for (auto& disjunct : disjunction) {
      int conjunctCount = 0;
      for (auto& conjunct : disjunct) {
        printTree(conjunct, disjunctCount, conjunctCount);
        conjunctCount++;
      }
      disjunctCount++;
    }
    out << "}\n";
  }

  void
  printTreeIR(const Conjunct conjunct) const {
    auto visitor = overloaded {
      [&, this](ExprID exprID, const ConstantExprNode node) {
        // out.changeColor(llvm::raw_ostream::Colors::GREEN);
        if (node.constant) {
          out << "( " << *(node.constant) << " )";
        } else {
          out << "( " << exprID << " )";
        }
        // out.changeColor(llvm::raw_ostream::Colors::WHITE);
      },
      [&, this](ExprID exprID, const BinaryExprNode node) {
        // out.changeColor(llvm::raw_ostream::Colors::BLUE);
        if (auto it = opMap.find(node.op.opCode); it != opMap.end()) {
          auto [first, opString] = *it;
          out << " " << opString << " " ;
        }
        // out.changeColor(llvm::raw_ostream::Colors::WHITE);
      },
      [&, this](ExprID exprID, const ValueExprNode node) {
        // out.changeColor(llvm::raw_ostream::Colors::GREEN);
        llvm::errs() << "Printing for " << exprID;
        out << "( " << *(node.value) << " )";
        // out.changeColor(llvm::raw_ostream::Colors::WHITE);
      }};
    generator->inOrderFor(conjunct, visitor);
  }

  void
  printBasicIR(llvm::Instruction* const location,
               const Disjunction disjunction) {
    printLineNumber(location);
    bool firstDisjunct = true;
    for (auto& disjunct : disjunction) {
      if (firstDisjunct) {
        out << "{";
        firstDisjunct = false;
      } else {
        out << "OR \n{";
      }
      bool firstConjunct = true;
      for (auto& conjunct : disjunct) {
        if (firstConjunct) {
          if (conjunct.notNegated) {
            out << " [";
          } else {
            out << "-[";
          }
          firstConjunct = false;
        } else {
          if (conjunct.notNegated) {
            out << " AND  [";
          } else {
            out << " AND -[";
          }
        }
        printTreeIR(conjunct);
        out << "]";
      }
      out << "}\n";
    }
    out << "\n";
  }

  void
  initializeMap() {
    #define HANDLE(a,b,c) opMap.try_emplace(a, #b);
    #include "OperatorStrings.def"
  }


  // Digraph 00 {
  //
  //  label = <The <font color='red'><b>foo</b></font>,<br/> the <font
  //  point-size='20'>bar</font> and<br/> the <i>baz</i>>; labelloc = "t"; //
  //  place the label at the top (b seems to be default)
  //
  //  node [shape=plaintext]
  //
  //  FOO -> {BAR, BAZ};
  //}
};

#endif
