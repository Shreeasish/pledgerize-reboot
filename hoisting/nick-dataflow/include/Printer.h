#ifndef PRINTER_H
#define PRINTER_H

#include "ConditionList.h"
#include "Generator.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

#include <unordered_map>
#include <string>
#include <memory>

namespace lowering {
  class Printer;
}

class lowering::Printer {
public:
  Printer(Generator* g, llvm::raw_ostream& out, llvm::Module& m)
    : generator{g}, out{out}, module{m} {
    initializeMap();
  }

  void
  printState(llvm::Instruction* const location,
             const Disjunction& disjunction) {
    if (disjunction.isEmpty()) {
      return;
    }
    printStateTrees(location, disjunction);
  }

  void
  printIR(llvm::Instruction* const location, const Disjunction& disjunction) {
    if (disjunction.isEmpty()) {
      return;
    }
    generateIR(disjunction);
    printBasicIR(location, disjunction);
    // printCall(location, disjunction);
  }

private:
  int counter = 0;
  Generator* generator;
  llvm::raw_ostream& out;
  llvm::Module& module;
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
    // out.changeColor(llvm::raw_ostream::Colors::RED);
    if (const llvm::DILocation* debugLoc = inst->getDebugLoc()) {
      out << "At " << debugLoc->getFilename() << " line " << debugLoc->getLine()
          << "  " << *inst << "\n";
    } else {
      out << "At an unknown location" << *inst << "\n";
    }
    // out.changeColor(llvm::raw_ostream::Colors::WHITE);
  }

  void
  printTree(const Conjunct conjunct,
            const int disjunctCount,
            const int conjunctCount) const {
    auto visitor = overloaded{
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
    auto visitor = overloaded{
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
            out << " " << opString << " ";
          }
          // out.changeColor(llvm::raw_ostream::Colors::WHITE);
        },
        [&, this](ExprID exprID, const ValueExprNode node) {
          // out.changeColor(llvm::raw_ostream::Colors::GREEN);
          out << "( " << *(node.value) << " )";
          // out.changeColor(llvm::raw_ostream::Colors::WHITE);
        }};
    generator->inOrderFor(conjunct, visitor);
  }

  // TODO: Amortize IRBuilder acquisition
  class Visitor {
  public:
    Visitor(llvm::LLVMContext& c) : context{c} {}
    llvm::Value*
    operator()(ExprID exprID,
               BinaryExprNode binaryNode,
               llvm::Value* lhs,
               llvm::Value* rhs) {
      return binaryDispatch(exprID, binaryNode, lhs, rhs);
    }

    llvm::Value*
    operator()(ExprID exprID, ConstantExprNode node) {
      if (node.constant) {
        llvm::errs() << "\nReturning constant node for " << *(node.constant);
      } else {
        llvm::errs() << "\nReturning constant node for " << exprID;
      }
      return node.constant;
    }

    llvm::Value*
    operator()(ExprID exprID, ValueExprNode node) {
      if (node.value) {
        llvm::errs() << "\nReturning constant node for " << *(node.value);
      } else {
        llvm::errs() << "\nReturning constant node for " << exprID;
      }
      return node.value;
    }

  private:
    llvm::LLVMContext& context;

    llvm::Value*
    binaryDispatch(ExprID exprID,
                   BinaryExprNode binaryNode,
                   llvm::Value* lhs,
                   llvm::Value* rhs) {
      llvm::errs() << "\n (binaryNode.op.opCode) " << (binaryNode.op.opCode);
      switch (binaryNode.op.opCode) {
        case llvm::Instruction::Add:
          return generateAdd(binaryNode, lhs, rhs);
          break;
        case llvm::Instruction::ICmp:
          return generateIcmp(binaryNode, lhs, rhs);
          break;
        case OpIDs::loadOp: 
          return generateLoad(binaryNode, lhs, rhs); break;
        case OpIDs::switchOp:
          return generateSwitch(binaryNode, lhs, rhs);
          break;
        case llvm::Instruction::GetElementPtr:
          return generateGEP(binaryNode, lhs, rhs);
          break;
        default:
          llvm::errs() << "\n (binaryNode.op.opCode) "
                       << (binaryNode.op.opCode);
          llvm_unreachable("Found unknown instruction");
          return nullptr;
          break;
      }
    }

    llvm::Value*
    generateAdd(BinaryExprNode binaryNode, llvm::Value* lhs, llvm::Value* rhs) {
      llvm::IRBuilder builder(context);
      llvm::Value* generated = builder.CreateAdd(lhs, rhs, "add");
      llvm::errs() << "\nGenerated add instruction" << *generated;
      return generated;
    }

    llvm::Value*
    generateIcmp(BinaryExprNode binaryNode,
                 llvm::Value* lhs,
                 llvm::Value* rhs) {
      llvm::IRBuilder builder(context);
      llvm::Value* generated = nullptr;
      switch (binaryNode.op.predicate) {
        case Predicate::ICMP_EQ:
          generated = builder.CreateICmpEQ(lhs, rhs, "Icmp_EQ");
          llvm::errs() << "\nGenerated ICmp_EQ " << *generated;
          break;
        default:
          llvm::errs() << "\n (binaryNode.op.predicate) "
                       << (binaryNode.op.predicate);
          llvm_unreachable("Found unknown predicate");
          break;
      }
      return generated;
    }


    llvm::Value*
    generateLoad(BinaryExprNode binaryNode,
                 llvm::Value* lhs,
                 llvm::Value* rhs) {
      llvm::IRBuilder builder(context);
      llvm::Value* generated = builder.CreateLoad(rhs, "load");
      llvm::errs() << "\nGenerated add instruction" << *generated;
      return generated;
    }

    llvm::Value*
    generateSwitch(BinaryExprNode binaryNode,
                   llvm::Value* lhs,
                   llvm::Value* rhs) {
      return generateIcmp(binaryNode, lhs, rhs);
    }

    llvm::Value*
    generateGEP(BinaryExprNode binaryNode, llvm::Value* lhs, llvm::Value* rhs) {
      //TODO: Verify with Nick. Run on custom testcase
      llvm::IRBuilder builder(context);
      auto generated = builder.CreateGEP(lhs, rhs, "created gep");
      llvm::errs() << "\n Generated GEP" << *generated;
      return generated;
    }
  };

  // TODO: Handle negations
  // TODO: perform conjunctions
  // TODO: perform disjunctions
  void
  generateIR(const Disjunction disjunction) {
    auto& context = module.getContext();
    Visitor visitor{context};
    for (auto& disjunct : disjunction) {
      for (auto& conjunct : disjunct) {
        auto something = generator->postOrderFor(conjunct, visitor);
      }
    }
    // generator->postOrderFor(
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
  printCall(llvm::Instruction* const location, const Disjunction disjunction) {
    auto& context  = module.getContext();
    auto* voidTy   = llvm::Type::getVoidTy(context);
    auto* dropFunc = module.getOrInsertFunction("PlEdGeRiZe_drop", voidTy);

    llvm::IRBuilder<> builder(location);
    builder.CreateCall(dropFunc);
  }

  void
  initializeMap() {
    #define HANDLE(a, b, c) opMap.try_emplace(a, #b);
    #include "OperatorStrings.def"
    #undef HANDLE
  }
};
#endif
