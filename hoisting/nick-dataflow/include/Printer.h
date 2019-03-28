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
    printTrees(location, disjunction);
  }

  void
  insertIR(llvm::Instruction* const location, const Disjunction& disjunction) {
    if (disjunction.isEmpty()) {
      return;
    }
    llvm::errs() << "\nInserting at " << *location 
                 << "\n parent function " << location->getParent()->getParent()->getName();
    llvm::outs() << "\nInserting at " << *location 
                 << "\n parent function " << location->getParent()->getParent()->getName();
    llvm::IRBuilder<> builder{location};
    //printFlattened(location, disjunction);
    printState(location, disjunction);
    //insertIR(disjunction, location, builder);
    //auto* bb = location->getParent();
    //llvm::errs() << "After insertion" ;
    //llvm::errs() << *bb;
  }

private:
  int counter = 0;
  Generator* generator;
  llvm::raw_ostream& out;
  llvm::Module& module;
  const char* delimiter = "x";
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
  printFlattened(llvm::Instruction* const location,
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
        printValues(conjunct);
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
  printTrees(llvm::Instruction* const location,
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
        printGraph(conjunct, disjunctCount, conjunctCount);
        conjunctCount++;
      }
      disjunctCount++;
    }
    out << "}\n";
  }

  void
  printGraph(const Conjunct conjunct,
            const int disjunctCount,
            const int conjunctCount) const {
    auto visitor = overloaded{
        [&, this](ExprID exprID, const ConstantExprNode node) {
          out << "\t\"" << exprID << delimiter << disjunctCount << delimiter
              << conjunctCount << "\"";
          if (node.constant) {
            out << " [shape=box,label=\"" << *(node.constant) << "\"";
          } else {
            out << " [shape=box,label=\""
                << " nullptr " << "\"";
          }
          out << ", ID=\"" << exprID << delimiter << disjunctCount << delimiter
              << conjunctCount << "\"]\n";
        },
        [&, this](ExprID exprID, const BinaryExprNode node) {
          auto lhsID = node.lhs;
          auto rhsID = node.rhs;
          out << "\t\"" << exprID << delimiter << disjunctCount << delimiter
              << conjunctCount << "\""
              << " -> { "
              << "\"" << lhsID << delimiter << disjunctCount << delimiter
              << conjunctCount << "\""
              << "; "
              << "\"" << rhsID << delimiter << disjunctCount << delimiter
              << conjunctCount << "\"" << " }\n";
          

          out << "\t\"" << exprID << delimiter << disjunctCount << delimiter 
              << conjunctCount << "\"" << " [shape=box,label=\"";
              if (node.value) {
                out << *(node.value);
              } else {
                out << "nullptr";
              }
          out << "\""
              << ",ID=\"" << exprID << delimiter << disjunctCount << delimiter
              << conjunctCount
              << "\"]\n";
        },
        [&, this](ExprID exprID, const ValueExprNode node) {
          out << "\t\"" << exprID << delimiter << disjunctCount << delimiter 
              << conjunctCount << "\"" << " [shape=box,label=\"";
              if (node.value) {
                out << *(node.value);
              } else {
                out << "nullptr";
              }
          out << "\""
              << ",ID=\"" << exprID << delimiter << disjunctCount << delimiter
              << conjunctCount
              << "\"]\n";
        }};
    generator->preOrderFor(conjunct, visitor);
  }

  void
  printValues(const Conjunct conjunct) const {
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
            out << " (node.value)" << *(node.value) << " ";
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

  void
  insertIR(const Disjunction& disjunction, llvm::Instruction* location, llvm::IRBuilder<>& builder) {
    Visitor visitor{builder};

    auto applyForm = [&](const Conjunct& conjunct, llvm::Value* value) -> llvm::Value* {
      if (!conjunct.notNegated) {
        return builder.CreateNot(value);
      } 
      return value;
    };

    auto generateDisjunct = [&](const Disjunct& disjunct) -> llvm::Value* {
      std::vector<llvm::Value*> generatedConjuncts;
      std::transform(disjunct.begin(), disjunct.end(), 
                     std::back_inserter(generatedConjuncts),
           [&] (const Conjunct& conjunct) -> llvm::Value* {
             auto [retConjunct, generatedIR] = generator->postOrderFor(conjunct, visitor);
             return applyForm(conjunct, generatedIR);
           });
      return 
        std::accumulate(generatedConjuncts.begin(), generatedConjuncts.end(), 
                       *generatedConjuncts.begin(),
        [&] (auto* lhs, auto* rhs) -> llvm::Value* {
          return builder.CreateAnd(lhs, rhs);
        });
    };

    auto generateDisjunction = [&](const Disjunction& disjunction) {
      std::vector<llvm::Value*> generatedDisjuncts;
      std::transform(disjunction.begin(), disjunction.end(), 
                     std::back_inserter(generatedDisjuncts),
           [&] (const auto& disjunct) {
             return generateDisjunct(disjunct);
           });
      return 
        std::accumulate(generatedDisjuncts.begin(), generatedDisjuncts.end(), 
                        *generatedDisjuncts.begin(),
        [&] (auto* lhs, auto* rhs) -> llvm::Value* {
           return builder.CreateOr(lhs, rhs);
        });
    };
    generateDisjunction(disjunction);
  }


  class Visitor {
  public:
    Visitor(llvm::IRBuilder<>& irb) 
      : builder{irb} { }
    llvm::Value*
    operator()(ExprID exprID,
               BinaryExprNode binaryNode,
               llvm::Value* lhs,
               llvm::Value* rhs) {
      return binaryDispatch(exprID, binaryNode, lhs, rhs);
    }
  
    //TODO: Handle Special cases
    // Do not handle a vacuousExprNodes as lowering is guaranteed
    // at locations where the constraints are not vacuously true
    llvm::Value*
    operator()(ExprID exprID, ConstantExprNode node) {
      return node.constant;
    }

    llvm::Value*
    operator()(ExprID exprID, ValueExprNode node) {
      return node.value;
    }

  private:
    llvm::IRBuilder<>& builder;

    llvm::Value*
    binaryDispatch(ExprID exprID,
                   BinaryExprNode binaryNode,
                   llvm::Value* lhs,
                   llvm::Value* rhs) {
      llvm::errs() << "\n" << exprID << " (binaryNode.value) " << *(binaryNode.value);
      llvm::errs() << "\n" << exprID << " (binaryNode.op.opCode) " << (binaryNode.op.opCode);
      if (lhs && rhs) {
        llvm::errs() << "\n" << " lhs " << *lhs << "\n rhs " << *rhs
                     << "\n";
      }
      switch (binaryNode.op.opCode) {
        case llvm::Instruction::Add:
          return generateAdd(binaryNode, lhs, rhs);
          break;
        case llvm::Instruction::ICmp:
          return generateIcmp(binaryNode, lhs, rhs);
          break;
        case llvm::Instruction::Load:
          return generateLoad(binaryNode, lhs, rhs); 
          break;
        case llvm::Instruction::Switch:
          return generateSwitch(binaryNode, lhs, rhs);
          break;
        case llvm::Instruction::GetElementPtr:
          return generateGEP(binaryNode, lhs, rhs);
          break;
        case llvm::Instruction::Call:
          return generateCall(binaryNode, lhs, rhs);
          break;
        case OpIDs::Cast:
          llvm::errs() << "\nFound castOp";
          return generateCast(binaryNode, lhs, rhs);
          //llvm_unreachable("Found unknown instruction");
          break;
        case OpIDs::Alias:
          return generateAlias(binaryNode, lhs, rhs);
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
      llvm::Value* generated = builder.CreateAdd(lhs, rhs);
      return generated;
    }

    llvm::Value*
    generateIcmp(BinaryExprNode binaryNode,
                 llvm::Value* lhs,
                 llvm::Value* rhs) {
      switch (binaryNode.op.predicate) {
        case Predicate::ICMP_EQ:
          return builder.CreateICmpEQ(lhs, rhs);
          break;
        case Predicate::ICMP_SLT:
          return builder.CreateICmpSLT(lhs, rhs);
          break;
        case Predicate::ICMP_SGT:
          return builder.CreateICmpSGT(lhs, rhs);
          break;
        default:
          llvm::errs() << "\n (binaryNode.op.predicate) "
                       << (binaryNode.op.predicate);
          llvm_unreachable("Found unknown predicate");
          return nullptr;
          break;
      }
      return nullptr;
    }


    llvm::Value*
    generateLoad(BinaryExprNode binaryNode,
                 llvm::Value* lhs,
                 llvm::Value* rhs) {
      return builder.CreateLoad(rhs);
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
      auto* generated = builder.CreateGEP(lhs, rhs, "gep");
      return generated;
    }

    llvm::Value*
    generateCall(BinaryExprNode binaryNode, llvm::Value* lhs, llvm::Value* rhs) {
      if (auto* func = llvm::dyn_cast<llvm::Function>(lhs)) {
        auto* functionType = func->getFunctionType();
        auto* generated    = builder.CreateCall( functionType, func,
                                  llvm::ArrayRef<llvm::Value*>{rhs});
        return generated;
      } else if (auto* callInst = llvm::dyn_cast<llvm::CallInst>(lhs)) {
        auto* func = callInst->getCalledFunction();
        auto* functionType = func->getFunctionType();
        assert(func);
        std::vector<llvm::Value*> operands;
        for (auto& arg : callInst->arg_operands()) {
          operands.push_back(arg);
        }
        if (rhs) { // skip sentinel
          //llvm::errs() << "\n rhs for CallAST " << *rhs << "with exprID " << binaryNode.rhs;
          operands.push_back(rhs);
        }
        auto* generated = builder.CreateCall(functionType, func, llvm::ArrayRef<llvm::Value*>{operands});
        return generated;
      } else {
        llvm::errs() << "\nlhs is unknown " << *lhs;
        llvm_unreachable("lhs unknown");
        return nullptr;
      }
    }

    llvm::Value*
    generateAlias(BinaryExprNode binaryNode, llvm::Value* lhs, llvm::Value* rhs) {
      return builder.CreateICmpEQ(lhs, rhs, "aliasEQ");
    }

    llvm::Value*
    generateCast(BinaryExprNode binaryNode, llvm::Value* lhs, llvm::Value* rhs) {
      auto* castInst = llvm::dyn_cast<llvm::CastInst>(binaryNode.value);
      auto castType  = castInst->getOpcode();
      return builder.CreateCast(castType, lhs, castInst->getDestTy());
    }
  };

  void
  printLineNumber(llvm::Instruction* const inst) const {
    // out.changeColor(llvm::raw_ostream::Colors::RED);
    if (const llvm::DILocation* debugLoc = inst->getDebugLoc()) {
      out << "At " << debugLoc->getFilename() << " line " << debugLoc->getLine()
          << "  " << *inst;
    } else {
      out << "At an unknown location" << *inst;
    }
    // out.changeColor(llvm::raw_ostream::Colors::WHITE);
  }

  void
  initializeMap() {
    #define HANDLE(a, b, c) opMap.try_emplace(a, #b);
    #include "OperatorStrings.def"
    #undef HANDLE
  }
};
#endif
