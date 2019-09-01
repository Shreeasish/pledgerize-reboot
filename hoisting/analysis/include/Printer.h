#ifndef PRINTER_H
#define PRINTER_H

#include "ConditionList.h"
#include "Generator.h"

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "CustomDataflowAnalysis.h"

#include <unordered_map>
#include <string>
#include <memory>

namespace lowering {
  class Printer;
}

class lowering::Printer {
  using Builder = llvm::IRBuilder<>;
public:
  Printer(Generator* const g, 
          llvm::raw_ostream& out) : generator{g}, out{out} {
    initializeMap();
    setLoweringBoundaries();
  }

  Printer(Generator* const g, 
          llvm::raw_ostream& out,
          llvm::Module* m) : generator{g}, out{out}, module{m} {
    initializeMap();
    setLoweringBoundaries();
  }

  void
  printState(llvm::Instruction* const location,
             const Disjunction& disjunction) {
    if (disjunction.isEmpty()) {
      return;
    }
    printTrees(location, disjunction);
    return;
  }

  void
  printFlattened(ExprID exprID, llvm::raw_ostream& ostream) {
    ostream << asFlatAST(exprID);
  }

  auto*
  insertStringResetter(llvm::Instruction* location) {
    auto& context    = module->getContext();
    auto* voidTy     = llvm::Type::getVoidTy(context);
    auto* functionTy = llvm::FunctionType::get(voidTy, false);

    auto* clearString = 
      module->getOrInsertFunction("ClEaR_StRiNg", functionTy);
    llvm::IRBuilder<> builder{location};
    return builder.CreateCall(clearString);
  }

  auto*
  insertStringBuilder(llvm::Instruction* const location,
           const Disjunction& disjunction, Promises promise) {
    auto getPrivAdderFunction = [this, promise] {
      auto&  context = module->getContext();
      auto* int1Ty     = llvm::Type::getInt1Ty(context);
      auto* voidTy     = llvm::Type::getVoidTy(context);
      auto* functionTy = llvm::FunctionType::get(voidTy, {int1Ty}, false);

      auto  pledge_name = PromiseNames[promise];
      auto  functionName = "AdD_" + pledge_name;
      auto  asString    = functionName.str();
      return module->getOrInsertFunction(asString, functionTy);
    };
    //llvm::errs() << "\nInserting at " << *location << "\n parent function "
    //             << location->getParent()->getParent()->getName();
    //llvm::outs() << "\nInserting at " << *location << "\n parent function "
    //             << location->getParent()->getParent()->getName();
    // printState(location, disjunction);
    this->location = location;
    llvm::IRBuilder<> builder{location};
    auto* predicate = foldDisjunction(disjunction, builder);
    auto* privAdderFn = getPrivAdderFunction();
    return builder.CreateCall(/*functiontype,*/ privAdderFn, predicate);
    //auto* bb = location->getParent();
    //llvm::errs() << "\nAfter insertion";
    //llvm::errs() << *bb;
  }

  auto*
  insertPledgeCall(llvm::Instruction* const location) {
    auto getPledgeCallFunction = [this] {
      auto& context = module->getContext();
      auto* voidTy     = llvm::Type::getVoidTy(context);
      auto* functionTy = llvm::FunctionType::get(voidTy, false);

      auto functionName = "MaKe_pLeDgE";
      return module->getOrInsertFunction(functionName, functionTy);
    };

    auto pledgeCaller = getPledgeCallFunction();
    llvm::IRBuilder<> builder{location};
    return builder.CreateCall(pledgeCaller);
  }

  bool isBlackListed(llvm::Instruction* inst) {
    auto fname = inst->getFunction()->getName();
    return functionBlackList.count(fname) > 0;
  }
private:
  Generator* const generator;
  llvm::raw_ostream& out;

  llvm::Instruction* location = nullptr;
  llvm::Module* module        = nullptr;
  //llvm::Context* context      = nullptr;

  llvm::DenseSet<llvm::StringRef> functionBlackList;
  void setLoweringBoundaries();

  llvm::DenseMap<Conjunct, llvm::Value*>    conjunctCache;
  llvm::DenseMap<Disjunct, llvm::Value*>    disjunctCache;
  llvm::DenseMap<Disjunction, llvm::Value*> disjunctionCache;

  llvm::Value*
  generateConjunct(const Conjunct& conjunct, Builder& builder) {
    auto applyForm = [&builder](const Conjunct& conjunct,
                         llvm::Value* value) -> llvm::Value* {
      if (!conjunct.notNegated) {
        return builder.CreateNot(value);
      }
      return value;
    };

    auto& cache = this->conjunctCache;
    if (cache.find(conjunct) != cache.end()) {
      return cache[conjunct];
    }
  
    InstVisitor visitor{builder, this->location};
    auto [retConjunct, generatedIR] =
        generator->postOrderFor(conjunct, visitor);
    auto* generatedValue = applyForm(conjunct, generatedIR);
    cache[conjunct]      = generatedValue;
    return generatedValue;
  }

  llvm::Value*
  generateDisjunct(const Disjunct& disjunct, Builder& builder) {
    auto& cache = this->disjunctCache;
    if (cache.find(disjunct) != cache.end()) {
      return cache[disjunct];
    }

    std::vector<llvm::Value*> generatedConjuncts;
    std::transform(disjunct.begin(), disjunct.end(),
                   std::back_inserter(generatedConjuncts),
                   [&builder, this](const Conjunct& conjunct) -> llvm::Value* {
                      return generateConjunct(conjunct, builder);
                   });

    auto* asValue =
      std::accumulate(generatedConjuncts.begin(),
          generatedConjuncts.end(), 
          *generatedConjuncts.begin(), 
          [&builder](auto* lhs, auto* rhs) -> llvm::Value* { 
            return lhs == rhs ? lhs : builder.CreateAnd(lhs, rhs);
          });
    cache[disjunct] = asValue;
    return asValue;
  }

  llvm::Value*
  generateDisjunction(const Disjunction& disjunction, Builder& builder) {
    auto& cache = this->disjunctionCache;
    if (cache.find(disjunction) != cache.end()) {
      return cache[disjunction];
    }

    std::vector<llvm::Value*> generatedDisjuncts;
    std::transform(disjunction.begin(),
                   disjunction.end(),
                   std::back_inserter(generatedDisjuncts),
                   [&builder, this](const auto& disjunct) {
                     return generateDisjunct(disjunct, builder);
                   });

    auto* asValue =
        std::accumulate(generatedDisjuncts.begin(),
                        generatedDisjuncts.end(),
                        *generatedDisjuncts.begin(),
                        [&builder](auto* lhs, auto* rhs) -> llvm::Value* {
                          return lhs == rhs ? lhs : builder.CreateOr(lhs, rhs);
                        });
    cache[disjunction] = asValue;
    return asValue;
  }

  llvm::Value*
  foldDisjunction(const Disjunction& disjunction,
             llvm::IRBuilder<>& builder) {
    return generateDisjunction(disjunction, builder);
  }

  class InstVisitor {
  public:
    InstVisitor(llvm::IRBuilder<>& irb, llvm::Instruction* const loc)
      : builder{irb}, location{loc} {}
    llvm::Value*
    operator()(ExprID exprID,
               BinaryExprNode binaryNode,
               llvm::Value* lhs,
               llvm::Value* rhs) {
      if (makeTrue) {
        auto& context = location->getContext();
        auto* boolTy = llvm::Type::getInt1Ty(context);
        return llvm::ConstantInt::getTrue(boolTy);
      } else {
        return binaryDispatch(exprID, binaryNode, lhs, rhs);
      }
    }

    llvm::Value*
    operator()(ExprID exprID, ConstantExprNode node) {
      if (exprID == 0) {
        auto& context = location->getContext();
        auto* boolTy = llvm::Type::getInt1Ty(context);
        return llvm::ConstantInt::getTrue(boolTy);
      }
      llvm::errs() << "\n ExprID " << exprID;
      if (node.constant) {
        llvm::errs() << "\n Direct Return Constant" << *node.constant;
      }
      return node.constant;
    }

    llvm::Value*
    operator()(ExprID exprID, ValueExprNode node) {
      if (!isInScope(node.value)) {
        llvm::errs() << "\nIs not in scope" << *node.value ;
      }
      makeTrue |= !(isInScope(node.value));
      return node.value;
    }

  private:
    llvm::IRBuilder<>& builder;
    llvm::Instruction* const location;
    bool makeTrue = false;

    bool
    isInScope(llvm::Value* const value) {
      auto* asInst = llvm::dyn_cast<llvm::Instruction>(value);
      if (!asInst) {
        llvm::errs() << "\n Used Value is not an instruction" << *value;
        return value;
      }
      auto* valueFunction    = asInst->getParent()->getParent();
      auto* locationFunction = location->getParent()->getParent();
      return valueFunction == locationFunction;
    }


    llvm::Value*
    binaryDispatch(ExprID exprID,
                   BinaryExprNode binaryNode,
                   llvm::Value* lhs,
                   llvm::Value* rhs) {
      llvm::errs() << "\n"
                   << exprID << " (binaryNode.value) " << *(binaryNode.value);
      llvm::errs() << "\n"
                   << exprID << " (binaryNode.op.opCode) "
                   << (binaryNode.op.opCode);
      if (lhs && rhs) {
        llvm::errs() << "\n"
                     << " lhs " << *lhs << "\n rhs " << *rhs << "\n";
        auto lhsInst = llvm::dyn_cast<llvm::Instruction>(lhs);
        auto rhsInst = llvm::dyn_cast<llvm::Instruction>(rhs);
        //if (lhsInst && rhsInst
        //    && lhsInst->getParent()->getParent()
        //           != rhsInst->getParent()->getParent()) {
        //  llvm::errs() << "\n Different Parents";
        //} else {
        //  llvm::errs() << " \n Same parent or not inst";
        //}
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
        case llvm::Instruction::Or:
          return generateOr(binaryNode, lhs, rhs);
          break;
        case llvm::Instruction::And:
          return generateAnd(binaryNode, lhs, rhs);
          break;
        case OpIDs::Cast:
          llvm::errs() << "\nFound castOp";
          return generateCast(binaryNode, lhs, rhs);
          break;
        case OpIDs::Alias: 
          return generateAlias(binaryNode, lhs, rhs); 
          break;
        case llvm::Instruction::Sub: 
          return generateSub(binaryNode, lhs, rhs);
          break;
        case llvm::Instruction::UDiv:
          return generateUDiv(binaryNode, lhs, rhs);
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
    generateUDiv(BinaryExprNode binaryNode, llvm::Value* lhs, llvm::Value* rhs) {
      llvm::Value* generated = builder.CreateUDiv(lhs, rhs);
      return generated;
    }

    llvm::Value*
    generateAnd(BinaryExprNode binaryNode, llvm::Value* lhs, llvm::Value* rhs) {
      llvm::Value* generated = builder.CreateAnd(lhs, rhs);
      return generated;
    }

    llvm::Value*
    generateOr(BinaryExprNode binaryNode, llvm::Value* lhs, llvm::Value* rhs) {
      llvm::Value* generated = builder.CreateOr(lhs, rhs);
      return generated;
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
        case Predicate::ICMP_EQ:  return builder.CreateICmpEQ(lhs, rhs); break;
        case Predicate::ICMP_SLT: return builder.CreateICmpSLT(lhs, rhs); break;
        case Predicate::ICMP_SGT: return builder.CreateICmpSGT(lhs, rhs); break;
        case Predicate::ICMP_NE:  return builder.CreateICmpNE(lhs, rhs); break;
        case Predicate::ICMP_UGT: return builder.CreateICmpUGT(lhs, rhs); break;
        case Predicate::ICMP_ULT: return builder.CreateICmpULT(lhs, rhs); break;
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
      auto* withType = binaryNode.value->getType();
      llvm::errs() << "\nGenerating Load "
                   << *binaryNode.value
                   << " with type" << *withType
                   << "\nPointer Operand" << *rhs;
      return builder.CreateLoad(withType, rhs);
    }

    llvm::Value*
    generateSwitch(BinaryExprNode binaryNode,
                   llvm::Value* lhs,
                   llvm::Value* rhs) {
      return generateIcmp(binaryNode, lhs, rhs);
    }

    llvm::Value*
    generateGEP(BinaryExprNode binaryNode, llvm::Value* lhs, llvm::Value* rhs) {
      // TODO: Verify with Nick. Run on custom testcase
      auto* generated = builder.CreateGEP(lhs, rhs, "gep");
      return generated;
    }

    llvm::Value*
    generateCall(BinaryExprNode binaryNode,
                 llvm::Value* lhs,
                 llvm::Value* rhs) {
      llvm::ArrayRef<llvm::Value*> opArray{};
      if (rhs) {
        opArray = llvm::ArrayRef<llvm::Value*>{rhs};
      }

      if (auto* func = llvm::dyn_cast<llvm::Function>(lhs)) {
        auto* functionType = func->getFunctionType();
        auto* generated    = builder.CreateCall(functionType, func, opArray);
        return generated;
      } else if (auto* callInst = llvm::dyn_cast<llvm::CallInst>(lhs)) {
        auto* func         = callInst->getCalledFunction();
        auto* functionType = func->getFunctionType();
        assert(func);
        std::vector<llvm::Value*> operands;
        for (auto& arg : callInst->arg_operands()) {
          operands.push_back(arg);
        }
        if (rhs) {  // skip sentinel
          // llvm::errs() << "\n rhs for CallAST " << *rhs << "with exprID " <<
          // binaryNode.rhs;
          operands.push_back(rhs);
        }
        auto* generated = builder.CreateCall(
            functionType, func, llvm::ArrayRef<llvm::Value*>{operands});

        callInst->eraseFromParent();
        return generated;
      } else {
        llvm::errs() << "\nlhs is unknown " << *lhs;
        llvm_unreachable("lhs unknown");
        return nullptr;
      }
    }

    llvm::Value*
    generateAlias(BinaryExprNode binaryNode,
                  llvm::Value* lhs,
                  llvm::Value* rhs) {
      return builder.CreateICmpEQ(lhs, rhs, "aliasEQ");
    }

    llvm::Value*
    generateCast(BinaryExprNode binaryNode,
                 llvm::Value* lhs,
                 llvm::Value* rhs) {
      auto* castInst = llvm::dyn_cast<llvm::CastInst>(binaryNode.value);
      auto castType  = castInst->getOpcode();
      return builder.CreateCast(castType, lhs, castInst->getDestTy());
    }

    llvm::Value*
    generateSub(BinaryExprNode binaryNode, llvm::Value* lhs, llvm::Value* rhs) {
      return builder.CreateSub(lhs, rhs);
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


  llvm::DenseMap<OpKey, const char*> opMap;
  const char* delimiter = "x";
  int counter           = 0;
  // Visitor Helpers
  template <class... Ts>
  struct overloaded : Ts... {
    using Ts::operator()...;
  };
  template <class... Ts>
  overloaded(Ts...)->overloaded<Ts...>;

  std::string
  getDebugData(llvm::Instruction* const location) const {
    return location->getName();
  }

  // void
  // printCall(llvm::Instruction* const location, const Disjunction disjunction)
  // {
  //  auto& context  = module.getContext();
  //  auto* voidTy   = llvm::Type::getVoidTy(context);
  //  auto* dropFunc = module.getOrInsertFunction("PlEdGeRiZe_drop", voidTy);

  //  llvm::IRBuilder<> builder(location);
  //  builder.CreateCall(dropFunc);
  //}

  void
  printTrees(llvm::Instruction* const location, const Disjunction disjunction) {
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
                << " nullptr "
                << "\"";
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
              << conjunctCount << "\""
              << " }\n";


          out << "\t\"" << exprID << delimiter << disjunctCount << delimiter
              << conjunctCount << "\""
              << " [shape=box,label=\"{" << opMap.find(node.op.opCode)->second
              << "} ";
          if (node.value) {
            out << *(node.value);
          } else {
            out << "nullptr";
          }
          out << "\""
              << ",ID=\"" << exprID << delimiter << disjunctCount << delimiter
              << conjunctCount << "\"]\n";
        },
        [&, this](ExprID exprID, const ValueExprNode node) {
          out << "\t\"" << exprID << delimiter << disjunctCount << delimiter
              << conjunctCount << "\""
              << " [shape=box,label=\"";
          if (node.value) {
            out << *(node.value);
          } else {
            out << "nullptr";
          }
          out << "\""
              << ",ID=\"" << exprID << delimiter << disjunctCount << delimiter
              << conjunctCount << "\"]\n";
        }};
    generator->preOrderFor(conjunct, visitor);
  }

  std::string
  asFlatAST(ExprID exprID) const {
    struct PrettyPrinter
      : public llvm::InstVisitor<PrettyPrinter, std::string> {
      std::string
      visitInstruction(llvm::Instruction& I) {
        std::string ret;
        llvm::raw_string_ostream rso(ret);
        rso << I.getOpcodeName();
        return ret;
      }

      std::string
      visitPHINode(llvm::PHINode& I) {
        std::string ret;
        llvm::raw_string_ostream rso(ret);
        rso << I;
        return ret;
      }

      std::string
      visitICmpInst(llvm::ICmpInst& I) {
        // Actual Predicates can be dealt with later
        return {"ICmp"};
      }

      // std::string
      // visitLoadInst(llvm::LoadInst& loadInst) {
      //  std::string ret;
      //  llvm::raw_string_ostream rso(ret);
      //  rso << *(loadInst.getPointerOperand());
      //  return ret;
      //}

      // std::string
      // visitGetElementPtrInst(llvm::GetElementPtrInst& gep) {
      //  std::string ret;
      //  llvm::raw_string_ostream rso(ret);
      //  rso << "GEP " << *(gep.getPointerOperand());
      //  return ret;
      //}
    };


    std::string flatAST;
    llvm::raw_string_ostream rso{flatAST};
    auto printFName = [&rso](llvm::Function* func) {
      rso << "[@" << func->getName() << "]";
    };
    auto visitor = overloaded{
        [&rso, &printFName](ExprID exprID, const ConstantExprNode node) {
          // Constants are small; print as is
          if (node.constant) {
            if (auto func = llvm::dyn_cast<llvm::Function>(node.constant)) {
              printFName(func);
            } else {
              rso << "[" << *(node.constant) << "]";
            }
          } else {
            rso << "[END]";
          }
        },

        [this, &rso](ExprID exprID, const BinaryExprNode node) {
          if (auto it = opMap.find(node.op.opCode); it != opMap.end()) {
            auto [first, opString] = *it;
            rso << "_op_" << opString << "_";
            // rso << " (node.value)" << *(node.value) << " ";
          }
        },

        [&rso](ExprID exprID, const ValueExprNode node) {
          PrettyPrinter printer;
          if (!node.value) {
            llvm::outs().changeColor(llvm::raw_ostream::Colors::RED);
            llvm::outs() << "\nFor ExprID:" << exprID << "\nValue is null";
            llvm::outs().resetColor();
            assert(false);
          }
          if (auto* asInst = llvm::dyn_cast<llvm::Instruction>(node.value)) {
            rso << "(__" << printer.visit(*asInst) << ")";
          } else if (node.value) {
            rso << "(__" << *node.value << ")";
            llvm::outs().changeColor(llvm::raw_ostream::Colors::RED);
            llvm::outs() << "\nFor ExprID:" << exprID
                         << "\nValue is not an Instruction: "
                         << "\nNode.Value:" << *node.value;
            llvm::outs().resetColor();
          }
        }};
    generator->inOrderFor(exprID, visitor);
    return flatAST;
  }

  void
  initializeMap() {
#define HANDLE(a, b, c) opMap.try_emplace(a, #b);
#include "OperatorStrings.def"
#undef HANDLE
  }
};

void
lowering::Printer::setLoweringBoundaries() {
  auto& blacklist = functionBlackList;
  blacklist.insert("evbuffer_new");
  blacklist.insert("evbuffer_free");
  blacklist.insert("evbuffer_add_buffer");
  blacklist.insert("evbuffer_add");
  blacklist.insert("evbuffer_drain");
  blacklist.insert("evbuffer_add_vprintf");
  blacklist.insert("evbuffer_expand");
  blacklist.insert("evbuffer_add_printf");
  blacklist.insert("evbuffer_remove");
  blacklist.insert("evbuffer_readline");
  blacklist.insert("evbuffer_readln");
  blacklist.insert("evbuffer_read");
  blacklist.insert("evbuffer_write");
  blacklist.insert("evbuffer_find");
  blacklist.insert("evbuffer_setcb");
  blacklist.insert("bufferevent_read_pressure_cb");
  blacklist.insert("bufferevent_new");
  blacklist.insert("bufferevent_readcb");
  blacklist.insert("bufferevent_writecb");
  blacklist.insert("bufferevent_setcb");
  blacklist.insert("bufferevent_setfd");
  blacklist.insert("bufferevent_priority_set");
  blacklist.insert("bufferevent_free");
  blacklist.insert("bufferevent_write");
  blacklist.insert("bufferevent_write_buffer");
  blacklist.insert("bufferevent_read");
  blacklist.insert("bufferevent_enable");
  blacklist.insert("bufferevent_disable");
  blacklist.insert("bufferevent_settimeout");
  blacklist.insert("bufferevent_setwatermark");
  blacklist.insert("bufferevent_base_set");
  blacklist.insert("event_init");
  blacklist.insert("event_base_new");
  blacklist.insert("event_base_priority_init");
  blacklist.insert("event_base_free");
  blacklist.insert("event_del");
  blacklist.insert("event_queue_remove");
  blacklist.insert("event_reinit");
  blacklist.insert("event_priority_init");
  blacklist.insert("event_dispatch");
  blacklist.insert("event_base_loop");
  blacklist.insert("event_loop");
  blacklist.insert("event_base_dispatch");
  blacklist.insert("event_base_get_method");
  blacklist.insert("event_loopexit");
  blacklist.insert("event_loopexit_cb");
  blacklist.insert("event_once_cb");
  blacklist.insert("event_add");
  blacklist.insert("event_once");
  blacklist.insert("event_base_once");
  blacklist.insert("event_base_loopexit");
  blacklist.insert("event_loopbreak");
  blacklist.insert("event_base_loopbreak");
  blacklist.insert("event_set");
  blacklist.insert("event_base_set");
  blacklist.insert("event_priority_set");
  blacklist.insert("event_pending");
  blacklist.insert("event_active");
  blacklist.insert("event_get_version");
  blacklist.insert("event_get_method");
  blacklist.insert("event_asr_run");
  blacklist.insert("event_asr_dispatch");
  blacklist.insert("event_asr_abort");
  blacklist.insert("evtag_init");
  blacklist.insert("encode_int");
  blacklist.insert("evtag_encode_tag");
  blacklist.insert("evtag_decode_tag");
  blacklist.insert("evtag_marshal");
  blacklist.insert("evtag_marshal_int");
  blacklist.insert("evtag_marshal_string");
  blacklist.insert("evtag_marshal_timeval");
  blacklist.insert("evtag_decode_int");
  blacklist.insert("evtag_peek");
  blacklist.insert("evtag_peek_length");
  blacklist.insert("evtag_payload_length");
  blacklist.insert("evtag_consume");
  blacklist.insert("evtag_unmarshal");
  blacklist.insert("evtag_unmarshal_int");
  blacklist.insert("evtag_unmarshal_fixed");
  blacklist.insert("evtag_unmarshal_string");
  blacklist.insert("evtag_unmarshal_timeval");
  blacklist.insert("evutil_socketpair");
  blacklist.insert("evutil_make_socket_nonblocking");
  blacklist.insert("evutil_strtoll");
  blacklist.insert("evutil_snprintf");
  blacklist.insert("evutil_vsnprintf");
  blacklist.insert("evutil_getenv");
  blacklist.insert("kq_init");
  blacklist.insert("kq_add");
  blacklist.insert("kq_del");
  blacklist.insert("kq_dispatch");
  blacklist.insert("kq_dealloc");
  blacklist.insert("kq_insert");
  blacklist.insert("kq_sighandler");
  blacklist.insert("event_err");
  blacklist.insert("_warn_helper");
  blacklist.insert("event_warn");
  blacklist.insert("event_errx");
  blacklist.insert("event_warnx");
  blacklist.insert("event_msgx");
  blacklist.insert("_event_debugx");
  blacklist.insert("event_set_log_callback");
  blacklist.insert("poll_init");
  blacklist.insert("poll_add");
  blacklist.insert("poll_del");
  blacklist.insert("poll_dispatch");
  blacklist.insert("poll_dealloc");
  blacklist.insert("select_init");
  blacklist.insert("select_add");
  blacklist.insert("select_del");
  blacklist.insert("select_dispatch");
  blacklist.insert("select_dealloc");
  blacklist.insert("select_resize");
  blacklist.insert("evsignal_init");
  blacklist.insert("evsignal_cb");
  blacklist.insert("_evsignal_set_handler");
  blacklist.insert("evsignal_add");
  blacklist.insert("evsignal_handler");
  blacklist.insert("_evsignal_restore_handler");
  blacklist.insert("evsignal_del");
  blacklist.insert("evsignal_process");
  blacklist.insert("evsignal_dealloc");
  blacklist.insert("bcrypt_pbkdf");
  blacklist.insert("bcrypt_hash");
  blacklist.insert("login_check_expire");
  blacklist.insert("isduid");
  blacklist.insert("scan_scaled");
  blacklist.insert("fmt_scaled");
  blacklist.insert("fparseln");
  blacklist.insert("getmaxpartitions");
  blacklist.insert("getrawpartition");
  blacklist.insert("ibuf_open");
  blacklist.insert("ibuf_dynamic");
  blacklist.insert("ibuf_add");
  blacklist.insert("ibuf_reserve");
  blacklist.insert("ibuf_seek");
  blacklist.insert("ibuf_size");
  blacklist.insert("ibuf_left");
  blacklist.insert("ibuf_close");
  blacklist.insert("ibuf_write");
  blacklist.insert("msgbuf_drain");
  blacklist.insert("ibuf_free");
  blacklist.insert("msgbuf_init");
  blacklist.insert("msgbuf_clear");
  blacklist.insert("msgbuf_write");
  blacklist.insert("imsg_init");
  blacklist.insert("imsg_read");
  blacklist.insert("imsg_get");
  blacklist.insert("imsg_compose");
  blacklist.insert("imsg_create");
  blacklist.insert("imsg_add");
  blacklist.insert("imsg_close");
  blacklist.insert("imsg_composev");
  blacklist.insert("imsg_free");
  blacklist.insert("imsg_flush");
  blacklist.insert("imsg_clear");
  blacklist.insert("login_fbtab");
  blacklist.insert("login_protect");
  blacklist.insert("login");
  blacklist.insert("login_tty");
  blacklist.insert("logout");
  blacklist.insert("logwtmp");
  blacklist.insert("ohash_create_entry");
  blacklist.insert("ohash_delete");
  blacklist.insert("ohash_remove");
  blacklist.insert("ohash_resize");
  blacklist.insert("ohash_find");
  blacklist.insert("ohash_insert");
  blacklist.insert("ohash_entries");
  blacklist.insert("ohash_first");
  blacklist.insert("ohash_next");
  blacklist.insert("ohash_init");
  blacklist.insert("ohash_interval");
  blacklist.insert("ohash_lookup_interval");
  blacklist.insert("ohash_lookup_memory");
  blacklist.insert("ohash_qlookup");
  blacklist.insert("ohash_qlookupi");
  blacklist.insert("opendev");
  blacklist.insert("opendisk");
  blacklist.insert("pw_file");
  blacklist.insert("pw_setdir");
  blacklist.insert("pw_lock");
  blacklist.insert("pw_mkdb");
  blacklist.insert("pw_abort");
  blacklist.insert("pw_init");
  blacklist.insert("pw_cont");
  blacklist.insert("pw_edit");
  blacklist.insert("pw_error");
  blacklist.insert("pw_prompt");
  blacklist.insert("pw_copy");
  blacklist.insert("pw_scan");
  blacklist.insert("pw_write_entry");
  blacklist.insert("pidfile");
  blacklist.insert("pidfile_cleanup");
  blacklist.insert("pkcs5_pbkdf2");
  blacklist.insert("hmac_sha1");
  blacklist.insert("getptmfd");
  blacklist.insert("openpty");
  blacklist.insert("fdopenpty");
  blacklist.insert("forkpty");
  blacklist.insert("fdforkpty");
  blacklist.insert("readlabelfs");
  blacklist.insert("uu_lock");
  blacklist.insert("uu_lock_txfr");
  blacklist.insert("uu_unlock");
  blacklist.insert("uu_lockerr");
};

#endif
