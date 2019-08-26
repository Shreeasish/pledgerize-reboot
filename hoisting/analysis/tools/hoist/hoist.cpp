#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SparseBitVector.h"

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/CFLSteensAliasAnalysis.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/ConstantFolding.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolutionAliasAnalysis.h"
#include "llvm/Analysis/ScopedNoAliasAA.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TypeBasedAliasAnalysis.h"


#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "llvm/IRReader/IRReader.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"


#include <bitset>
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <unordered_set>


#include "CustomDataflowAnalysis.h"
#include "TaintAnalysis.h"
#include "Handler.h"
#include "ConditionList.h"
#include "Generator.h"
#include "Printer.h"
#include "IndirectCallResolver.h"
#include "InstructionResolver.h"
#include "LibEventAnalyzer.h"

#include "WPA/Andersen.h"

using namespace llvm;


using std::string;
using std::unique_ptr;

static cl::OptionCategory futureFunctionsCategory{"future functions options"};

static cl::opt<string> inPath{cl::Positional,
                              cl::desc{"<Module to analyze>"},
                              cl::value_desc{"bitcode filename"},
                              cl::init(""),
                              cl::Required,
                              cl::cat{futureFunctionsCategory}};


std::unique_ptr<Generator> generator;
std::unique_ptr<IndirectCallResolver> resolver;
//std::unique_ptr<lowering::Printer> printer;
std::unique_ptr<PrivilegeResolver> privilegeResolver;


analysis::SVFAnalysis* svfResults;

llvm::DenseMap<const llvm::Function*,
               std::unique_ptr<llvm::MemorySSA>> functionMemSSAs;
llvm::DenseMap<const llvm::Function*,
               std::unique_ptr<llvm::LoopInfo>> functionLoopInfos;
llvm::DenseMap<const llvm::Function*,
               llvm::AAResultsWrapperPass*> functionAAs;

using Edge  = std::pair<const llvm::BasicBlock*,const llvm::BasicBlock*>;
using Edges = llvm::SmallVector<Edge, 10>;
llvm::DenseMap<const llvm::Function*, Edges> functionBackEdges;

using DisjunctionValue  = std::array<Disjunction, COUNT - 1>;
using DisjunctionState  = analysis::AbstractState<DisjunctionValue>;
using DisjunctionResult = analysis::DataflowResult<DisjunctionValue>;

constexpr int MaxPrivilege{4};
constexpr int MinPrivilege{0};

void
makeVacuouslyTrue(Disjunction& disjunction) {
  Disjunct disjunct{};
  disjunct.addConjunct(generator->GetVacuousConjunct());
  disjunction.disjuncts = Disjuncts{disjunct};
}

/*--------------------------------------------------------*
 *-------------------REWRITING POLICY---------------------*
 *--------------------------------------------------------*/
struct Topological : public Generator::DefaultPolicy {
public:
  std::optional<Disjunction>  // Return if changed
  operator()(const Disjunction& disjunction,
             const ExprID oldExprID,
             const ExprID newExprID) override {
    llvm::errs() << "\nBefore Topology Check";
    disjunction.print(llvm::errs());

    auto oldNode  = generator->GetExprNode(oldExprID);
    auto* oldProv = std::visit(Provenance{oldExprID}, oldNode);

    auto newNode  = generator->GetExprNode(newExprID);
    auto* newProv = std::visit(Provenance{newExprID}, newNode);

    auto isOrdered = [&oldProv, &newProv] {
      if ( !(oldProv && newProv) ||  // either = constant, different parents
          oldProv->getFunction() != newProv->getFunction()) {
        return true;
      }

      auto* currFunction = oldProv->getFunction();
      auto& fDTrees      = generator->functionDTs;
      auto& oInstMap     = generator->orderedInstMap;
      if (!oInstMap.count(currFunction)) {
        auto dt     = std::make_unique<llvm::DominatorTree>(*currFunction);
        auto oInsts = std::make_unique<llvm::OrderedInstructions>(dt.get());

        fDTrees.try_emplace(currFunction, std::move(dt));
        oInstMap.try_emplace(currFunction, std::move(oInsts));
      }
      auto& orderedInsts = *oInstMap[currFunction];
      // llvm::errs() << "\noldProv = " << oldProv->getFunction()->getName()
      //              << "\nnewProv = " << newProv->getFunction()->getName();
      llvm::errs() << "\nCheckin newProv = " << *newProv << "\ndominates oldProv = " << *oldProv;
      return orderedInsts.dominates(newProv, oldProv);
      // true if oldProv before newProv
    }();

    if (isOrdered) {
      return std::nullopt;
    } else {
      llvm::errs() << "\nKilling OldExprID: " << oldExprID;
      llvm::errs() << "\nFor New ExprID" << newExprID;
      llvm::errs() << "\nWith Pushed To True";
      return generator->pushToTrue(disjunction, oldExprID);//.print(llvm::errs());
      //return std::nullopt;
    }
  }

private:
  struct Provenance {
    Provenance(ExprID exprID)
      : exprID{exprID} {}
    ExprID exprID;
      
    llvm::Instruction*
    operator()(const ConstantExprNode& node) {
      return nullptr;  // Permit Constant Rewrites
    }

    template <typename ValueOrBinary>
    llvm::Instruction*
    operator()(const ValueOrBinary& node) {
      llvm::errs() << "\nGetting Origin for " << exprID;
      llvm::errs() << "\nFor Node Inst" << *node.value;
      return generator->getOrigin(this->exprID);
    }
  };
};


/*--------------------------------------------------------*
 *----------------------BACKWARD MEET---------------------*
 * -------------------------------------------------------*/
class DisjunctionMeet : public analysis::Meet<DisjunctionValue, DisjunctionMeet> {
public:
  DisjunctionValue
  meetPair(DisjunctionValue& s1, DisjunctionValue& s2) const {
    llvm::errs() << "\nPerforming Meet";
    DisjunctionValue result;
    static_for<MaxPrivilege>(s1, s2, result);
    return result;
  }

  // Using a concrete type here would need me to also explicitly show the state
  // of states infrastructure from the dataflow lib
  template <typename Predecessors>
  DisjunctionValue
  getIntersection(Predecessors& predecessors) {
    DisjunctionValue intersection;
    for (int privilege = MaxPrivilege; privilege >= MinPrivilege; privilege--) {
      intersection[privilege] = intersectPerPrivilege(predecessors, privilege);
      //llvm::errs() << "\nIntersection State Final";
      //intersection[privilege].print(llvm::errs());
    }
    return intersection;
  }

  template <typename Predecessors>
  Disjunction
  intersectPerPrivilege(Predecessors& predecessors, int privilege) {
    using DisjunctionSet = std::unordered_set<Disjunct, DisjunctHash>;
    //llvm::errs() << "\nBefore Optimizations";
    //llvm::errs() << "\npredecessors.size()" << predecessors.size();
    //for (auto [key, pred] : predecessors) {
    //  llvm::errs() << "\npredecessor";
    //  if (key) {
    //    llvm::errs() << "\nFor key " << *key;
    //  } else {
    //    llvm::errs() << "\nFor key nullptr";
    //  }
    //  pred[nullptr][privilege].print(llvm::errs()); // use here
    //}
    
    auto skip = [](const auto& disjunction) {
      return !disjunction.isReachable && disjunction.isEmpty();
    }; // skip if: NOT reachable AND is empty

    auto intersect = 
    [&privilege, &skip] (Disjunction prev, auto& incoming) {
      auto& [key, value] = incoming;                   // make ref for ease
      auto& state        = value[nullptr][privilege];  // State with nullptr
      if (skip(state)) {
        //llvm::errs() << "\nWould reject incoming";
        return prev;
      }
      auto [larger, smaller] = prev.size() < state.size()
                                   ? std::make_pair(prev, state)
                                   : std::make_pair(state, prev);

      auto larger_set   = DisjunctionSet(larger.begin(), larger.end());
      auto smaller_set  = DisjunctionSet(smaller.begin(), smaller.end());
      auto intersection = Disjunction{true};

      for (auto& disjunct : smaller_set) {
        if (larger_set.count(disjunct)) {
          intersection.addDisjunct(disjunct);
        }
      }
      return intersection;
    };

    Disjunction intersection{}; // initialize this to the smallest in pred set
    int intersectionSize = std::numeric_limits<int>::max();
    for (auto [key, state] : predecessors) {
      auto disjunction = state[nullptr][privilege];
      if (skip(disjunction)) {
        continue;
      }
      intersection = intersectionSize < disjunction.size()
        ? intersection : disjunction;
      intersectionSize = disjunction.size();
    }

    intersection = std::accumulate(predecessors.begin(), predecessors.end(),
                                   intersection, intersect);
    Disjunction ret{};
    for (auto& disjunct : intersection) {  //For each disjunct in intersection
      // Remove intersecting disjuncts from the original predecessors
      for (auto& [key, state] : predecessors) { //For each predecessor
        Disjunction& predDisjunction = state[nullptr][privilege];
        auto from = std::remove_if(predDisjunction.begin(), predDisjunction.end(), 
            [&disjunct](Disjunct& incoming) {
              return incoming == disjunct;
             });
        predDisjunction.disjuncts.erase(from, predDisjunction.end());
      }
      ret.addDisjunct(disjunct);
    }

    //llvm::errs() << "\nAfter Optimizations";
    //llvm::errs() << "\npredecessors.size()" << predecessors.size();
    //for (auto [key, pred] : predecessors) {
    //  llvm::errs() << "\npredecessor";
    //  pred[nullptr][privilege].print(llvm::errs()); // use here
    //}
    return ret;
  }

private:
  Disjunction
  meetPairImpl(Disjunction& s1, Disjunction& s2) const {
    //llvm::errs() << "\nlhs";
    //s1.print(llvm::errs());
    //llvm::errs() << "\nrhs";
    //s2.print(llvm::errs());
    auto result =  Disjunction::unionDisjunctions(s1, s2)
            .simplifyComplements()
            .simplifyRedundancies(generator->GetVacuousConjunct())
            .simplifyImplication()
            .simplifyUnique()
            .simplifyTrues();
    result.isReachable = Disjunction::intersectReachability(s1, s2);
    //llvm::errs() << "\nResult";
    //result.print(llvm::errs());
    return result;
  }

  template <int Counter>
  void
  static_for(DisjunctionValue& s1,
             DisjunctionValue& s2,
             DisjunctionValue& result) const {
    static_for<Counter - 1>(s1, s2, result);
    Promises promise = static_cast<Promises>(Counter);
    result[promise] = meetPairImpl(s1[promise], s2[promise]);
  }

  template <>
  void
  static_for<MinPrivilege>(DisjunctionValue& s1,
                           DisjunctionValue& s2,
                           DisjunctionValue& result) const {
    result[MinPrivilege] = meetPairImpl(s1[MinPrivilege], s2[MinPrivilege]);
    return;
  }
};

/*--------------------------------------------------------*
 *--------------------DEBUGGER CLASSS---------------------*
 * -------------------------------------------------------*/
llvm::DenseMap<llvm::Instruction*, int> SContributionMap; // Global Counters
class Debugger {
public:
  //static constexpr bool SUPPRESSED = true;

  struct ContributionCounter {
    ContributionCounter(llvm::Instruction* i)
      : exprCount{generator->getSize()}, inst{i} {}

    ~ContributionCounter() {
      SContributionMap[inst] += generator->getSize() - exprCount;
    }

  private:
    size_t exprCount;
    llvm::Instruction* inst;
  };

  static bool 
  checkThreshold(DisjunctionValue state, int p, int threshold = 20000) {
    auto check = static_cast<Promises>(p);
    if (check > threshold) {
      
    }
    return state[check].conjunctCount() > threshold;
  }

  Debugger( int p = PLEDGE_CPATH,
           llvm::raw_ostream& ostream = llvm::errs())
    :  promise{static_cast<Promises>(p)}, out{ostream} {};

  // TODO: Set on-off switch in constructor as cl option
  void
  printBefore(DisjunctionValue disjunction, llvm::Instruction* inst) const {
    out << "\n---- Before ---- ";
    out << PromiseNames[promise];
    out << "\n@Instruction" << *inst;
    disjunction[promise].print(llvm::errs());
    return;
  }

  void
  printAfter(DisjunctionValue disjunction, llvm::Instruction* inst, const Context& context) const {
    out << "\n----  After ----";
    out << PromiseNames[promise];
    if (!disjunction[promise].isReachable) {
      out << "--UNREACHABLE";
    }
    out << "\n@Instruction" << *inst << "@parent "
        << inst->getParent()->getParent()->getName();
    out << "\nwith Context";
    for (auto* call : context) {
      if (call) {
        out << "[" << *call << "]";
      } else {
        out << "[ nullptr ]";
      }
    }
    disjunction[promise].print(llvm::errs());
    out << "\n----------------";
  }

  template <typename FunctionResults>
  void
  dump(llvm::Instruction* currInst,
       Context context,
       FunctionResults functionResults,
       int threshold) {
    if (SUPPRESSED) {
      return;
    }

    auto* currBB          = currInst->getParent();
    auto* currFunction    = currBB->getParent();

    llvm::errs() << "\nDumping at " << *currInst << " in function"
                 << currFunction->getName() << " with threshold exceeding " << threshold;

    auto [modulePath, _]  = currFunction->getParent()->getName().split(".");
    auto [__, moduleName] = modulePath.rsplit("/");
    auto baseString       = "/home/shreeasish/pledgerize-reboot/hoisting/logs/"
                      + moduleName.str() + "/" + PromiseNames[promise].str()
                      + "/";
    auto ec = std::error_code{};
    std::string fileName =
        baseString + "/cfg.dump." + currFunction->getName().str() + ".dot";
    //llvm::sys::fs::create_directories(fileName);
    auto dotFile = llvm::raw_fd_ostream{fileName, ec};


    auto printGraph = [&currInst, &currFunction, &baseString](auto* inst, auto& disjunction) {
      if (inst != currInst) {
        return;
      }
      std::string graphFileName =
          baseString + "/ast." + currFunction->getName().str() + ".dot";
      auto ec = std::error_code{};
      // llvm::sys::fs::create_directories(graphFileName);
      auto graphFile = llvm::raw_fd_ostream{graphFileName, ec};
      lowering::Printer printer{generator.get(), graphFile};
      printer.printState(currInst, disjunction);
      graphFile.close();
      //llvm::outs() << "\nPrinted Graph: " << graphFileName;
    };

    auto printBasicBlock = [&](llvm::BasicBlock& bb, auto& functionResults) {
      dotFile << "\n  Node" << &bb << " [shape=record,label=\"<label>\\l";
      for (auto& inst : bb) {
        if (functionResults.find(&inst) != functionResults.end()) {
          auto& state = functionResults[&inst][nullptr][promise];
          auto conjunctCount = state.conjunctCount();
          auto disjunctCount = state.disjunctCount();

          dotFile << inst << " [CC:" << conjunctCount << "][DC:" << disjunctCount << "]"
                  << "[G:" << SContributionMap[&inst] << "]";
          if (!state.isReachable) {
            dotFile << "[UNREACH";
          } else {
            dotFile << "[_______";
          }
          dotFile << "]\\l";
          printGraph(&inst, state);
        } else {
          dotFile << inst << "[NOT REACHED]\\l";
        }
      }
      dotFile << "\"];";

      for (auto* succ : llvm::successors(&bb)) {
        dotFile << "\n  Node" << &bb << " -> Node" << succ << ";";
      }
    };

    dotFile << "digraph \"CFG for function: " << currFunction->getName() << "\"{";
    dotFile << "\nlabel=\"CFG for function: " << currFunction->getName() << "\";";
    for (auto& bb : *currFunction) {
      printBasicBlock(bb, functionResults);
    }
    dotFile << "\n}";
    dotFile.close();

    std::string exprHistFileName =
        baseString + "/exprs.histogram.unique" + std::to_string(threshold) + ".csv";
    //llvm::sys::fs::create_directories(exprHistFileName);
    auto binExprFile = llvm::raw_fd_ostream{exprHistFileName, ec};
    generator->dumpExprData(binExprFile);
    binExprFile.close();
  }

  static void
  exit(llvm::Instruction* inst,
       DisjunctionValue disjunction,
       int promise,
       llvm::raw_ostream& ostream) {
    generator->dumpState(ostream);
    auto* asModule = inst->getParent()->getParent()->getParent();
    generator->dumpToFile<lowering::Printer>(*asModule);
    ostream << "\n----  Exiting At ----";
    ostream << PromiseNames[promise];
    ostream << "\n@Instruction" << *inst << "@parent: "
            << inst->getParent()->getParent()->getName();
    //disjunction[promise].print(ostream);
    //llvm_unreachable("Exiting");
    //assert(false && "Exiting Program");
    std::exit(0);
  }

  static void
  exit(Disjunction& disjunction, llvm::Instruction* inst, llvm::raw_ostream& ostream) {
    generator->dumpState(ostream);
    auto* asModule = inst->getParent()->getParent()->getParent();
    generator->dumpToFile<lowering::Printer>(*asModule);
    ostream << "\n----  Exiting ---- with disjunction";
    ostream << "\n@Instruction" << *inst << "@parent: "
            << inst->getParent()->getParent()->getName();
    disjunction.print(ostream);
    std::exit(0);
  }

  void
  printActivePrivileges(DisjunctionValue disjunction) const {
    if (!disjunction[promise].isEmpty()) {
      out << PromiseNames[promise] << " ";
    }
  }
  
  template <typename State>
  static void
  printCalleeStats(llvm::CallSite& cs, llvm::raw_ostream& outs, State& state) {
    llvm::Function* callee = analysis::getCalledFunction(cs);
    outs << "\nCurrent state: ";
    state[nullptr].print(outs);

    outs << "\nPulling in state from: " << callee->getName();
    outs << "\nState from " << callee->getName() << ":--------------------";
    state[cs.getInstruction()].print(outs);
    outs << "\n------------------------------------------------END";
  }
  
private:
  //llvm::Module* module;
  const Promises promise;
  llvm::raw_ostream& out;
}; // end Class Debugger

// Variadic arguments should be killed by 
// array access simplification at geps
struct ArgumentRewriter {
  Disjunction
  operator()(Disjunction state,
             const llvm::CallSite& cs,
             llvm::Function* const callee) {
    // CallSites have arguments, functions have paramaters
    using argParams = std::pair<llvm::Value*, llvm::Value*>;
    std::vector<argParams> argPairs;
    int i = 0;
    for (auto& param : callee->args()) {
      auto* paramAsValue = (llvm::Value*)&param;
      auto arg           = cs.getArgument(i);
      argPairs.push_back(argParams{arg, paramAsValue});
      i++;
    }

    auto rewritePair = [&](auto* arg, auto* param) {
      auto argExprID   = generator->GetOrCreateExprID(arg);
      auto paramExprID = generator->GetOrCreateExprID(param);
      return generator->rewrite(state, paramExprID, argExprID);
    };
    for (auto [arg, param] : argPairs) {
      state = rewritePair(arg, param);
    }
    return state;
  }
};

struct SemanticSimplifier {
public:
  Disjunction
  simplifyConstants(Disjunction& disjunction, llvm::Value& value) {
    struct Folder {
    /* Expensive but no way around it
     * without constraint metadata */
    public:
      llvm::Value*
      operator()(ExprID id, ConstantExprNode node) {
        if (node.constant && isConstant) {
          constants.push_back(node.constant);
        }         
        return node.constant;
      }

      llvm::Value*
      operator()(ExprID id,
                 BinaryExprNode node,
                 llvm::Value* lhs,
                 llvm::Value* rhs) {
        if (isConstant && llvm::Instruction::isBinaryOp(node.op.opCode)) {
          auto* inst = llvm::dyn_cast<llvm::Instruction>(node.value);
          auto* module = inst->getModule();
          return generateConstant(node.op, module); 
        } 
        else if (isConstant && (llvm::Instruction::ICmp == node.op.opCode 
                             || llvm::Instruction::Switch == node.op.opCode)) {
          auto* inst = llvm::dyn_cast<llvm::Instruction>(node.value);
          auto* module = inst->getModule();
          return generateCompare(node.op, module); 
        } else {
//          llvm::errs() << "\nSkipping opCode name "
//                       << llvm::Instruction::getOpcodeName(node.op.opCode)
//                       << " OpCode value " << node.op.opCode;
          isConstant = false;
          return nullptr;
        }
      }

      llvm::Value*
      operator()(ExprID id, ValueExprNode veNode) {
        auto* asConstant = llvm::dyn_cast<llvm::Constant>(veNode.value);
        if (asConstant) {
          llvm::errs() << "\nValue as Constant" << *asConstant;
          std::exit(0);
        }
        isConstant = false;
        return nullptr;
      }

      private:
      std::deque<Constant*> constants;
      bool isConstant = true;
  
      llvm::Constant*
      generateConstant(ExprOp op, llvm::Module* module) {
        auto* lhs = constants.front();
        constants.pop_front();

        auto* rhs = constants.front();
        constants.pop_front();
        
        auto& layout = module->getDataLayout();
        auto* constant = ConstantFoldBinaryOpOperands(op.opCode, lhs, rhs, layout);
        constants.push_back(constant);
        return constant;
      }

      // Works for switches too, since switches use the same predicate
      llvm::Constant* 
      generateCompare(ExprOp op, llvm::Module* module) {
        auto* lhs = constants.front();
        constants.pop_front();

        auto* rhs = constants.front();
        constants.pop_front();
        
        auto& layout = module->getDataLayout();
        auto* constant = ConstantFoldCompareInstOperands(op.predicate, lhs, rhs, layout);
        constants.push_back(constant);
        return constant;
      }
    };

    auto generateFolded = [](auto& conjunct) -> llvm::Constant* {
      Folder visitor;
      auto [_ , asValue] = generator->postOrderFor(conjunct, visitor);
      return llvm::dyn_cast_or_null<llvm::Constant>(asValue);
    };

    auto isCmpInst = [](auto& conjunct) -> bool {
      auto exprID = conjunct.exprID;
      auto node = generator->GetExprNode(exprID);

      if (auto* binNode = std::get_if<const BinaryExprNode>(&node)) {
        auto opCode = binNode->op.opCode;
        return opCode == llvm::Instruction::ICmp || opCode == llvm::Instruction::Switch;
      }
      return false;
    };

    auto isConstantNode = [](auto& conjunct) -> bool {
      auto exprID = conjunct.exprID;
      auto node   = generator->GetExprNode(exprID);
      if (auto* constantNode = std::get_if<const ConstantExprNode>(&node); 
          exprID > 0 && constantNode) {
        llvm::errs() << "\nTop Level Constant: " << *constantNode->constant;
        return true;
      }
      return false;
    };

    llvm::DenseMap<ExprID, llvm::Constant*> idConstantMap;
    bool foundConstant = false;
    for (auto& disjunct : disjunction) {
      for (auto& conjunct : disjunct) {
        if (!isCmpInst(conjunct) && !isConstantNode(conjunct)) {
          continue;
        }
        if (auto* constant = generateFolded(conjunct)) {
          idConstantMap[conjunct.exprID] = constant;
          foundConstant = true;
        }
      }
    }
  
    if (!foundConstant) {
      return disjunction;
    }

    auto handle = [](auto& conjunct, bool conjunctIsTrue) -> bool {
      if (conjunct.notNegated xor conjunctIsTrue) {
        /* Either the constraint is negated && evaluated to true
         * Or the constraint is true and it evaluates to false 
         * i.e. represents a false constraint, kill it. */
        return true; 
      } else { /* Push constraint to true and move on */
        conjunct = generator->GetVacuousConjunct();
        return false;
      }
    };

    auto checkDisjunct = [&idConstantMap, &handle](auto& disjunct) {
      for (auto& conjunct : disjunct) {
        auto& exprID = conjunct.exprID;
        if (!idConstantMap.count(exprID)) {
           continue;
        }
      
        auto* constant = idConstantMap[exprID];
        llvm::errs() << "\nFound Generated constant for " << exprID
                     << "\nConstant " << *constant;
        bool evaluatesToTrue = constant->isOneValue();
        if (handle(conjunct, evaluatesToTrue)) {
          return true;
        }
      }
      return false;
    };
  
    auto strictLogical = [&disjunction, &checkDisjunct]() {
      llvm::errs() << "\nBefore constant simplification";
      disjunction.print(llvm::errs());
      auto from = std::remove_if(disjunction.begin(), disjunction.end(), checkDisjunct);
      auto& disjuncts = disjunction.disjuncts;
      disjuncts.erase(from, disjuncts.end());
    };
    
    auto trueOnNegation = [&disjunction, &checkDisjunct] {
      bool shouldKill = false;
      for (auto& disjunct : disjunction) {
        shouldKill = checkDisjunct(disjunct);
        if (shouldKill) {
          break;
        }
      }
      if (shouldKill) {
        makeVacuouslyTrue(disjunction);
      }
    };
    
    trueOnNegation();
    return disjunction.simplifyDisjuncts(generator->GetVacuousConjunct());
  }

  Disjunction
  killLoad(llvm::LoadInst* loadInst, Disjunction& disjunction) {
    auto asValue  = llvm::dyn_cast<llvm::Value>(loadInst);
    auto valueExprID = generator->GetOrCreateExprID(asValue);

    auto isAlias = [](const auto exprID) -> std::optional<const BinaryExprNode> {
      auto node = generator->GetExprNode(exprID);
      if (auto* binNode = std::get_if<const BinaryExprNode>(&node)) {
        auto isAliasOpCode = binNode->op.opCode == OpIDs::Alias;
        return 
          isAliasOpCode ? std::optional<BinaryExprNode>{*binNode}
                        : std::nullopt;
      }
      return std::nullopt;
    };

    auto isCallAlias = [](const BinaryExprNode& binNode) -> bool {
      return !(llvm::isa<llvm::LoadInst>(binNode.value));
    };

    auto shouldKill = [&isAlias, &isCallAlias, &valueExprID] (auto& conjunct) {
      if (auto binNode = isAlias(conjunct.exprID); 
          binNode && isCallAlias(*binNode) /*&& !(conjunct.notNegated)*/) { 
        // Semantic mutual exclusion of may-aliasing calls precludes the
        // need for checking the negated form of the constraint i.e. kill em all
        return generator->find(conjunct, valueExprID);
        //return binNode->rhs == valueExprID;
      }
      return false;
    };

    auto checkMayCall = [&shouldKill, disjunction](auto& disjunct) {
      for (auto& conjunct : disjunct) {
        if (shouldKill(conjunct)) {
          conjunct = generator->GetVacuousConjunct();
        }
      }
    };

    for (auto& disjunct : disjunction) {
      checkMayCall(disjunct);
    }
  
    disjunction.simplifyDisjuncts(generator->GetVacuousConjunct());
    return disjunction;
  }

  Disjunction
  killGEP(llvm::GetElementPtrInst* gep, Disjunction& disjunction) {
    auto asValue     = llvm::dyn_cast<llvm::Value>(gep);
    auto valueExprID = generator->GetOrCreateExprID(asValue);

    auto isAlias = [](const auto exprID) -> std::optional<const BinaryExprNode> {
      auto node = generator->GetExprNode(exprID);
      if (auto* binNode = std::get_if<const BinaryExprNode>(&node)) {
        auto isAliasOpCode = binNode->op.opCode == OpIDs::Alias;
        return 
          isAliasOpCode ? std::optional<BinaryExprNode>{*binNode}
                        : std::nullopt;
      }
      return std::nullopt;
    };

    auto isCallAlias = [](const BinaryExprNode& binNode) -> bool {
      return !(llvm::isa<llvm::LoadInst>(binNode.value));
    };

    auto shouldKill = [&isAlias, &isCallAlias, &valueExprID] (auto& conjunct) {
      if (auto binNode = isAlias(conjunct.exprID); 
          binNode && isCallAlias(*binNode) /*&& !(conjunct.notNegated)*/) {
        // Semantic mutual exclusion of may-aliasing calls precludes the
        // need for checking the negated form of the constraint i.e. kill em all
        return generator->find(conjunct, valueExprID);
        //return binNode->rhs == valueExprID;
      }
      return false;
    };

    auto checkMayCall = [&shouldKill, disjunction](auto& disjunct) {
      for (auto& conjunct : disjunct) {
        if (shouldKill(conjunct)) {
          conjunct = generator->GetVacuousConjunct();
        }
      }
    };

    for (auto& disjunct : disjunction) {
      checkMayCall(disjunct);
    }
  
    disjunction.simplifyDisjuncts(generator->GetVacuousConjunct());
    return disjunction;
  }
};


/*--------------------------------------------------------*
 *-------------------EDGE TRANSFORMER---------------------*
 *--------------------------------------------------------*/
class DisjunctionEdgeTransformer {
public:
  llvm::Instruction*
  setLocation(llvm::Value* value) {
    auto* branch = llvm::dyn_cast<llvm::Instruction>(value);
    assert(branch && "Not an instruction");
    generator->setLocation(branch);
    return branch;
  }

  void
  removeLocation(llvm::Instruction* checkLoc) {
    assert(generator->getLocation() == checkLoc && "Location changed");
    generator->setLocation(nullptr);
  }

  Edges
  getBackedges(const llvm::Function* func) {
    return functionBackEdges[func];
  }

  void  // Clean BackEdges
  cleanBackEdge(Disjunction& disjunction,
                llvm::Value* branch,        
                llvm::Value* destination) {
    auto* srcInst = llvm::dyn_cast<llvm::Instruction>(branch);
    auto* srcBB = srcInst->getParent(); // in the forward sense
    auto* function = srcBB->getParent();

    assert(functionLoopInfos.count(function) && "No loop info for function");
    auto* loopInfo = functionLoopInfos[function].get();
    auto* loop     = loopInfo->getLoopFor(srcBB);
    //if (loop && loop->isLoopLatch(srcBB)) {
    //  llvm::errs() << "\nFound Loop from loopInfo";
    //}

    auto isBackEdge = [&srcBB, &loop] {
      return loop && loop->isLoopLatch(srcBB);
    }();

    if (!isBackEdge) {
      //llvm::errs() << "\nNo Loop from backedgecheck";
      return;
    }

    llvm::errs() << "\nBefore Cleaning BackEdges";
    disjunction.print(llvm::errs());
    // 1. get constraints introduced within the loop
    auto& blackList  = loop->getBlocksSet();

    auto isFromLoop  = [&blackList](auto& conjunct) -> bool {
      auto& exprID = conjunct.exprID;
      auto* location = generator->getOrigin(exprID);
      auto* asBB = location->getParent();
      return blackList.count(asBB);
    };

    auto vacuousConjunct = generator->GetVacuousConjunct();
    for (auto& disjunct : disjunction) {
      for (auto& conjunct : disjunct) {
        if (conjunct == vacuousConjunct
            || !isFromLoop(conjunct)) {
          continue;
        }
        conjunct = generator->GetVacuousConjunct();
      }
    }

    disjunction.simplifyDisjuncts(generator->GetVacuousConjunct());
    llvm::errs() << "\nAfter Cleaning BackEdges";
    disjunction.print(llvm::errs());
    return;
  }
  
  DisjunctionValue
  operator()(DisjunctionValue toMerge,
             llvm::Value* branchAsValue,
             llvm::Value* destination) {
    // Store reachability
    // Use the label of the basic block of the branch to replace the appropriate
    // operand of the phi
    auto getAssocValue = [&branchAsValue](llvm::PHINode* const phi) {
      auto* basicBlock =
          llvm::dyn_cast<llvm::Instruction>(branchAsValue)->getParent();
      return phi->getIncomingValueForBlock(basicBlock);
    };
    auto isUsedPhi = [](llvm::PHINode& phi) -> bool {
      return generator->isUsed(&phi);
    };
    auto handlePhi = 
      [&getAssocValue, &destination, &isUsedPhi](Disjunction destState) {
      auto destBlock = llvm::dyn_cast<llvm::BasicBlock>(destination);
      for (auto& phi : destBlock->phis()) {
        if (!isUsedPhi(phi)) {
          continue;
        }
        auto phiAsValue    = llvm::dyn_cast<llvm::Value>(&phi);
        auto phiExprID     = generator->GetOrCreateExprID(phiAsValue);
        auto phiOperand    = getAssocValue(&phi);
        auto operandExprID = generator->GetOrCreateExprID(phiOperand);
        // Phis can be loop dependent
        destState = generator->rewrite(destState, phiExprID, operandExprID);
      }
      return destState;
    };

    auto* branchOrSwitch    = llvm::dyn_cast<llvm::Instruction>(branchAsValue);
    auto isConditionalJump = [&branchOrSwitch]() {
      return branchOrSwitch->getNumOperands() > 2;
    };

    auto edgeOp = [&](Disjunction& destState) {
      cleanBackEdge(destState, branchAsValue, destination);
      auto oldState = destState;
      auto withPhi  = handlePhi(destState);
      if (!isConditionalJump()) {
        return withPhi;
      }

      auto&& retState = handle(branchOrSwitch, withPhi, destination);
      return retState;
    };

    Debugger::ContributionCounter counter{branchOrSwitch};
    return static_for{edgeOp}(toMerge);
  }

  DisjunctionValue
  rewriteArgs(DisjunctionValue& toRewrite,
              const llvm::CallSite& caller,
              llvm::Function* indCallee) {
    auto wrapper = [&,this](Disjunction toRewrite) {
      return argRewriter(toRewrite, caller, indCallee);
    };
    return static_for{wrapper}(toRewrite);
  }

  /* The edge transformer also works on indirect callees, 
   * (consider them as edges in an inter-procedural control-flow-graph).
   * In this situation however, the edge-transformer must rewrite the
   * states with their arguments at the callsites. */
  DisjunctionValue
  operator()(DisjunctionValue& toMerge,
             const llvm::CallSite& caller,
             llvm::Function* indCallee) {
    auto getNonConst = [&indCallee](const llvm::Function* cfunc) {
      auto* module = indCallee->getParent();
      return module->getFunction(cfunc->getName());
    };

    auto getAliasConjunct = [](const llvm::CallSite& caller,
                               llvm::Function* const mayCallee,
                               bool form) {
      auto aliasExpr = generator->GetOrCreateAliasID(mayCallee, caller);
      //auto numCallees = svfResults->getIndCSCallees(caller).size();
      return Conjunct(aliasExpr, form);
    };

    auto callStitcher = [&](Disjunction destState) {
      auto callees = svfResults->getIndCSCallees(caller);
      if (callees.size() <= 1 || destState.isEmpty()) {
        return destState;
      } // Early return if there's only a single target

      //intrinsic representation of exclusivity
      auto callConjunct = getAliasConjunct(caller, getNonConst(indCallee), true);
      destState.applyConjunct(callConjunct);
      return destState;
    };
    return static_for{callStitcher}(toMerge);
  }


private:
  ArgumentRewriter argRewriter;

  template <typename Lambda>
  struct static_for {
    Lambda& lambda;
    DisjunctionValue result;

    static_for(Lambda& lm) 
      : lambda(lm) { }

    DisjunctionValue
    operator()(DisjunctionValue& value) {
      static_for_impl<MaxPrivilege>(value);
      return result;
    }
    
    template<int Counter>
    void
    static_for_impl(DisjunctionValue& value) {
      result[Counter] = lambda(value[Counter]);
      static_for_impl<Counter - 1>(value);
      return;
    }

    template<>
    void
    static_for_impl<MinPrivilege>(DisjunctionValue& value) {
      result[MinPrivilege] = lambda(value[MinPrivilege]);
      return;
    }
  };


  Disjunction&
  handle(llvm::Instruction* branchOrSwitch, Disjunction& destState, llvm::Value* destination) {
    if (auto branchInst = llvm::dyn_cast<llvm::BranchInst>(branchOrSwitch)) {
      return handleAsBranch(branchInst, destState, destination);
    } else {
      auto switchInst = llvm::dyn_cast<llvm::SwitchInst>(branchOrSwitch);
      return handleAsSwitch(switchInst, destState, destination);
    }
    return destState;
  }

  Disjunction&
  handleAsSwitch(llvm::SwitchInst* switchInst, Disjunction& destState, llvm::Value* destination) {
    auto getConjunctForm = [&](auto& caseOp) {
      llvm::BasicBlock* targetBB = caseOp.getCaseSuccessor();
      return targetBB == llvm::dyn_cast<BasicBlock>(destination);
    };
    auto getCaseExprID = [&](auto& caseOp) {
      llvm::Constant* caseVal = caseOp.getCaseValue();
      auto caseValueAsExprID  = generator->GetOrCreateExprID(caseVal);
      auto* condition         = switchInst->getCondition();
      auto conditionAsExprID  = generator->GetOrCreateExprID(condition);

      ExprKey key{conditionAsExprID, llvm::Instruction::Switch, caseValueAsExprID};
      auto form = getConjunctForm(caseOp);
      return std::make_pair(generator->GetOrCreateExprID(key, switchInst), form);
    };

    auto isDefaultCase = destination == switchInst->getDefaultDest();
    for (auto& caseOp : switchInst->cases()) {
      auto  [asExprID, form]  = getCaseExprID(caseOp);
      if (form || isDefaultCase) {
        destState.applyConjunct({asExprID, form});
      }
    }
    return destState;
  }


  Disjunction&
  handleAsBranch(llvm::BranchInst* branchInst, Disjunction& destState, llvm::Value* destination) {
    auto conjunctForm = [&] ( ) {
      return destination == branchInst->getOperand(2);
    };
    auto* condition        = branchInst->getCondition();
    auto conditionAsExprID = generator->GetOrCreateExprID(condition);
    destState.applyConjunct({conditionAsExprID, conjunctForm()});
    return destState;
  }
};



/*--------------------------------------------------------*
 *------------------TRANSFER FUNCTION---------------------*
 *--------------------------------------------------------*/
class DisjunctionTransfer {
public:
  llvm::Instruction*
  setLocation(llvm::Instruction* inst) {
    assert(inst && "Not an instruction");
    generator->setLocation(inst);
    return inst;
  }

  void
  removeLocation(llvm::Instruction* checkLoc) {
    assert(generator->getLocation() == checkLoc && "Location changed");
    generator->setLocation(nullptr);
  }

  bool isInterprocedural;

  template <int PledgeCounter>
  void
  callTransfers(llvm::Value& value,
                DisjunctionState& stateMap,
                const Context& context) {
    auto* inst   = llvm::dyn_cast<llvm::Instruction>(&value);
    bool handled = false;
    if (auto constantExpr = llvm::dyn_cast<llvm::ConstantExpr>(&value)) {
      llvm::errs() << "\n\n" << value;
      llvm_unreachable("\nFound constant exprs");
    }
    Debugger::ContributionCounter counter{inst};

    constexpr Promises promise = static_cast<Promises>(PledgeCounter);
    /* PLUGIN for stdio REMOVE THIS */
    //if constexpr (promise == PLEDGE_STDIO) {
    //  return;
    //}
    /* REMOVE THIS */
    auto state = StateAccessor<promise>{stateMap};
    bool notEmpty = !state[nullptr].isEmpty();

    /* Get reachability. The transfers should not be changing the reachability
     * but it does generate states which are default initialized to
     * unreachable. */
    handled |= handleBrOrSwitch(inst, handled);
    handled |= handlePhiBackEdges(&value, state, handled);
    handled |= handleCallSite<promise>(&value, state, context, handled);
    handled |= handleStore(&value, state, context, handled);
    handled |= handleGep(&value, state, handled);
    handled |= handleRet(&value, state, context, handled);
    // Check use
    handled |= handleLoad(&value, state, handled);
    handled |= handleBinaryOperator(&value, state, handled);
    handled |= handleCmpInst(&value, state, handled);
    handled |= handleCast(&value, state, handled);
    handleUnknown(&value, state, handled);

    if (notEmpty && state[nullptr].isEmpty()) {
      llvm::CallSite callSite{&value};
      if (!callSite.getInstruction()) {
        llvm::outs().changeColor(llvm::raw_ostream::Colors::RED);
        llvm::outs() << "\n\n\n\n"
                     << "DISJUNCT BECAME EMPTY BEFORE SIMPLIFCATION"
                     << "\n\n\n\n";
        llvm::outs().resetColor();
      }
    }


    //llvm::errs() << "\nBefore Sem Simplifier";
    //state[nullptr].print(llvm::errs());
    semSimplifier.simplifyConstants(state[nullptr], value);

    //llvm::errs() << "\nAfter Sem Simplifier";
    //state[nullptr].print(llvm::errs());

    state[nullptr]
        .simplifyComplements()
        .simplifyRedundancies(generator->GetVacuousConjunct())
        .simplifyImplication();

    auto becameEmpty = notEmpty && state[nullptr].isEmpty();
    if (becameEmpty) {
      llvm::CallSite callSite{&value};
      if (!callSite.getInstruction()) {
        llvm::errs().changeColor(llvm::raw_ostream::Colors::RED);
        llvm::errs() << "\n\n\n\n"
                     << "DISJUNCT BECAME EMPTY AFTER SIMPLIFCATION"
                     << "\n\n\n\n";
        llvm::errs().resetColor();
      }
    }

    //for (const auto& [key, value] : stateMap) {
    //  llvm::errs() << "\nState Size for promise " << PromiseNames[promise];
    //  if (key) {
    //    llvm::errs() << " with key: " << *key << " ";
    //  } else {
    //    llvm::errs() << " with key nullptr: ";
    //  }
    //  llvm::errs() << value[promise].size();
    //}
    Debugger debugger{PledgeCounter};
    debugger.printAfter(stateMap[nullptr],llvm::dyn_cast<llvm::Instruction>(&value), context);
    debugger.printActivePrivileges(stateMap[nullptr]);
    return;
  }

  template <int Counter>
  void
  static_for(llvm::Value& value,
             DisjunctionState& state,
             const Context& context) {
    callTransfers<Counter>(value, state, context);
    static_for<Counter - 1>(value, state, context);
    return;
  }

  template <>
  void
  static_for<MinPrivilege>(llvm::Value& value, DisjunctionState& state, const Context& context) {
    callTransfers<MinPrivilege>(value, state, context);
    return;
  }

  void
  operator()(llvm::Value& value,
             DisjunctionState& state,
             const Context& context) {
    static_for<MaxPrivilege>(value, state, context);
    return;
  }

private:
  SemanticSimplifier semSimplifier;

  template <Promises promise>
  struct StateAccessor {
    StateAccessor(DisjunctionState& sm)
      : stateMap{sm} { }
    
    DisjunctionState& stateMap;

    Disjunction&
    operator[](llvm::Instruction* inst) {
      return stateMap[inst][promise];
    }
    
  
    bool
    has(llvm::Instruction* inst) {
      return stateMap.count(inst);
    }

    bool
    erase(llvm::Instruction* key) {
      return stateMap.erase(key);
    }
  };

  bool
  handleBrOrSwitch(llvm::Instruction* inst, bool handled) {
    return !handled && (llvm::isa<llvm::BranchInst>(inst)
      || llvm::isa<llvm::SwitchInst>(inst));
  }

  Edges
  getBackedges(const llvm::Function* func) {
    return functionBackEdges[func];
  }

  template <Promises Promise>
  bool
  handlePhiBackEdges(llvm::Value* value, StateAccessor<Promise>& state, bool handled) {
    if (handled) {
      return true;
    }

    auto* phi = llvm::dyn_cast<llvm::PHINode>(value);
    if (!phi) {
      return false;
    }
    auto isFromBackEdge =
      [&phi, this](const llvm::BasicBlock* incomingBlock) -> bool {
      auto* parentBlock    = phi->getParent();
      auto* parentFunction = parentBlock->getParent();
      auto backEdges       = getBackedges(parentFunction);
      auto foundIt         = std::find_if(
          backEdges.begin(), backEdges.end(), [&](const auto& bbPair) {
            return bbPair.first == incomingBlock
                   && bbPair.second == parentBlock;
          });
      return foundIt != backEdges.end();
    };

    bool isLoopPhi = false;
    for (auto& incomingBlock : phi->blocks()) {
      isLoopPhi |= isFromBackEdge(incomingBlock);
    }

    if (!isLoopPhi) {
      return true;
    }
    //auto oldSize  = state[nullptr].conjunctCount();
    auto phiExprID = generator->GetOrCreateExprID(value);
    state[nullptr] = generator->pushToTrue(state[nullptr], phiExprID);
    state[nullptr] = state[nullptr].simplifyImplication();
    //auto newsize  = state[nullptr].conjunctCount();
    //llvm::outs() << "\nDropping loop phi: " << *value << " oldsize - newsize "
    //             << oldSize << " " << newsize;
    return true;
  }


  enum class CallType {
    IndCall,
    ExternalCall,
    LLVMSpecific,
    Internal
  };

  CallType
  getCallType(llvm::CallSite& cs) {
    bool isIndCall = [&cs] {
      auto* PAG = svfResults->getPAG();
      return PAG->isIndirectCallSites(cs);
    }();

    bool isExtCall = [&cs, &isIndCall] {
      return !isIndCall && analysis::isExternalCall(cs);
    }();

    bool isLLVMCall = [&cs, &isIndCall] {
      return !isIndCall
             && analysis::getCalledFunction(cs)->getName().startswith("llvm");
    }();

    bool isBlackListed = [&cs, &isIndCall, &isLLVMCall] {
      return !isIndCall && !isLLVMCall && analysis::isBlackListed(cs);
    }();


    if (isIndCall) {
      return CallType::IndCall;
    } else if (isExtCall) {
      return CallType::ExternalCall;
    } else if (isLLVMCall) {
      return CallType::LLVMSpecific;
    } else if (isBlackListed) {
      return CallType::ExternalCall;
    } else {
      return CallType::Internal;
    }
  }


  template<Promises Promise>
  bool
  handleCallSite(llvm::Value* value,
                 StateAccessor<Promise>& state,
                 const Context& context,
                 bool handled) {
    llvm::CallSite callSite{value};
    if (handled || !callSite.getInstruction()) {
      return handled;
    }
    //llvm::errs() << "\nprinting from callsite" << *callSite.getInstruction();
    //llvm::errs() << "\nState at key";
    //state[callSite.getInstruction()].print(llvm::errs());

    auto getCalleeState = [&state, &callSite] {
      auto calleeState = state[callSite.getInstruction()];
      if constexpr (Promise == MinPrivilege) { 
        state.erase(callSite.getInstruction());
      }
      llvm::errs() << "\nGetting Callee State";
      calleeState.print(llvm::errs());
      return calleeState;
    };

    auto stubWorker = [&state, &value](bool isWhiteListed) {
      if (!isWhiteListed) {
        makeVacuouslyTrue(state[nullptr]);
      } else if (isWhiteListed && generator->isUsed(value)) {
        auto oldExprID = generator->GetOrCreateExprID(value);
        state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);
      }
    };

    auto handleExternalCall = 
      [&state,  &context, &stubWorker] (auto callInfo) { // FName or CallSite
      if (privilegeResolver->hasPrivilege<Promise>({callInfo}, context)) {
        llvm::errs() << "\nGoing to Top";
        makeVacuouslyTrue(state[nullptr]);
      } else {
        privilegeResolver->handleStubs(stubWorker, {callInfo});
      }
    };

    auto handleIndCall = 
      [&state, &callSite, &value, &handleExternalCall, this, &getCalleeState] {
      /* If the call resolves to an internal function, the abstract state
       * within the calle is rewritten with the arguments at the callsite. This
       * is handled by the edge transformer in this case, since the meet
       * function is lossy regarding the function-state ownership information,
       * resulting in a search + replace across all function states if the
       * argument rewriting were to be handled here.
       * Simplification I suspect simplification to be easier when performed
       * over every incoming pair vs over the entire set */
      if (!isInterprocedural) {
        auto oldExprID = generator->GetOrCreateExprID(value);
        state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);
        return; // Push conjuncts to true and exit
      } else if (svfResults->hasIndCSCallees(callSite)) {
        // The state will either have the merged states from all it's
        // callees. If a callee is an external call, it should be 
        // treated as such.
        state[nullptr] = getCalleeState();
        for (auto* func : svfResults->getIndCSCallees(callSite)) {
          if (func->isDeclaration()) {
            handleExternalCall(func->getName());
          } 
        }
      } else {  // if SVF is unable to find callees
        llvm::errs() << "\n Could Not Find Callee for"
                     << *callSite.getInstruction();
        dropOperands(value, state);
        auto oldExprID = generator->GetOrCreateExprID(value);
        state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);
        //makeVacuouslyTrue(state[nullptr]);
        return;
      }
    };

    auto handleInternal = [&state, &callSite, &value, this, &getCalleeState] {
      if (!isInterprocedural) {
        auto oldExprID = generator->GetOrCreateExprID(value);
        state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);
        return; // Push conjuncts to true and exit
      }
      Debugger::printCalleeStats(callSite, llvm::errs(), state);

      llvm::Function* callee = analysis::getCalledFunction(callSite);
      // auto calleeState      = state[callSite.getInstruction()];
      state[nullptr] = getCalleeState(); 
      //this->argRewriter(getCalleeState(), callSite, callee);
    };

    switch (getCallType(callSite)) {
      case CallType::IndCall:
        handleIndCall();
        return true;
        break;
      case CallType::ExternalCall:
        handleExternalCall(callSite);
        return true;
        break;
      case CallType::LLVMSpecific:
        /*TODO: llvm versions of memcpy etc will need to be managed*/
        return true; 
        break;
      case CallType::Internal:
        handleInternal();
        return true;
        break;
    }

    assert(false && "Broken Casing");
    return true;
  }

  template <Promises Promise>
  bool
  handleGep(llvm::Value* const value, StateAccessor<Promise>& state, bool handled) {
    // Possibly can be guarded by isUsed
    if (handled) {
      return handled;
    } 
    auto* gep = llvm::dyn_cast<llvm::GetElementPtrInst>(value);
    if (!gep) {
      return false;
    }
    if (!generator->isUsed(value)) {
      return true;
    }

    //llvm::errs() << "\nGep SRC type " << *gep->getSourceElementType();
    //llvm::errs() << "\nGep RES type " << *gep->getResultElementType();

    bool isArrayGEP = [&gep] {
      auto* resType = gep->getSourceElementType();
      //if (resType->isArrayTy()) {
      //  llvm::errs() << "\nArray Type GEP ";
      //  llvm::errs() << *resType;
      //  llvm::errs() << "GEP:" << *gep;
      //}
      return resType->isArrayTy();
    }();

    if (isArrayGEP) {
      auto oldExprID = generator->GetOrCreateExprID(value);
      //llvm::errs() << "\nWith/Killing ExprID " << oldExprID;
      //llvm::errs() << "\nBefore killing geps";
      //state[nullptr].print(llvm::errs());
      state[nullptr] = semSimplifier.killGEP(gep, state[nullptr]);
      dropOperands(value, state);
      //state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);
      //llvm::errs() << "\nAfter killing geps";
      //state[nullptr].print(llvm::errs());
      return true;
    }
    
    auto* resType = gep->getResultElementType();
    if (resType->isPointerTy()) {
      //llvm::errs() << "\nFound gep ptr result type";
      //llvm::errs() << *gep;
      //llvm::errs() << "\n\nSourceType" << *gep->getSourceElementType()
      //             << "\nResultType" << *gep->getResultElementType();

      auto oldExprID = generator->GetOrCreateExprID(value);
      //llvm::errs() << "\nWith/Killing ExprID " << oldExprID;
      //llvm::errs() << "\nBefore killing geps";
      //state[nullptr].print(llvm::errs());
      state[nullptr] = semSimplifier.killGEP(gep, state[nullptr]);
      dropOperands(value, state);
      state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);

      //llvm::errs() << "\nAfter killing geps";
      //state[nullptr].print(llvm::errs());
      return true;
    }

    if (gep->getSourceElementType() == gep->getResultElementType()) {
      //llvm::errs() << "\nFound gep with src == result";
      //llvm::errs() << "\n\nSourceType" << *gep->getSourceElementType()
      //             << "\nResultType" << *gep->getResultElementType();

      auto oldExprID = generator->GetOrCreateExprID(value);
      //llvm::errs() << "\nWith/Killing ExprID " << oldExprID;
      //llvm::errs() << "\nBefore killing geps";
      //state[nullptr].print(llvm::errs());
      state[nullptr] = semSimplifier.killGEP(gep, state[nullptr]);
      dropOperands(value, state);
      //state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);
      //llvm::errs() << "\nAfter killing geps";
      //state[nullptr].print(llvm::errs());
      return true;
    }

    auto oldExprID = generator->GetOrCreateExprID(value);
    auto gepExprID = generator->GetOrCreateExprID(gep);
    // GEPs can be loop dependent
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, gepExprID);
    return true;
  }

  using NodeExprPair = std::pair<BinaryExprNode, ExprID>;
  auto
  seperateLoads(llvm::StoreInst* const storeInst, std::vector<NodeExprPair>& loads) {
    std::vector<NodeExprPair> otherLoads;
    std::vector<NodeExprPair> strongLoads;

    auto distill = [&] (llvm::LoadInst* loadInst, BinaryExprNode node, ExprID exprID) {
      auto aliasResult = getAliasType(storeInst, loadInst);
      if (loadInst->getParent()->getParent() != storeInst->getParent()->getParent()) {
        llvm::errs() << "\n Interprocedural";
      }
      if (aliasResult == AliasResult::MustAlias) {
        strongLoads.push_back({node, exprID});
        llvm::errs() << "\n Found must-alias for " << *loadInst << " and " << *storeInst;
        return;
      } else if (aliasResult == AliasResult::NoAlias) {
        llvm::errs() << "\n Found no-alias for   " << *loadInst << " and " << *storeInst;
        return; //Discard NoAliases
      } else {
        llvm::errs() << "\n Found may-alias for  " << *loadInst << " and " << *storeInst;
        //llvm::errs() << "\n with ExprID" << exprID;
        otherLoads.push_back({node, exprID});
        return;
      }
    };

    for (auto [loadNode, exprID] : loads) {
      auto loadInst = llvm::dyn_cast<llvm::LoadInst>(loadNode.value);
      distill(loadInst, loadNode, exprID);
    }
    return std::make_pair(strongLoads, otherLoads);
  }

  /* General Load/Store pairs may have non-exclusive alias sets.
   * Alias sets to indcalls are necessarily exclusive.
   * WHen non-exclusive, only the (negated) conjunct describing
   * the non-aliasing relationship between the current load
   * pointer being analyzed and other store pointers must be killed.
   * For exclusive sets since the load pointer *must* point to any one
   * out of a set of aliases, the negated conjuncts describing 
   * otherwise must be killed.*/
  Disjunction
  killConflicts(const Disjunction& state, llvm::StoreInst* storeInst) {
    auto isAlias = [](const auto exprID) -> std::optional<const BinaryExprNode> {
      auto node = generator->GetExprNode(exprID);
      if (auto* binNode = std::get_if<const BinaryExprNode>(&node)) {
        auto isAliasOpCode = binNode->op.opCode == OpIDs::Alias;
        return 
          isAliasOpCode ? std::optional<BinaryExprNode>{*binNode}
                        : std::nullopt;
      }
      return std::nullopt;
    };

    auto isMustAlias = 
      [&storeInst, this](const BinaryExprNode& binNode) -> bool {
      if (auto* loadInst = llvm::dyn_cast<llvm::LoadInst>(binNode.value)) {
        return getAliasType(storeInst, loadInst)
               == llvm::AliasResult::MustAlias;
      } else { // Get Function from left node
        llvm::errs() << "\nBad Access at " << binNode.lhs;
        llvm::errs() << "\nFor store inst " << *storeInst;
        llvm::errs() << "\nFor bin node value " << *binNode.value;
        auto  functionNode    = generator->GetExprNode(binNode.lhs);
        auto asValueExprNode  = std::get<const ConstantExprNode>(functionNode);
        auto* asValue         = asValueExprNode.constant;
        auto* asFunction      = llvm::dyn_cast<llvm::Function>(asValue);
        return getAliasType(storeInst, asFunction)
               == llvm::AliasResult::MustAlias;
      }
    };

    auto isCallAlias = [](const BinaryExprNode& binNode) -> bool {
      return !(llvm::isa<llvm::LoadInst>(binNode.value));
    };

    /* Kill conflicting definitions of a load value at store*/
    auto localState = state;
    auto killConjunct = [&localState](const Conjunct& conjunct) {
      llvm::errs() << "\nKilling ExprID (CONJUNCT)" << conjunct.exprID;
      llvm::errs() << "\nBefore killing conjuncts";
      localState.print(llvm::errs());
      
      for (auto& disjunct : localState.disjuncts) {
        auto from = std::remove_if(disjunct.begin(), disjunct.end(),
            [&conjunct](const auto& target) {
              return target.exprID == conjunct.exprID /*&& !target.notNegated*/;
            });
        disjunct.conjunctIDs.erase(from, disjunct.end());
      }

      llvm::errs() << "\nAfter killing conjuncts";
      localState.print(llvm::errs());
    };

    auto killDisjunct = [&localState](const Disjunct& disjunct) {
      llvm::errs() << "\nKilling Disjunct";
      disjunct.print(llvm::errs());

      auto from = std::remove_if(localState.begin(), localState.end(),
          [&disjunct](const auto& incoming) {
            return disjunct == incoming;
          });
      localState.disjuncts.erase(from, localState.disjuncts.end());
    };

    std::unordered_set<ExprID> finished;
    for (auto& disjunct : state.disjuncts) {
      for (auto& conjunct : disjunct.conjunctIDs) {
        if (finished.count(conjunct.exprID)) {
          continue; 
        } // Check if already processed since it's not inplace
        if (auto binNode = isAlias(conjunct.exprID); binNode 
            && isMustAlias(*binNode) && !(conjunct.notNegated)) { // neg forms only

          if (!isCallAlias(*binNode)) {
            killConjunct(conjunct); // kill conjunct if load alias
          } else {
            killDisjunct(disjunct); // kill disjunct if call alias
          }
        }
        finished.insert(conjunct.exprID);
      }
    }
    return localState
        .simplifyComplements()
        .simplifyRedundancies(generator->GetVacuousConjunct())
        .simplifyImplication();
  }

  bool
  validate(llvm::StoreInst* store) {
    auto* valueOperand = store->getValueOperand();
    bool isFromLoad    = llvm::isa<llvm::LoadInst>(valueOperand);
    auto isPointer = valueOperand->getType()->isPointerTy();
    if (isFromLoad && isPointer) {
      llvm::errs() << "From load with pointer " << *valueOperand;
    }
    return isFromLoad && isPointer;
  }

  void
  sterilise(Disjunction& disjunction,
            const ExprID loadExprID,
            const BinaryExprNode& loadNode) {
    disjunction = generator->pushToTrue(disjunction, loadExprID);
    return;
  }


  template <Promises Promise>
  bool
  handleStore(llvm::Value* const value,
              StateAccessor<Promise>& state,
              const Context context,
              bool handled) {
    using NodeExprPair = std::pair<BinaryExprNode, ExprID>;
    if (handled) {
      return true;
    }
    auto* storeInst = llvm::dyn_cast<llvm::StoreInst>(value);
    if (!storeInst) {
      return false;
    }
    /* Kill all offending aliases to the current store.
     * Correct since it's a path specific update. 
     * This would only work for must aliases since anything which 
     * works otherwise would be incorrect. Ensure that it works
     * for both indCallSite and regular load/store pairs */
    state[nullptr] = killConflicts(state[nullptr], storeInst);

    //llvm::errs() << "\n Handling store at " << *value;
    std::vector<NodeExprPair> asLoads = getLoads(state);
    auto [strongLoads, weakLoads]     = seperateLoads(storeInst, asLoads);
    bool isValid = validate(storeInst);
    
    auto localDisjunction = state[nullptr];
    for (auto& [loadNode, exprID] : strongLoads) {
      //llvm::errs() << "\nStrong load for " << *(loadNode.value);
      state[nullptr] = strongUpdate(localDisjunction, exprID, loadNode, storeInst);
    }

    for (auto& [loadNode, exprID] : weakLoads) {
      //llvm::errs() << "\nWeak load for " << *(loadNode.value) << " id " << exprID;
      if (!isValid) {
        sterilise(localDisjunction, exprID,  loadNode);
      } else {
      state[nullptr] =
          weakUpdate(localDisjunction, exprID, loadNode, storeInst);
      }
    }
    return true;
  }

  template<Promises Promise>
  bool
  handleRet(llvm::Value* const value,
            StateAccessor<Promise>& state,
            const Context context,
            bool handled) {
    auto* ret = llvm::dyn_cast<llvm::ReturnInst>(value);
    if (handled || !ret) {
      return handled;
    }

    if (!isInterprocedural) {
      return true;
    }
  
    llvm::Value* callAsValue = nullptr;
    for (auto* inst : context) {
      if (inst != nullptr) {
        callAsValue = llvm::dyn_cast<llvm::Value>(inst);
      }
    }

    auto isMain = [&ret] {
      auto fName = ret->getFunction()->getName();
      llvm::errs() << "\nPrinting Function Name";
      llvm::errs() << fName;
      return fName == "main";
    }(); // set reachable if main.

    state[nullptr].isReachable |= isMain;
    if (!callAsValue) {
      return true;
    }

    state[nullptr] = state[ret];
    state[nullptr].isReachable |= isMain;
    //llvm::errs() << "\nstate[ret]";
    //state[ret].print(llvm::errs());

    if constexpr (Promise == MinPrivilege) { 
      state.erase(ret);
      //llvm::errs() << "\nAfter wiping ret: " << state.has(ret);
      //assert(!(state.has(ret)) && "ret not empty");
    }
    auto* retValue = ret->getReturnValue();
    if (!retValue) {
      return true;
    }

    auto newExprID = generator->GetOrCreateExprID(retValue);
    auto oldExprID = generator->GetOrCreateExprID(callAsValue);
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, newExprID);
    return true;
  }

  template <Promises Promise>
  bool
  handleLoad(llvm::Value* const value, StateAccessor<Promise>& state, bool handled) {
    if (handled) {
      return true;
    }
    auto* loadInst = llvm::dyn_cast<llvm::LoadInst>(value);
    if (!loadInst) {
      return false;
    }
    if (!generator->isUsed(value)) {
      return true;
    }


    auto ptrOp = loadInst->getPointerOperand();
    auto ptrTy = ptrOp->getType();
    llvm::errs() << "\nLoad     Type:" << *loadInst->getType();
    llvm::errs() << "\nLoad Ptr Type:" << *ptrOp->getType();

    if (ptrTy->getPointerElementType()->isPointerTy()) {
      llvm::errs() << "\nLoad Inst " << *loadInst;
      llvm::errs() << "\nLoad with ptr2ptr as ptr";

      auto oldExprID = generator->GetOrCreateExprID(value);
      llvm::errs() << "\nWith/Killing ExprID " << oldExprID;
      llvm::errs() << "\nBefore killing loads";
      state[nullptr].print(llvm::errs());
      state[nullptr] = semSimplifier.killLoad(loadInst, state[nullptr]);
      dropOperands(value, state);
      state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);
      llvm::errs() << "\nAfter killing loads";
      state[nullptr].print(llvm::errs());
      return true;
    }

    if (auto asGep = llvm::dyn_cast<llvm::GetElementPtrInst>(ptrOp);
        asGep && asGep->getSourceElementType() == asGep->getResultElementType()) {
      llvm::errs() << "\nfound load through gep with same src and result";
      llvm::errs() << "\nloadInst" << *loadInst;
      llvm::errs() << "\npointer op" << *loadInst->getPointerOperand();

      llvm::errs() << "\n\nSourceType" << *asGep->getSourceElementType()
                   << "\nResultType" << *asGep->getResultElementType();

      auto oldExprID = generator->GetOrCreateExprID(value);
      llvm::errs() << "\nWith/Killing ExprID " << oldExprID;
      llvm::errs() << "\nBefore killing loads";
      state[nullptr].print(llvm::errs());
      state[nullptr] = semSimplifier.killLoad(loadInst, state[nullptr]);
      dropOperands(value, state);
      state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);
      llvm::errs() << "\nAfter killing loads";
      state[nullptr].print(llvm::errs());
      return true;
    }

    //lvm::outs() << "\nHandling Load";
    auto oldExprID = generator->GetOrCreateExprID(value);
    auto newExprID = generator->GetOrCreateExprID(loadInst);
    // Loads can be loop dependent
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, newExprID);
    return true;
  }

  template <Promises Promise>
  bool
  handleBinaryOperator(llvm::Value* const value, StateAccessor<Promise>& state, bool handled) {
    if (handled) {
      return true;
    }
    auto* const binOp = llvm::dyn_cast<llvm::BinaryOperator>(value);
    if (!binOp) {
      return false;
    }
    if (!generator->isUsed(value)) {
      return true;
    }
    auto oldExprID = generator->GetOrCreateExprID(value);
    auto exprID    = generator->GetOrCreateExprID(binOp);
    if (oldExprID == exprID) {
      llvm_unreachable("oldExprId == newexprId");
      return false;
    }
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, exprID);
    return true;
  }

  template <Promises Promise>
  bool
  handleCmpInst(llvm::Value* const value, StateAccessor<Promise>& state, bool handled) {
    if (handled) {
      return true;
    }
    auto cmpInst = llvm::dyn_cast<llvm::CmpInst>(value);
    if (!cmpInst) {
      return false;
    }
    if (!generator->isUsed(value)) {
      return true;
    }
    auto oldExprID = generator->GetOrCreateExprID(value);
    auto exprID = generator->GetOrCreateExprID(cmpInst);
    if (oldExprID == exprID) {
      return false;
    }
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, exprID);
    return true;
  }

  template <Promises Promise>
  bool
  handleCast(llvm::Value* const value, StateAccessor<Promise>& state, bool handled) {
    if (handled) {
      return true;
    }
    auto* castInst = llvm::dyn_cast<llvm::CastInst>(value);
    if (!castInst) {
      return false;
    }
    if (!generator->isUsed(value)) {
      return true;
    }
    auto oldExprID = generator->GetOrCreateExprID(value);
    auto exprID    = generator->GetOrCreateExprID(castInst);
    // Casts can be loop dependent
    state[nullptr] = generator->rewrite(state[nullptr], oldExprID, exprID);
    return true;
  }

  template <Promises Promise>
  bool
  handleUnreachable(llvm::Value* const value, StateAccessor<Promise>& state, bool handled) {
    if (handled) {
      return handled;
    }

    if (auto* unreachable = llvm::dyn_cast<llvm::UnreachableInst>(value)) {
      llvm::errs() << "\nSetting unreachable bit";
      state[nullptr].setUnreachable();
      return true;
    }
    return handled;
  }

  template <Promises Promise>
  void
  handleUnknown(llvm::Value* const value, StateAccessor<Promise>& state, bool handled) {
    if (handled) {
      return;
    }
    llvm::errs() << "\nUnknown at " << *value;
    //dropOperands(value, state);
    if (!generator->isUsed(value)) {
      return;
    }
    auto oldLeafTableSize = generator->getLeafTableSize();

    auto oldExprID = generator->GetOrCreateExprID(value);
    //llvm::errs() << "\nDropping Unknown " << oldExprID;
    state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);
    assert(oldLeafTableSize == generator->getLeafTableSize() && "New Value at Unknown");
  }

  /// Helpers ///
  template <Promises Promise>
  void
  dropOperands(const llvm::Value* value, StateAccessor<Promise>& state) {
    //TODO: Does not work for callsites. Fix later
    //llvm::errs() << "\nDropping operands of " << *value;
    for (auto& op : value->uses()) {
      if (!generator->isUsed(op.get())) {
        continue;
      }
      llvm::errs() << "\nDropping Unknown Op for" << *(op.get());
      auto oldExprID = generator->GetOrCreateExprID(op.get());
      state[nullptr] = generator->pushToTrue(state[nullptr], oldExprID);
    }
  }

  template <Promises Promise>
  std::vector<NodeExprPair>
  getLoads(StateAccessor<Promise>& state) {
    std::unordered_set<ExprID> foundIDs;
    std::vector<NodeExprPair> asLoads;

    auto isBinaryExprID = [](const ExprID exprID) -> bool {
      return generator->GetExprType(exprID) == 2;
    };

    auto insertIfLoad =
        [&](const auto& exprID, const auto& node, const auto* value) {
          if (node.op.opCode == llvm::Instruction::Load
              //llvm::isa<llvm::LoadInst>(value) 
              //&& foundIDs.count(exprID) == 0
              /*&& binaryNode.op.opCode*/) {
            llvm::errs() << "\nInserting id" << exprID;
            foundIDs.insert(exprID);
            asLoads.push_back({node, exprID});
          }
        };
    auto findLoads = [&](const auto& exprID, auto& findLoads) {
      if (!isBinaryExprID(exprID)) {
        return;
      }
      auto& binaryNode = generator->GetBinaryExprNode(exprID);
      insertIfLoad(exprID, binaryNode, binaryNode.value);
      findLoads(binaryNode.lhs, findLoads);
      findLoads(binaryNode.rhs, findLoads);
      return;
    };

    llvm::errs() << "\n getting ids";
    for (auto& disjunct : state[nullptr].disjuncts) {
      for (auto& conjunct : disjunct.conjunctIDs) {
        findLoads(conjunct.exprID, findLoads);
      }
    }
    return {asLoads.begin(), asLoads.end()};
  }

  [[maybe_unused]]
  AliasResult
  getMemSSAResults(llvm::StoreInst* const storeInst,
                   llvm::LoadInst* const loadInst) {
    auto [storeFunc, loadFunc] = [&]() {
      return std::make_pair(storeInst->getFunction(), loadInst->getFunction());
    }();
     if (storeFunc != loadFunc) {
       return AliasResult::MayAlias;
     } // Conservatively assert that mem-ops from different functions alias
     auto [walker, memSSA] = [&]() {
       auto* func = storeInst->getFunction();
       return std::make_pair(functionMemSSAs[func]->getWalker(),
                             functionMemSSAs[func].get());
     }();
     assert(memSSA != nullptr && "No memSSA for function");

     auto* clobber     = walker->getClobberingMemoryAccess(loadInst);
     auto* storeMemAcc = memSSA->getMemoryAccess(storeInst);
     auto* storeMemDef = llvm::dyn_cast<llvm::MemoryDef>(storeMemAcc);
     for (auto memDef = clobber->defs_begin(); memDef != clobber->defs_end();
          memDef++) {
       if (memDef == storeMemDef) {
         return AliasResult::MustAlias;
       }
    }
    //llvm_unreachable("Part of the same function, but does not alias");
    return AliasResult::NoAlias;
  }

  AliasResult
  getSVFResults(llvm::MemoryLocation& storeAsMemLoc,
                   llvm::MemoryLocation& loadAsMemLoc) {
    llvm::errs() << "\nSVF Invoked";
    return svfResults->alias(loadAsMemLoc, storeAsMemLoc);
  }
  /* SVF has lower accuracy but filters interprocedural no-aliases
   * Use llvm's Aliasing stack for must-aliases and SVF for may-aliases*/
  AliasResult
  getAliasType(llvm::StoreInst* const storeInst,
               llvm::LoadInst* const loadInst) {
    auto* storeFunction = storeInst->getFunction();
    auto* AAWrapper     = functionAAs[storeFunction];
    auto& AAResults = AAWrapper->getAAResults();
    assert(AAWrapper != nullptr && "AA is nullptr");

    auto loadAsMemLoc  = llvm::MemoryLocation::get(loadInst);
    auto storeAsMemLoc = llvm::MemoryLocation::get(storeInst);
    auto aliasType = AAResults.alias(loadAsMemLoc, storeAsMemLoc);
    if (aliasType == AliasResult::MayAlias
        || aliasType == AliasResult::NoAlias) {
      return getSVFResults(storeAsMemLoc, loadAsMemLoc);
    }
    return aliasType;
  }

  AliasResult
  getAliasType(llvm::StoreInst* const storeInst,
               llvm::Function*  const function) {
    auto asValue = storeInst->getPointerOperand()->stripPointerCasts();
    if (auto* function = llvm::dyn_cast<llvm::Function>(asValue)) {
      // remove check
      llvm::errs() << "\nFound store with function pointer" << function->getName();
      exit(0);
    }
    return llvm::AliasResult::NoAlias;
  }

  Disjunction
  strongUpdate(const Disjunction& disjunction,
               const ExprID& loadExprID,
               const BinaryExprNode& loadNode,
               llvm::StoreInst* const storeInst) {
    auto operand    = storeInst->getValueOperand();
    auto opExprID   = generator->GetOrCreateExprID(operand);
    // Stores can also be loop dependent
    return generator->rewrite(disjunction, loadExprID, opExprID);
  }

  Disjunction
  weakUpdate(const Disjunction& disjunction,
             const ExprID& loadExprID,
             const BinaryExprNode& loadNode,
             llvm::StoreInst* const storeInst) {
    auto aliasConjunct = [](const BinaryExprNode& loadNode,
                            llvm::StoreInst* const storeInst) {
      auto aliasExpr = generator->GetOrCreateAliasID(loadNode, storeInst);
      return Conjunct(aliasExpr, true);
    }(loadNode, storeInst);

    auto reachable = disjunction.isReachable;
    Disjunction forRewrites{reachable};
    Disjunction noRewrites{reachable};
    for (const auto& disjunct : disjunction.disjuncts) {
      auto aliasDisjunct{disjunct};
      auto notAliasDisjunct{disjunct};
      auto found = false;
      for (auto& conjunct : disjunct.conjunctIDs) {
        if (generator->find(conjunct, loadExprID)) {
          aliasDisjunct.addConjunct(aliasConjunct);
          llvm::errs() << "\nAdded alias conjunct " << aliasConjunct.exprID;
          notAliasDisjunct.addConjunct(!aliasConjunct);
          forRewrites.addDisjunct(aliasDisjunct);
          noRewrites.addDisjunct(notAliasDisjunct);
          found = true;
          break;
        }
      }
      if (!found) {
        noRewrites.addDisjunct(disjunct);
      }
    }

    auto storeValue = storeInst->getValueOperand();
    auto valExprID  = generator->GetOrCreateExprID(storeValue);
    //llvm::errs() << "\nRewriting load exprID " << loadExprID
    //             << "  with store value expr " << valExprID;
    
    // TODO: may-alias can screw with the constraints
    // I expect loop dependent aliases to be must aliases though
    // May-aliases can also be loop dependent
    auto rewritten = generator->rewrite(forRewrites, loadExprID, valExprID);
    //llvm::errs() << "\n rewritten disjunction";
    //rewritten.print(llvm::errs());
    return Disjunction::unionDisjunctions(rewritten, noRewrites);
  }
};


class BuildPromiseTreePass : public llvm::ModulePass {
public:
  BuildPromiseTreePass()
    : llvm::ModulePass{ID}
      { }

  bool runOnModule(llvm::Module& m) override;
  void getAnalysisUsage(llvm::AnalysisUsage &info) const override;
  StringRef getPassName() const override;

  static char ID;
private:
  void initializeGlobals(llvm::Module&);
};
char BuildPromiseTreePass::ID = 0;

llvm::StringRef
BuildPromiseTreePass::getPassName() const {
  return "BuildPromiseTreePass";
}

void
BuildPromiseTreePass::initializeGlobals(llvm::Module& m) {
  generator = std::make_unique<Generator>(Generator{});
  //resolver  = std::make_unique<IndirectCallResolver>(IndirectCallResolver{m});
  privilegeResolver = std::make_unique<PrivilegeResolver>(m);

  svfResults = analysis::getSVFResults(m);
  //AndersenWaveDiffWithType::createAndersenWaveDiffWithType(m);
  //svfResults = analysis::SVFAnalysis::createAndersenWaveDiff(m);

  //libEventAnalyzer::getResults(m);
  for (auto& f : m) {
    if ( f.isDeclaration()) {
      continue;
    }
    //llvm::errs() << "\n Get Dom Tree for " << f.getName() << "\n";
    auto* DT = &getAnalysis<DominatorTreeWrapperPass>(f).getDomTree();
    //llvm::errs() << "\n Get AA Tree for " << f.getName() << "\n";
    auto* AAWrapper = &getAnalysis<AAResultsWrapperPass>(f);
    //llvm::errs() << "\n Get memSSA Tree for " << f.getName() << "\n";
    auto memSSA = std::make_unique<MemorySSA>(f, &AAWrapper->getAAResults(), DT);
    functionAAs.insert({&f, AAWrapper});

    functionMemSSAs.try_emplace(&f, std::move(memSSA));
    Edges backedges;
    llvm::FindFunctionBackedges(f, backedges);
    functionBackEdges.try_emplace(&f, backedges);

    auto* liWrapper = &getAnalysis<llvm::LoopInfoWrapperPass>(f);
    auto& loopInfo = liWrapper->getLoopInfo();
    auto asUptr = std::make_unique<llvm::LoopInfo>(std::move(loopInfo));
    functionLoopInfos.try_emplace(&f, std::move(asUptr));
  }
  return;
}

void
dumpGlobals(llvm::Module& m) {
  generator->dumpToFile<lowering::Printer>(m);
  privilegeResolver->dumpToFile(m);
}


std::vector<llvm::Instruction*>&
operator+=(std::vector<llvm::Instruction*>& to, std::vector<llvm::Instruction*>&& from) {
  std::copy(from.begin(), from.end(), std::back_inserter(to));
  return to;
}

template<typename ContextMerger>
class Instrument {
  using LoweringInfo = llvm::DenseMap<llvm::Instruction*, DisjunctionValue>;
public:
  std::vector<llvm::Instruction*>
  getFunctionTops(llvm::Module& module) {
    std::vector<llvm::Instruction*> insertionPts;
    for (auto& function : module) {
      if (function.isDeclaration()) {
        continue;
      }
      auto& entryBlock = function.getEntryBlock();
      llvm::Instruction& insertionPt = *(entryBlock.getFirstInsertionPt());
      insertionPts.push_back(&insertionPt);
    }
    return insertionPts;
  }

  template<typename Results>
  std::vector<llvm::Instruction*>
  firstNonTrivial(Results& results) {
    auto getFirstNonTrivial = [this](auto* function, auto& functionResults) -> llvm::Instruction* {
      for (auto* bb : llvm::ReversePostOrderTraversal(function)) {
        for (auto& inst : *bb) {
          auto& state = functionResults[&inst][nullptr];
          if (filter(state)) {
            return &inst;
          }
        }
      }
      return nullptr;
    };

    llvm::DenseSet<llvm::Instruction*> locations;
    for (auto& [context, cResults] : results) {
      for (auto& [function, functionResults] : cResults) {
        llvm::errs() << "\nfunction " << function->getName();
        if (auto* location = getFirstNonTrivial(function, functionResults)) {
          locations.insert(location);
        }
      }
    }

    return {locations.begin(), locations.end()};
  }

  template<typename Results>
  std::vector<llvm::Instruction*>
  getLoweringLocations(llvm::Module& module, Results& results) {
    std::vector<llvm::Instruction*> locations;
    //locations += getFunctionTops(module);
    locations  += firstNonTrivial(results);
    return locations;
  }

  template<typename Results>
  LoweringInfo
  getMergedResults(std::vector<llvm::Instruction*>& locations, Results& results) {
    auto getStates = [&results](auto* instruction) {
      std::vector<DisjunctionValue> states;
      auto* function = instruction->getFunction();
      for (auto& [context, cResults] : results) {
        if (cResults.count(function) < 1) {
          continue;
        }
        auto& funcResults = cResults[function]; // @context
        auto state = funcResults[instruction][nullptr];
        states.push_back(state);
      }
      return states;
    };

    auto merge = [this](llvm::ArrayRef<DisjunctionValue> values) {
    auto& merger = this->merger;
    return std::accumulate(values.begin(), values.end(),
      DisjunctionValue(),
      [merger] (DisjunctionValue v1, DisjunctionValue v2) {
        return merger.meetPair(v1, v2);
      });
    };

    auto mergeStates = [&getStates, &merge](auto* instruction) {
      auto states = getStates(instruction);
      return merge({states});
    };

    LoweringInfo ret;
    auto addToMap = [&mergeStates, this, &ret] (auto* inst) {
      auto mergedState = mergeStates(inst);
      //printMerged(mergedState);
      auto shouldAdd = filter(mergedState);
      if (shouldAdd) {
        ret[inst] = mergedState;
      }
    };

    for (auto* inst : locations) {
      addToMap(inst);
    }
    return ret;
  }
  
  bool
  makeInsertions(llvm::Module& module, LoweringInfo& info) {
    auto lower = [](auto* inst, auto& stateArray) {
      auto* module = inst->getModule();
      auto printer = lowering::Printer{generator.get(), llvm::outs(), module};
      auto* loc = printer.insertStringResetter(inst);
      // TODO: constexpr away
      for (int privilege = MaxPrivilege; privilege >= MinPrivilege; privilege--) {
        auto& disjunction = stateArray[privilege];
        if (disjunction.conjunctCount() < 1) {
          continue;
        }
        llvm::errs() << "\nGenerating disjunct";
        disjunction.print(llvm::errs());
        auto asEnum = static_cast<Promises>(privilege);
        printer.insertStringBuilder(inst, disjunction, asEnum);
      }
      printer.insertPledgeCall(inst);
      printer.insertStringResetter(inst);
      llvm::errs() << "\nAfter insertions";
      llvm::errs() << *(loc->getParent());
    };

    for (auto& [inst, stateArray] : info) {
      lower(inst, stateArray);
    }
    return true;
  }

  template<typename Results>
  void
  operator()(llvm::Module& module, Results& results) {
    auto asInsts = getLoweringLocations(module, results);
    auto loweringInfo = getMergedResults(asInsts, results);
    makeInsertions(module, loweringInfo);
    auto ec = std::error_code{};
    auto fileOuts =
        llvm::raw_fd_ostream{"/home/shreeasish/pledgerize-reboot/hoisting/"
                             "build-release/instrumented/fileStream",
                             ec};
    llvm::errs() << "\n\nRunning Verifier\n";
    if (!llvm::verifyModule(module, &llvm::errs())) {
      llvm::WriteBitcodeToFile(module, fileOuts);
    }
  }

private:
  //llvm::Module& module;
  //Results results;
  ContextMerger merger;

  bool
  filter(DisjunctionValue& stateArray) {
    for (int privilege = MaxPrivilege; privilege >= MinPrivilege; privilege--) {
      auto& disjunction = stateArray[privilege];
      if (disjunction.conjunctCount() > 1) {
        return true;
      }
    }
    return false;
  }

  bool
  printMerged(DisjunctionValue& stateArray) {
    for (int privilege = MaxPrivilege; privilege >= MinPrivilege; privilege--) {
      llvm::errs() << "\nMerged State for--" << PromiseNames[privilege];
      auto& disjunction = stateArray[privilege];
      disjunction.print(llvm::errs());
    }
    return false;
  }
};



void
runAnalysisFor(llvm::Module& m,
               llvm::ArrayRef<llvm::StringRef> functionNames,
               bool isInterprocedural) {
  using V = DisjunctionValue;
  using T = DisjunctionTransfer;
  using M = DisjunctionMeet;
  using E = DisjunctionEdgeTransformer;
  using D = Debugger;
  using Analysis 
    = analysis::DataflowAnalysis<V, T, M, E, D, analysis::Backward>;

  auto dumpAnnotatedCFG = [](llvm::Function* function,
                             Context context,
                             auto& results) {
    auto functionResults = results[context][function];
    llvm::Instruction& inst = *(function->getEntryBlock().getFirstInsertionPt());
    Debugger debugger{MaxPrivilege, llvm::outs()};
    debugger.dump(&inst, context, functionResults, 0);
  };


  auto ec = std::error_code{};
  auto fileOuts = llvm::raw_fd_ostream{"/home/shreeasish/pledgerize-reboot/hoisting/"
                       "build-release/debug.ll",ec};
  fileOuts << m;

  // Instrumentation class
  Instrument<DisjunctionMeet> instrument;
  for (auto fname : functionNames) {
    auto* entryPoint = m.getFunction(fname);
    if (!entryPoint) {
      llvm::report_fatal_error("Unable to find entrypoint " + fname);
    }
    Analysis analysis{m, entryPoint, svfResults, isInterprocedural}; // true for interprocedural
    auto results = analysis.computeDataflow();
    llvm::outs().changeColor(llvm::raw_ostream::Colors::GREEN);
    llvm::outs() << "\nFinished " << entryPoint->getName() << "\n";
    llvm::outs().resetColor();
    dumpGlobals(m);
  
    instrument(m, results);
    //dumpAnnotatedCFG(entryPoint, {nullptr, nullptr}, results);
  }
  //dumpGlobals(m);
}


bool
BuildPromiseTreePass::runOnModule(llvm::Module& m) {
  initializeGlobals(m);
  auto isInterprocedural = true; // false for turning off interprocedural
  if (isInterprocedural) {
    runAnalysisFor(m, {"main"}, isInterprocedural);
  } else {
    for (auto& function : m) {
      if (function.isDeclaration()) {
        continue;
      }
      llvm::errs() << "\nRunning For " << function.getName();
      runAnalysisFor(m, {function.getName()}, isInterprocedural);
    } //TODO: Fix event_warn
    //runAnalysisFor(m, {"event_warn"}, isInterprocedural);
  }

  svfResults->releaseAndersenWaveDiffWithType();

  //auto getLocationStates = [](llvm::Function* function, auto functionResults) {
  //  auto* location = &*(function->getEntryBlock().getFirstInsertionPt());
  //  auto state = functionResults[location][nullptr];
  //  return std::make_pair(location, state);
  //};

  //auto getParent = [](auto& context) -> llvm::Instruction* {
  //  llvm::Instruction* parent = nullptr;
  //  for (auto* function : context) {
  //    parent = function == nullptr ? function : nullptr;
  //  }
  //  return parent;
  //};

  //using ResolverQueue = std::deque<lowering::LocationState>;
  //ResolverQueue resolverQueue;
  //for (auto& [context, contextResults] : results) {
  //  for (auto& [function, functionResults] : contextResults) {
  //    auto [location, state] = getLocationStates(function, functionResults);
  //    llvm::Instruction* parent = getParent(context);
  //    resolverQueue.emplace_back(
  //        lowering::LocationState{.parentCallSite = parent,
  //                                .callee         = function,
  //                                .location       = location,
  //                                .state          = state,
  //                                .context        = context});
  //  }
  //}
  //lowering::InstructionResolver resolver{m, generator.get()};
  //for (auto& locationState : resolverQueue) {
  //  resolver(locationState);
  //}


  //for (auto& [context, contextResults] : results) {
  //  for (auto& [function, functionResults] : contextResults) {
  //    auto locationStates = getLocationStates(function, functionResults);
  //    for (auto& [loc, state] : locationStates) {
  //      if (state.isVacuouslyTrue() || state.isEmpty()) {
  //        continue;
  //      }
  //      printer->insertIR(loc, context, state);
  //    }
  //  }
  //}

  // auto ec = std::error_code{};
  // auto fileOuts =
  //     llvm::raw_fd_ostream{"/home/shreeasish/pledgerize-reboot/hoisting/"
  //                          "build-dataflow/instrumented/fileStream",
  //                          ec};
  // llvm::errs() << "\n\nRunning Verifier\n";
  // if (!llvm::verifyModule(m, &llvm::errs())) {
  //   llvm::WriteBitcodeToFile(m, fileOuts);
  // }
  return false;
}

void
BuildPromiseTreePass::getAnalysisUsage(llvm::AnalysisUsage &info) const {
  //info.setPreservesAll();
  info.addRequired<DominatorTreeWrapperPass>();
  info.addRequired<AAResultsWrapperPass>();
  info.addRequired<LoopInfoWrapperPass>();
  //info.addRequired<AliasAnalysis>();
  //info.addRequired<PostDominatorTreeWrapperPass>();
}


static void
instrumentPromiseTree(llvm::Module& m) {
  //llvm::DebugFlag = true;
  legacy::PassManager pm;
  pm.add(createGlobalsAAWrapperPass());
  pm.add(createSCEVAAWrapperPass());
  pm.add(createScopedNoAliasAAWrapperPass());
  pm.add(createCFLSteensAAWrapperPass());
  pm.add(createTypeBasedAAWrapperPass());
  pm.add(new llvm::LoopInfoWrapperPass());
  //pm.add(createPostDomTree());
  pm.add(new MemorySSAWrapperPass());
  pm.add(new DominatorTreeWrapperPass());
  pm.add(new AAResultsWrapperPass());
  pm.add(new BuildPromiseTreePass());
  pm.run(m);
}


int
main(int argc, char** argv) {
  // This boilerplate provides convenient stack traces and clean LLVM exit
  // handling. It also initializes the built in support for convenient
  // command line option handling.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  llvm::PrettyStackTraceProgram X(argc, argv);
  llvm_shutdown_obj shutdown;
  //cl::HideUnrelatedOptions(futureFunctionsCategory);
  cl::ParseCommandLineOptions(argc, argv);

  // Construct an IR file from the filename passed on the command line.
  SMDiagnostic err;
  LLVMContext context;
  unique_ptr<Module> module = parseIRFile(inPath.getValue(), err, context);

  if (!module.get()) {
    errs() << "Error reading bitcode file: " << inPath << "\n";
    err.print(argv[0], errs());
    return -1;
  }

  instrumentPromiseTree(*module);
  return 0;
}
