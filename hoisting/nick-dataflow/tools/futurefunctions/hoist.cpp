#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SparseBitVector.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"


#include <bitset>
#include <memory>
#include <string>
#include <iostream>
#include <variant>

#include "DataflowAnalysis.h"
#include "FutureFunctions.h"
#include "IndirectCallResolver.h"
#include "TaintAnalysis.h"

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
                              

//LibC map here
std::unordered_map<std::string, Handler> libCHandlers;


static const llvm::Function *
getCalledFunction(const llvm::CallSite cs) {
  if (!cs.getInstruction()) {
    return nullptr;
  }

  const llvm::Value *called = cs.getCalledValue()->stripPointerCasts();

  if (called->getName().contains("llvm.dbg")) {
    return nullptr;
  }

  return llvm::dyn_cast<llvm::Function>(called);
}


static void
setRequiredPrivileges(FunctionsValue& requiredPrivileges, llvm::CallSite cs, const Context& context) { 
  
  auto functionName = getCalledFunction(cs)->getName().str();

  auto found = libCHandlers.find(functionName);
  
  if(found != libCHandlers.end()){   
    //Use the context of the current callsite
    auto promisesBitset = found->second.getPromisesBitset(cs, context);
    requiredPrivileges |= promisesBitset;
    // requiredPrivileges |= 0;
  }
}


class FunctionsMeet : public analysis::Meet<FunctionsValue, FunctionsMeet> {
public:
  FunctionsValue
  meetPair(FunctionsValue& s1, FunctionsValue& s2) const {
    return s1 | s2;
  }
};


class FunctionsTransfer {
  
public:
  void
  operator()(llvm::Value& v, FunctionsState& state, const Context& context) {
    
    const CallSite cs{&v};
    const auto* fun = getCalledFunction(cs);

    // Pretend that indirect calls & non calls don't exist for this analysis
    if (!fun) {
      return;
    }

    // FunctionsValue requiredPrivileges{};
    setRequiredPrivileges(state[nullptr], cs, context);
 
    auto [found, inserted] = functionIDs.insert({fun, functions.size()});
    if (inserted) {
      functions.push_back(fun);
    }

    // state[nullptr].set(found->second);
  }
};


template <typename OutIterator>
static void
collectCritical(llvm::Function& fun, FunctionsResult& states, OutIterator critical) {
  auto getStateAt = [&states] (llvm::BasicBlock* bb) {
    return states[&*bb->begin()][nullptr];
  };
  for (auto& bb : fun) {
    auto* terminator = bb.getTerminator();
    if (terminator->getNumSuccessors() < 2) {
      continue;
    }
    auto successors = terminator->successors();
    auto promises = getStateAt(*successors.begin());
    if (std::any_of(++successors.begin(), successors.end(),
        [&getStateAt,promises](auto* bb) { return getStateAt(bb) != promises; })) {
      *critical++ = terminator;
    }
  }
}


static void
printLineNumber(llvm::raw_ostream& out, llvm::Instruction& inst) {
  if (const llvm::DILocation* debugLoc = inst.getDebugLoc()) {
    out << "At " << debugLoc->getFilename()
        << " line " << debugLoc->getLine()
        << ":\n";
  } else {
    out << "At an unknown location:\n";
  }
}


static void
printFollowers(llvm::ArrayRef<std::pair<llvm::Instruction*, FunctionsValue>> followers) {
  for (auto& [callsite, after] : followers) {
    llvm::outs().changeColor(raw_ostream::Colors::RED);
    printLineNumber(llvm::outs(), *callsite);

    auto* called = getCalledFunction(llvm::CallSite{callsite});
    llvm::outs().changeColor(raw_ostream::Colors::YELLOW);
    llvm::outs() << "After call to \"" << called->getName() << "\" ";
    
    
    // for (auto id : after) {
    //   llvm::outs() << " " << functions[id]->getName();
    // }

    for (int i = 0; i < COUNT ; i++) {
      if(after[i]){
        llvm::outs() << PromiseNames[i] << " ";
      }

    }


    llvm::outs() << "\n";
  }

  if (followers.empty()) {
    llvm::outs().changeColor(raw_ostream::Colors::GREEN);
    llvm::outs() << "No followers collected\n";
  }
  llvm::outs().resetColor();
}


struct ConditionSummary {
  struct Edge {
    size_t position;
    ConditionSummary* child;
  };
  struct Branch {
    Instruction* condition;
    llvm::SmallVector<Edge,2> edges;
  };

  // Get around some of the current bugs in variant by rolling our own.
  struct CallOrBranch {
    CallOrBranch(Instruction* i)
      : isBranch{false},
        as{i}
        { }
    CallOrBranch(Branch b)
      : isBranch{true},
        as{std::move(b)}
        { }
    ~CallOrBranch() {
      if (isBranch) {
        as.branch.~Branch();
      }
    }
    CallOrBranch(const CallOrBranch& other) {
      isBranch = other.isBranch;
      if (other.isBranch) {
        new (&as.branch) Branch(other.as.branch);
      } else {
        as.call = other.as.call;
      }
    }
    CallOrBranch&
    operator=(const CallOrBranch& other) {
      // Because `as` is a union, it cannot simply be assigned to. The previous
      // contents may have been a different type. Instead, the existing contents
      // must be destroyed and the new contents must be initialized in place.
      if (this != &other) {
        this->~CallOrBranch();
        isBranch = other.isBranch;
        if (other.isBranch) {
          new (&as.branch) Branch(other.as.branch);
        } else {
          as.call = other.as.call;
        }
      }
      return *this;
    }

    Instruction*&
    operator=(Instruction* i) {
      isBranch = false;
      as.call = i;
      return as.call;
    }

    Branch&
    operator=(Branch b) {
      isBranch = true;
      as.branch = std::move(b);
      return as.branch;
    }

    bool isBranch;
    union Union {
      Branch branch;
      Instruction* call;
      Union()
        : call{nullptr}
          { }
      Union(Branch b)
        : branch{std::move(b)}
          { }
      Union(Instruction* i)
        : call{i}
          { }
      ~Union() {
        // variant member destruction is handled by the enclosing class;
      }
    } as;
  };

  llvm::SmallVector<CallOrBranch,8> events;
};


class ConditionTree {
public:
  ConditionTree(llvm::Module& m)
    : resolver{m},
      storage{},
      summaries{}
      { }

  template<class Pred, class GetPosts>
  static ConditionTree build(llvm::Module& m, Pred& shouldGuard, GetPosts& getPosts);

  ConditionSummary*
  getFunctionSummary(llvm::Function* function) {
    return summaries[function];
  }

  void dump(llvm::raw_ostream& out) const;

  llvm::SmallVector<ConditionSummary::Branch,8>
  getChildren(ConditionSummary* summary) {
    llvm::SmallVector<ConditionSummary::Branch,8> children;
    children.reserve(summary->events.size());
    for (auto& event : summary->events) {
      if (event.isBranch) {
        children.push_back(event.as.branch);

      } else {
        llvm::CallSite cs(event.as.call);
        if (auto* f = getCalledFunction(cs);
            f && !f->isDeclaration() && summaries[f]) {
          children.push_back({event.as.call, {{0, summaries[f]}}});

        } else if (cs.isIndirectCall()) {
          auto targets = resolver.getPossibleTargets(cs);
          llvm::SmallVector<ConditionSummary::Edge, 2> edges;
          edges.reserve(targets.size());
          for (auto* target : targets) {
            if (summaries[target]) {
              edges.push_back({ edges.size(), summaries[target]});
            }
          }
          children.push_back({event.as.call, std::move(edges)});
        }
      }
    }
    return children;
  }

  IndirectCallResolver resolver;
  std::deque<ConditionSummary> storage;
  llvm::DenseMap<const llvm::Function*,ConditionSummary*> summaries;
};


template<class Pred, class GetPosts>
ConditionTree
ConditionTree::build(llvm::Module& m, Pred& shouldGuard, GetPosts& getPosts) {
  using CallOrBranch = ConditionSummary::CallOrBranch;
  ConditionTree tree{m};
  for (auto& fun : m) {
    if (fun.isDeclaration()) {
      continue;
    }

    auto& posts = getPosts(fun);
    llvm::DenseSet<llvm::BasicBlock*> seen;
    auto extractSummaries =
      [&posts, &tree, &shouldGuard, &seen] (auto* start, const auto* post, auto& extractSummaries)
        -> ConditionSummary* {
      if (seen.count(start)) {
        return nullptr;
      }
      seen.insert(start);

      llvm::SmallVector<CallOrBranch,8> events;

      llvm::BasicBlock* bb = start;
      while (bb && bb != post) {
        llvm::SmallVector<llvm::Instruction*,4> insts;
        insts.resize(bb->size());
        std::transform(bb->begin(), bb->end(), insts.begin(), [] (auto& i) { return &i; });
        std::copy_if(insts.begin(), insts.end(), std::back_inserter(events),
          shouldGuard);

        unsigned numSuccessors = bb->getTerminator()->getNumSuccessors();
        if (numSuccessors == 1) {
          bb = *llvm::succ_begin(bb);

        } else if (numSuccessors > 1) {
          auto* newPost = posts[bb]->getIDom()->getBlock();
          auto* terminator = bb->getTerminator();
          ConditionSummary::Branch newBranch{terminator, {}};

          for (size_t position = 0, numSuccessors = terminator->getNumSuccessors();
              position < numSuccessors; ++position) {
            auto* successor = terminator->getSuccessor(position);
            auto* summary = extractSummaries(successor, newPost, extractSummaries);
            if (summary) {
              newBranch.edges.push_back({ position, summary});
            }
          }
          bb = newPost;
          if (!newBranch.edges.empty()) {
            events.push_back(ConditionSummary::CallOrBranch{newBranch});
          }

        } else {
          break;
        }
        // TODO: It used to be necessary to special case infinite loops. Is it still?
      }

      if (!events.empty()) {
        ConditionSummary summary{std::move(events)};
        tree.storage.push_back(summary);
        return &tree.storage.back();
      }

      seen.erase(bb);
      return nullptr;
    };
    tree.summaries[&fun] = extractSummaries(&fun.getEntryBlock(),
                                            static_cast<llvm::BasicBlock*>(nullptr),
                                            extractSummaries);
  }
  return tree;
}


void
ConditionTree::dump(llvm::raw_ostream& out) const {
  out << "digraph {\n";
  auto printInst = [&out] (llvm::Instruction* i) {
    if (i) {
      std::string str;
      llvm::raw_string_ostream stringout{str};
      printLineNumber(stringout, *i);
      stringout << *i;
      stringout.flush();
      out.write_escaped(stringout.str());
    } else {
      out << "NONE";
    }
  };
  auto printLabel = [&out,&printInst] (const ConditionSummary::Branch& branch) {
    for (auto& edge : branch.edges) {
      out << "    n" << edge.child
          << "[label=\"" << edge.position << "-";
      printInst(branch.condition);
      out << "\"];\n";
    }
  };
  auto atNode = [&out,&printLabel,&printInst] (ConditionSummary* summary, auto& atNode) -> void {
    for (auto& event : summary->events) {
      if (!event.isBranch) {
        // The event was a call in the region
        auto* call = event.as.call;
        out << "    n" << summary << " -> n" << call << ";\n";
        out << "    n" << call << " [shape=\"box\",label=\"";
        printInst(call);
        out << "\"];\n";

      } else {
        // The event was a branch to children
        for (auto& edge : event.as.branch.edges) {
          out << "    n" << summary << " -> n" << edge.child << ";\n";
        }
      }
    }

    for (auto& event : summary->events) {
      if (event.isBranch) {
        printLabel(event.as.branch);
        // The event was a branch to children
        for (auto& edge : event.as.branch.edges) {
          atNode(edge.child, atNode);
        }        
      }
    }
  };

  for (auto& kv : summaries) {
    out << "  " << kv.first->getName() << " -> n" << kv.second << ";\n";
    if (kv.second) {
      printLabel(ConditionSummary::Branch{nullptr, {{0, kv.second}}});
      atNode(kv.second, atNode);
    }
  }
  out << "}\n";
}


template<class CanHoist>
static void
buildHoistedConditions(ConditionTree& tree,
                       llvm::Function* entry,
                       llvm::DenseMap<llvm::Value*, FunctionsValue> promises,
                       CanHoist& canHoist) {
  llvm::DenseMap<llvm::Value*,bool> hoistable;
  auto isHoistable = [&hoistable,&canHoist] (llvm::Value* condition) {
    if (!condition) {
      return true;
    } else if (auto found = hoistable.find(condition); found != hoistable.end()) {
      return found->second;
    } else {
      return hoistable[condition] = canHoist(condition);
    }
    return true;
  };

  auto prune = [&isHoistable] (ConditionSummary* summary, auto& prune) -> void {
    for (auto& event : summary->events) {
      if (event.isBranch) {
        for (auto& edge : event.as.branch.edges) {
          prune(edge.child, prune);          
        }
      }
    }
    for (auto& event : summary->events) {
      if (event.isBranch && !isHoistable(event.as.branch.condition)) {
        event.as.branch.condition = nullptr;
      }
    }
  };
  for (auto& [function, summary] : tree.summaries) {
    if (summary) {
      prune(summary, prune);
    }
  }

}


class BuildPromiseTreePass : public llvm::ModulePass {
public:
  BuildPromiseTreePass()
    : llvm::ModulePass{ID}
      { }

  bool runOnModule(llvm::Module& m) override;
  void getAnalysisUsage(llvm::AnalysisUsage &info) const override;

private:
  static char ID;
};
char BuildPromiseTreePass::ID = 0;

bool
BuildPromiseTreePass::runOnModule(llvm::Module& m) {
  auto* mainFunction = m.getFunction("main");
  if (!mainFunction) {
    llvm::report_fatal_error("Unable to find main function.");
  }

  using Value    = FunctionsValue;
  using Transfer = FunctionsTransfer;
  using Meet     = FunctionsMeet;
  using Analysis = analysis::DataflowAnalysis<Value, Transfer, Meet, analysis::Backward>;
  Analysis analysis{m, mainFunction};

  // Get all forward analyses here
  auto tmpAnalysisResults = tmpanalysis::gettmpAnalysisResults(m);

  // Get all the libraries here and initialise them with it
  libCHandlers = getLibCHandlerMap(
        tmpAnalysisResults
         );

  auto results = analysis.computeDataflow();

  // TODO: Add pledge dropping along the promise frontier

  llvm::DenseMap<llvm::Value*, FunctionsValue> unifiedResults;
  for (auto& [context, contextResults] : results) {
    for (auto& [function, functionResults] : contextResults) {
      for (auto& bb : *function) {
        for (auto& i : bb) {
          FunctionsState state;
          FunctionsTransfer{}(i, state, context);
          if (state[nullptr].any()) {
            unifiedResults[&i] |= state[nullptr];
          }
        }
      }
      auto* entry = &*function->getEntryBlock().getFirstInsertionPt();
      unifiedResults[function] |= functionResults[entry][nullptr];
    }
  }


  llvm::CallGraph cg{m};
  IndirectCallResolver resolver{m};
  for (auto& f : m) {
    if (!f.isDeclaration()) {
      continue;
    }
    auto* cgNode = cg.getOrInsertFunction(&f);
    for (auto& bb : f) {
      for (auto& i : bb) {
        CallSite cs(&i);
        if (!cs.getInstruction() || !cs.isIndirectCall()) {
          continue;
        }
        auto targets = resolver.getPossibleTargets(cs);
        for (auto* target : targets) {
          auto* targetNode = cg.getOrInsertFunction(target);
          cgNode->addCalledFunction(cs, targetNode);
        }
      }
    }
  }

  llvm::DenseMap<llvm::Function*, bool> usesPromises;
  for (auto* node : llvm::post_order(cg.getExternalCallingNode())) {
    auto* function = node->getFunction();
    if (!function) {
      continue;
    }
    usesPromises[function] =
      std::any_of(node->begin(), node->end(), [&usesPromises] (auto& record) {
      auto found = usesPromises.find(record.second->getFunction());
      return found == usesPromises.end() || found->second;
    });
  }

  auto shouldCapture =
    [&usesPromises, &unifiedResults, &resolver] (llvm::Instruction* i) {
    llvm::CallSite cs{i};
    if (!cs.getInstruction()) {
      return false;
    }
    auto* called     = cs.getCalledValue()->stripPointerCasts();
    auto* asFunction = llvm::dyn_cast<llvm::Function>(called);
    if (!asFunction) {
      auto targets = resolver.getPossibleTargets(cs);
      return std::any_of(targets.begin(), targets.end(),
        [&usesPromises] (auto* f) { return usesPromises[f]; });
    } else {
      return (!asFunction->isDeclaration() && usesPromises[asFunction])
        || unifiedResults[cs.getInstruction()].any();
    }
  };

  auto getPosts = [this] (auto& f) -> llvm::PostDominatorTree& {
    return getAnalysis<PostDominatorTreeWrapperPass>(f).getPostDomTree();
  };

  auto tree = ConditionTree::build(m, shouldCapture, getPosts);

  tree.dump(llvm::outs());

  auto canHoist = [this] (llvm::Value* condition) {
    if (auto* branch = llvm::dyn_cast<BranchInst>(condition)) {
      std::deque<llvm::Value*> toVisit = { branch->getCondition() };
      llvm::DenseSet<llvm::Value*> visited;
      while (!toVisit.empty()) {
        auto* current = toVisit.back();
        toVisit.pop_back();
        if (visited.count(current)) {
          continue;
        }

        bool isSafe = false;
        if (isa<Constant>(current)
            || isa<CmpInst>(current)
            || isa<BinaryOperator>(current)
            || isa<UnaryInstruction>(current)
            || isa<Argument>(current)) {
          if (auto* user = dyn_cast<User>(current)) {
            toVisit.insert(toVisit.end(), user->op_begin(), user->op_end());
          }
          isSafe = true;

        } else if (auto* load = dyn_cast<LoadInst>(current)) {
          auto* pointer = load->getPointerOperand();
          if (auto* gep = dyn_cast<GetElementPtrInst>(pointer)) {
            pointer = gep->getPointerOperand();
          } else if (auto* ce = dyn_cast<ConstantExpr>(pointer);
                     ce && ce->getOpcode() == Instruction::GetElementPtr) {
            pointer = ce->getOperand(0);
          }
          isSafe = isa<GlobalVariable>(pointer);
        }

        if (!isSafe) {
          return false;
        }

        visited.insert(current);
      }

      return true;
    } else {
      return false;
    }
  };
  buildHoistedConditions(tree, mainFunction, unifiedResults, canHoist);

  tree.dump(llvm::outs());

  return false;
}

void
BuildPromiseTreePass::getAnalysisUsage(llvm::AnalysisUsage &info) const {
  info.addRequired<PostDominatorTreeWrapperPass>();
  info.addRequired<LoopInfoWrapperPass>();
}


static void
instrumentPromiseTree(llvm::Module& m) {
  legacy::PassManager pm;
  pm.add(llvm::createPostDomTree());
  pm.add(new llvm::LoopInfoWrapperPass());
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
  cl::HideUnrelatedOptions(futureFunctionsCategory);
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

