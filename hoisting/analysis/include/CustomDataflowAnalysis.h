#ifndef DATAFLOW_ANALYSIS_H
#define DATAFLOW_ANALYSIS_H

#include <algorithm>
#include <deque>
#include <numeric>

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"

#include "PromiseDeclarations.h"
#include "WPA/Andersen.h"

namespace llvm {

template<unsigned long Size>
struct DenseMapInfo<std::array<llvm::Instruction*, Size>> {
  using Context = std::array<llvm::Instruction*, Size>;
  static inline Context
  getEmptyKey() {
    Context c;
    std::fill(c.begin(), c.end(),
      llvm::DenseMapInfo<llvm::Instruction*>::getEmptyKey());
    return c;
  }
  static inline Context
  getTombstoneKey() {
    Context c;
    std::fill(c.begin(), c.end(),
      llvm::DenseMapInfo<llvm::Instruction*>::getTombstoneKey());
    return c;
  }
  static unsigned
  getHashValue(const Context& c) {
    return llvm::hash_combine_range(c.begin(), c.end());
  }
  static bool
  isEqual(const Context& lhs, const Context& rhs) {
    return lhs == rhs;
  }
};


}


namespace analysis {

using SVFAnalysis = AndersenWaveDiffWithType;
auto getSVFResults = [] (auto& m){
  return SVFAnalysis::createAndersenWaveDiffWithType(m);
};

template<typename T>
class WorkList {
public:
  template<typename IterTy>
  WorkList(IterTy i, IterTy e)
    : inList{},
      work{i, e} {
    inList.insert(i,e);
  }

  WorkList()
    : inList{},
      work{}
      { }

  bool empty() const { return work.empty(); }

  bool contains(T elt) const { return inList.count(elt); }

  void
  add(T elt) {
    if (!inList.count(elt)) {
      work.push_back(elt);
    }
  }

  T
  take() {
    T front = work.front();
    work.pop_front();
    inList.erase(front);
    return front;
  }

private:
  llvm::DenseSet<T> inList;
  std::deque<T> work;
};

using BasicBlockWorklist = WorkList<llvm::BasicBlock*>;

// The dataflow analysis computes three different granularities of results.
// An AbstractValue represents information in the abstract domain for a single
// LLVM Value. An AbstractState is the abstract representation of all values
// relevent to a particular point of the analysis. A DataflowResult contains
// the abstract states before and after each instruction in a function. The
// incoming state for one instruction is the outgoing state from the previous
// instruction. For the first instruction in a BasicBlock, the incoming state is
// keyed upon the BasicBlock itself.
//
// Note: In all cases, the AbstractValue should have a no argument constructor
// that builds constructs the initial value within the abstract domain.

template <typename AbstractValue>
using AbstractState = llvm::DenseMap<llvm::Value*,AbstractValue>;


template <typename AbstractValue>
using DataflowResult =
  llvm::DenseMap<llvm::Value*, AbstractState<AbstractValue>>;

// ska: Abstract State Comparison
template <typename AbstractValue>
bool
operator==(const AbstractState<AbstractValue>& s1,
           const AbstractState<AbstractValue>& s2) {
  if (s1.size() != s2.size()) {
    return false;
  }
  return std::all_of(s1.begin(), s1.end(),
    [&s2] (auto &kvPair) {
      auto found = s2.find(kvPair.first);
      return found != s2.end() && found->second == kvPair.second;
    });
}


template <typename AbstractValue>
AbstractState<AbstractValue>&
getIncomingState(DataflowResult<AbstractValue>& result, llvm::Instruction& i) {
  auto* bb = i.getParent();
  auto* key = (&bb->front() == &i)
    ? static_cast<llvm::Value*>(bb)
    : static_cast<llvm::Value*>(&*--llvm::BasicBlock::iterator{i});
  return result[key];
}


// NOTE: This class is not intended to be used. It is only intended to
// to document the structure of a Transfer policy object as used by the
// DataflowAnalysis class. For a specific analysis, you should implement
// a class with the same interface.
template <typename AbstractValue>
class Transfer {
public:
  template <typename Context>
  void
  operator()(llvm::Value& v, AbstractState<AbstractValue>& s, const Context& ) {
    llvm_unreachable("unimplemented transfer");
  }
};


// This class can be extended with a concrete implementation of the meet
// operator for two elements of the abstract domain. Implementing the
// `meetPair()` method in the subclass will enable it to be used within the
// general meet operator because of the curiously recurring template pattern.
template <typename AbstractValue, typename SubClass>
class Meet {
public:
  AbstractValue
  operator()(llvm::ArrayRef<AbstractValue> values) {
    return std::accumulate(values.begin(), values.end(),
      AbstractValue(),
      [this] (auto v1, auto v2) {
        return this->asSubClass().meetPair(v1, v2);
      });
  }

  AbstractValue
  meetPair(AbstractValue& v1, AbstractValue& v2) const {
    llvm_unreachable("unimplemented meet");
  }

  void print(llvm::raw_ostream& out, AbstractValue& value) { }
  void printState(llvm::raw_ostream& out, AbstractState<AbstractValue>& state) {
    out << "DUMP ";
    for (auto& kvPair : state) {
      this->asSubClass().print(out, kvPair.second);
    }
    out << "\n";
  }

private:
  SubClass& asSubClass() { return static_cast<SubClass&>(*this); };
};


class Forward {
public:
  static auto getInstructions(llvm::BasicBlock& bb) {
    return llvm::iterator_range<decltype(bb.begin())>(bb);
  };
  static auto getFunctionTraversal(llvm::Function& f) {
    return llvm::ReversePostOrderTraversal<llvm::Function*>(&f);
  }
  static auto* getEntryKey(llvm::BasicBlock& bb) {
    return &bb;
  }
  static auto* getExitKey(llvm::BasicBlock& bb) {
    return bb.getTerminator();
  }
  static llvm::Value* getFunctionValueKey(llvm::BasicBlock& bb) {
    if (auto* ret = llvm::dyn_cast<llvm::ReturnInst>(bb.getTerminator())) {
      return ret->getReturnValue();
    }
    return nullptr;
  }
  static auto getSuccessors(llvm::BasicBlock& bb) {
    return llvm::successors(&bb);
  }
  static auto getPredecessors(llvm::BasicBlock& bb) {
    return llvm::predecessors(&bb);
  }
  static bool shouldMeetPHI() { return true; }
  template <class State, class Transfer, class Meet, class Context>
  static bool prepareSummaryState(llvm::CallSite cs,
                                  llvm::Function* callee,
                                  State& state,
                                  State& summaryState,
                                  Transfer& transfer,
                                  Meet& meet,
                                  const Context& context) {
    unsigned index = 0;
    bool needsUpdate = false;
    for (auto& functionArg : callee->args()) {
      auto* passedConcrete = cs.getArgument(index);
      auto passedAbstract = state.find(passedConcrete);
      if (passedAbstract == state.end()) {
        transfer(*passedConcrete, state, context);
        passedAbstract = state.find(passedConcrete);
      }
      auto& arg     = summaryState[&functionArg];
      auto newState = meet({passedAbstract->second, arg});
      needsUpdate |= !(newState == arg);
      arg = newState;
      ++index;
    }
    return needsUpdate;
  }
};


class Backward {
public:
  static auto getInstructions(llvm::BasicBlock& bb) {
    return llvm::reverse(bb);
  };
  static auto getFunctionTraversal(llvm::Function& f) {
    return llvm::post_order<llvm::Function*>(&f);
  }
  static auto* getEntryKey(llvm::BasicBlock& bb) {
    return &bb;
  }
  static auto* getExitKey(llvm::BasicBlock& bb) {
    return &*bb.begin();
  }
  static llvm::Value* getFunctionValueKey(llvm::BasicBlock& bb) {
    return (&bb == &bb.getParent()->getEntryBlock()) ? getExitKey(bb) : nullptr;
  }
  static auto getSuccessors(llvm::BasicBlock& bb) -> std::vector<llvm::BasicBlock*> {
    constexpr auto traverseBackEdges = true;
    auto preds = llvm::predecessors(&bb);
    if constexpr (traverseBackEdges) {
      return {preds.begin(), preds.end()};  
    } // Early return if backedge traversal is enabled

    auto* function = bb.getParent();
    auto traversalOrder =
        getFunctionTraversal(*function);  // Should be cached
                                          // Gets it in Post Order (bottom up)
    llvm::DenseSet<llvm::BasicBlock*> predSet;
    for (auto* toVisit : preds) {             // Get predecessor as set toVisit
      predSet.insert(toVisit);                // Iterate function bbs in post order
    }                                         // If current bb found set flag, foundCurrent
                                              // Remove anything after it from toVisit
    bool foundCurrent = false;                // Could be faster if there were an ordering 
    for (auto* orderedBB : traversalOrder) {  // between bbs O(n) -> n = bbs in function
      foundCurrent |= &bb == orderedBB;
      if (!foundCurrent) {
        predSet.erase(orderedBB);
        //llvm::errs() << "\n Removing Basic Block\n" << bb;
      } else {
        break; // Since the rest of BBs in the predecessor set will become no-ops
      }
    }
    //std::vector<llvm::BasicBlock*> withNoBackEdges{predSet.begin(), predSet.end()};
    //return llvm::iterator_range{withNoBackEdges.begin(), withNoBackEdges.end()};
    return {predSet.begin(), predSet.end()};
  }
  static auto getPredecessors(llvm::BasicBlock& bb) {
    return llvm::successors(&bb);
  }
  static bool shouldMeetPHI() { return false; }
  template <class State, class Transfer, class Meet, class Context>
  static bool prepareSummaryState(llvm::CallSite cs,
                                  llvm::Function* callee,
                                  State& state,
                                  State& summaryState,
                                  Transfer& transfer,
                                  Meet& meet,
                                  const Context& context) {
     // TODO: This would be better if it checked whether the new state at this
     // function different from the old.
     bool needsUpdate = false;
     //auto* passedConcrete = cs.getInstruction();
     auto& passedAbstract = state.FindAndConstruct(nullptr);
     //llvm::errs() << "\nPassed Abstract";
     //passedAbstract.second[PLEDGE_CPATH].print(llvm::errs());
     /*Marshalling abstract state into functions*/
     for (auto& bb : *callee) {
       if (auto* ret = llvm::dyn_cast<llvm::ReturnInst>(bb.getTerminator());
           ret /*&& ret->getReturnValue()*/ ) {
         // Pick up returns with return values
         // auto& retState = summaryState[ret->getReturnValue()];
         // auto  newState = meet({passedAbstract, retState});
         auto& retState = summaryState[ret];
         //llvm::errs() << "\nPassed retstate before merging";
         //retState[PLEDGE_CPATH].print(llvm::errs());
        
         auto newState = meet({passedAbstract.second, retState});
         needsUpdate |= !(newState == retState);
         {
           //llvm::errs() << "\nChecking Ret State at " << *cs.getInstruction();
           //llvm::errs() << " parent" << cs.getInstruction()->getParent()->getParent()->getName();
           //llvm::errs() << "\nretState";
           //retState[PLEDGE_CPATH].print(llvm::errs());

           //llvm::errs() << "\nnewState";
           //newState[PLEDGE_CPATH].print(llvm::errs());
           //llvm::errs() << "\n----end check----";
           //if (needsUpdate) {
           //  llvm::errs() << " yes update";
           //} else {
           //  llvm::errs() << " no update";
           //}
         }
         retState = newState;
       }
     }
   // 
   // /*The top inst should hold final state computed for the function*/
   // llvm::Instruction* topInst = *(callee->getEntryBlock().begin());
   // /* The function summary should finally be kept at summaryState[nullptr]
   //  * i.e. allResults[context][callee][function][nullptr] */
   // auto newState = meet({summaryState[nullptr], calledState[topInst][nullptr]});
   // needsUpdate |= !(newState == summaryState[nullptr]);
   // summaryState[nullptr] = meet({summaryState[nullptr], calledState[topInst][nullptr]});
    return needsUpdate;
  }
};


llvm::Function*
getCalledFunction(llvm::CallSite cs) {
  auto* calledValue = cs.getCalledValue()->stripPointerCasts();
  return llvm::dyn_cast<llvm::Function>(calledValue);
}

bool
isAnalyzableCall(llvm::CallSite cs) {
  if (!cs.getInstruction()) {
    return false;
  }
  auto* called = getCalledFunction(cs);
  return called && !called->isDeclaration();
}

bool
isExternalCall(llvm::CallSite cs) {
  if (!cs.getInstruction()) {
    return false;
  }
  auto* called = getCalledFunction(cs);
  return called 
    && called->isDeclaration()
    && !called->getName().startswith("llvm");
}


template <typename AbstractValue,
          typename Transfer,
          typename Meet,
          typename EdgeTransformer,
          typename Debugger,
          typename Direction=Forward,
          unsigned long ContextSize=2ul>
class DataflowAnalysis {
public:
  using State   = AbstractState<AbstractValue>;
  using Context = std::array<llvm::Instruction*, ContextSize>;

  using FunctionResults = DataflowResult<AbstractValue>;
  using ContextFunction = std::pair<Context, llvm::Function*>;
  using ContextResults  = llvm::DenseMap<llvm::Function*, FunctionResults>;
  using ContextWorklist = WorkList<ContextFunction>;

  using ContextMapInfo =
    llvm::DenseMapInfo<std::array<llvm::Instruction*, ContextSize>>;
  using AllResults = llvm::DenseMap<Context, ContextResults, ContextMapInfo>;
  
  //AndersenWaveDiffWithType* svfResults;
  SVFAnalysis* svfResults;
  llvm::Module& module;

  DataflowAnalysis(llvm::Module& m, llvm::ArrayRef<llvm::Function*> entryPoints,
                   SVFAnalysis* svf, bool isInterprocedural) 
    : svfResults{svf}, module{m} {
    transfer.isInterprocedural = isInterprocedural;
    for (auto* entry : entryPoints) {
      contextWork.add({Context{}, entry});
    }
  }


  // computeDataflow collects the dataflow facts for all instructions
  // in the program reachable from the entryPoints passed to the constructor.
  AllResults
  computeDataflow() {
    while (!contextWork.empty()) {
      auto [context, function] = contextWork.take();
      computeDataflow(*function, context);
    }

    return allResults;
  }

  struct StateFunction {
    AbstractValue state;
    llvm::Function* indTarget;
    StateFunction(AbstractValue s, llvm::Function* f)
      : state{s}, indTarget{f} {}
  };
  // computeDataflow collects the dataflowfacts for all instructions
  // within Function f with the associated execution context. Functions whose
  // results are required for the analysis of f will be transitively analyzed.
  DataflowResult<AbstractValue>
  computeDataflow(llvm::Function& f, const Context& context) {
    active.insert({context, &f});

    // First compute the initial outgoing state of all instructions
    FunctionResults results = allResults.FindAndConstruct(context).second
                                        .FindAndConstruct(&f).second;
    if (results.find(getSummaryKey(f)) == results.end()) {
      for (auto& i : llvm::instructions(f)) {
        results.FindAndConstruct(&i);
      }
    }

    // Add all blocks to the worklist in topological order for efficiency
    auto traversal = Direction::getFunctionTraversal(f);
    BasicBlockWorklist work(traversal.begin(), traversal.end());

    while (!work.empty()) {
      auto* bb = work.take();

      // Save a copy of the outgoing abstract state to check for changes.
      const auto oldExitState   = results[Direction::getExitKey(*bb)];
      const auto& oldEntryState = results[Direction::getEntryKey(*bb)];

      // Merge the state coming in from all predecessors including the function
      // summary (which contains arguments, etc.)
      auto state = mergeStateFromPredecessors(bb, results);
      mergeInState(state, results[getSummaryKey(f)]);

      // If we have already processed the block and no changes have been made to
      // the abstract input, we can skip processing the block. Otherwise, save
      // the new entry state and proceed processing this block.
      if (state == oldEntryState && !state.empty()) {
        continue;
      }
      results[bb] = state;

      //auto getSize = [](auto& state) {
      //  size_t size = 0;
      //  for (auto& disjunctionMap : state) {
      //    size = disjunctionMap.conjunctCount();
      //  }
      //  return size;
      //};

      auto getNonConst = [this](const llvm::Function* cfunc) {
        return this->module.getFunction(cfunc->getName());
      };
      
      auto isIndirectCall = [this](const llvm::CallSite& cs) {
        auto* PAG = this->svfResults->getPAG();
        return PAG->isIndirectCallSites(cs);
      };


      // Propagate through all instructions in the block
      for (auto& i : Direction::getInstructions(*bb)) {
        [&context](auto* instruction) -> void {
          // auto fname = bb->getParent()->getName();
          auto fname = instruction->getParent()->getParent()->getName();
          llvm::outs() << "\nWorking on :" << *instruction
                       << " @inFunction: " << fname;
          llvm::outs().changeColor(llvm::raw_ostream::Colors::YELLOW);
          llvm::outs() << "\nWith Context :";
          for (auto* func : context) {
            if (func) {
              llvm::outs() << *func << "->";
            } else {
              llvm::outs() << "nullptr"
                           << "->";
            }
          }
          llvm::outs().resetColor();
          return;
        }(&i);
        // const auto& size = getSize(state[nullptr]); //
        // state[nullptr].conjunctCount();
        llvm::CallSite cs(&i);
        if (isAnalyzableCall(cs) && transfer.isInterprocedural) {
          //auto fname = getCalledFunction(cs)->getName();
          //llvm::errs() << "\nFound internal call" << fname;

          //llvm::errs() << "\nSending state in as";
          //state[nullptr][PLEDGE_CPATH].print(llvm::errs());
          analyzeCall(cs, state, context);
        } 
        else if (isIndirectCall(cs) && svfResults->hasIndCSCallees(cs)
            && transfer.isInterprocedural) {
          //llvm::errs() << "\nFound Pointer Call \t ";
          //llvm::errs() << i << "\n";
          //llvm::errs() << "Parent " << i.getParent()->getParent()->getName()
          //             << "\n";

          //llvm::errs() << "\nSending state in as";
          //state[nullptr][PLEDGE_CPATH].print(llvm::errs());

          std::vector<StateFunction> rewritten;
          for (auto* constIndTarget : svfResults->getIndCSCallees(cs)) {
            auto stateCopy = state;

            if (!constIndTarget->isDeclaration()) { 
              //llvm::errs() << "\nAdded Internal Analyzable Call "
              //             << constIndTarget->getName();
              analyzeCall(cs, stateCopy, context, getNonConst(constIndTarget));
              //llvm::errs() << "\n After analyzeCall For ind targetName " 
              //             << constIndTarget->getName();
              //stateCopy[cs.getInstruction()][promise].print(llvm::errs());
            } // declonly functions will be handled as havocs

            auto& stateAndTarget = 
              rewritten.emplace_back(stateCopy[cs.getInstruction()], getNonConst(constIndTarget));
            auto& absValueCopy = stateAndTarget.state;
            auto& indTarget = stateAndTarget.indTarget;

            auto* checkLoc = transformer.setLocation(cs.getInstruction());
            absValueCopy = transformer.rewriteArgs(absValueCopy, cs, indTarget);
            transformer.removeLocation(checkLoc);
            //llvm::errs() << "\n\nBefore rewriting args";
            //absValueCopy[PLEDGE_STDIO].print(llvm::errs());

            //llvm::errs() << "\n\nAfter rewriting args";
            //absValueCopy[PLEDGE_STDIO].print(llvm::errs());
            // Reset the state at cs.getInstruction but accept function summaries
            stateCopy[cs.getInstruction()] = state[cs.getInstruction()];
            state = stateCopy;
          }

          // Check for homogeniety
          std::vector<AbstractValue> toMeet;
          bool equal = true;
          auto first = !(rewritten.begin() == rewritten.end()) 
            ? rewritten.begin()->state : state[cs.getInstruction()];
          for (auto& [disjunction, target] : rewritten) {
            equal &= first == disjunction;

            auto* checkLoc = transformer.setLocation(cs.getInstruction());
            auto disj = transformer(disjunction, cs, target);
            transformer.removeLocation(checkLoc);
            toMeet.emplace_back(disj);
          }
          state[cs.getInstruction()] = equal ? first : meet(toMeet);
        } 

        //llvm::errs() << "\nbefore transfer "; 
        //state[nullptr][PLEDGE_STDIO].print(llvm::errs());
        applyTransfer(i, state, context);
        //llvm::errs() << "\nafter transfer "; 
        //state[nullptr][PLEDGE_STDIO].print(llvm::errs());

        results[&i] = state;
        auto snapShot = [&](int promiseNum) {
          static std::vector<std::vector<int>> sizes = {
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000},
              {10000, 8000, 6000, 4000, 2000, 1000}};

          auto from = std::remove_if(
              sizes[promiseNum].begin(),
              sizes[promiseNum].end(),
              [&](auto size) {
                if (Debugger::checkThreshold(
                        state[nullptr], promiseNum, size)) {
                  Debugger debugger{promiseNum, llvm::outs()};
                  debugger.dump(&i, context, results, size);
                  if (size >= 500) {
                    Debugger::exit(
                        &i, state[nullptr], promiseNum, llvm::outs());
                  }
                  return true;
                }
                return false;
              });
          sizes[promiseNum].erase(from, sizes[promiseNum].end());
        };
        for (int promiseNum = 0; promiseNum < 10; promiseNum++) {
         snapShot(promiseNum);
        }
      }
      // If the abstract state for this block did not change, then we are done
      // with this block. Otherwise, we must update the abstract state and
      // consider changes to successors.
      if (state == oldExitState) {
        continue;
      }

      for (auto* s : Direction::getSuccessors(*bb)) {
        work.add(s);
      }
      
      /* abstract function summary update */ 
      if (auto* key = Direction::getFunctionValueKey(*bb)) {
        auto* summary = getSummaryKey(f);
        // results[&f][summary] = meet({results[&f][summary], state[key]});
        results[&f][summary] = meet({results[&f][summary], state[nullptr]});
        for (int promiseNum = 0; promiseNum < 10; promiseNum++) {
          Debugger debugger{promiseNum, llvm::outs()};
          debugger.dump(
              llvm::dyn_cast<llvm::Instruction>(key), context, results, 0);
        }
      }
    }

    // The overall results for the given function and context are updated if
    // necessary. Updating the results for this (function,context) means that
    // all callers must be updated as well.
    auto& oldResults = allResults[context][&f];
    llvm::errs() << "\nFunction Summary check";
    if (!(oldResults == results)) {
      oldResults = results;
      for (auto& caller : callers[{context, &f}]) {
        contextWork.add(caller);
      }
    }

    active.erase({context, &f});
    return results;
  }


  void
  analyzeCall(llvm::CallSite cs,
              State& state,
              const Context& context,
              llvm::Function* forceEdge = nullptr) {
    Context newContext;
    if (newContext.size() > 0) {
      std::copy(context.begin() + 1, context.end(), newContext.begin());
      newContext.back() = cs.getInstruction();
    }

    auto* caller  = cs.getInstruction()->getFunction();
    //auto* callee  = getCalledFunction(cs);
    auto* callee  = forceEdge ? forceEdge : getCalledFunction(cs);
    //addToMap(caller, callee);

    auto toCall   = std::make_pair(newContext, callee);
    auto toUpdate = std::make_pair(context, caller);

    auto& calledState  = allResults[newContext][callee];
    auto& summaryState = calledState[callee];

    // Forces computation if not already computed
    bool needsUpdate   = summaryState.size() == 0; 

    needsUpdate 
      |= Direction::prepareSummaryState(cs, callee, state,
                                        summaryState, transfer, 
                                        meet, context);
    // TODO: 
    // 3. Wiring arguments.
    // 4. Prune Edges at loads
    if (!active.count(toCall) && needsUpdate) {
      computeDataflow(*callee, newContext);
    }
    //if (forceEdge) {
    //  auto& toMerge = calledState[callee][callee];
    //  llvm::errs() << "\nToMerge";
    //  toMerge[PLEDGE_STDIO].print(llvm::errs());
    //  auto withCallConjunct = transformer(toMerge, cs, forceEdge);
    //  state[cs.getInstruction()] = meet({state[cs.getInstruction()], withCallConjunct});
    //} else {
    state[cs.getInstruction()] = calledState[callee][callee];
    //}
    callers[toCall].insert(toUpdate);
  }

private:
  // These property objects determine the behavior of the dataflow analysis.
  // They should by replaced by concrete implementation classes on a per
  // analysis basis.
  using Callees = std::unordered_set<llvm::Function*>;
  std::unordered_map<llvm::Function*, Callees> callGraph;

  void
  addToMap(llvm::Function* caller, llvm::Function* callee) {
    callGraph[caller].insert(callee);
    checkCycle(callee);
    return;
  }

  void
  checkCycle(llvm::Function* start) {
    std::stack<llvm::Function*> callStack;
    std::unordered_set<llvm::Function*> visited;
    std::vector<llvm::Function*> path;

    callStack.push(start);
    while (!callStack.empty()) {
      auto caller = callStack.top();
      callStack.pop();
      auto calleeList = callGraph[caller];
      
      for (auto* callee : calleeList) {
        if (visited.count(callee)) {
          llvm::errs() << "\nFound cycle";
          llvm::errs() << "\nPath ";
          for (auto* inst : path) {
            llvm::errs() << *inst << " ";
          }
          llvm::errs() << *callee;
          std::exit(0);
        }
        callStack.push(callee);
        path.push_back(callee);
      }
    }
    return;
  }

  
  Meet meet;
  Transfer transfer;
  EdgeTransformer transformer;

  AllResults allResults;
  ContextWorklist contextWork;
  llvm::DenseMap<ContextFunction, llvm::DenseSet<ContextFunction>> callers;
  llvm::DenseSet<ContextFunction> active;

  static llvm::Value*
  getSummaryKey(llvm::Function& f) {
    return &f;
  }

  void
  mergeInState(State& destination, const State& toMerge) {
    for (auto& valueStatePair : toMerge) {
      // If an incoming Value has an AbstractValue in the already merged
      // state, meet it with the new one. Otherwise, copy the new value over,
      // implicitly meeting with bottom.
      auto [found, newlyAdded] = destination.insert(valueStatePair);
      if (!newlyAdded) {
        found->second = meet({found->second, valueStatePair.second});
      }
    }
  }


  void
  mergeInState(State& destinationState,
               const State& toMerge,
               llvm::Value* destination,
               llvm::Value* branchAsValue) {
    EdgeTransformer transformer;
    for (auto& valueStatePair : toMerge) {
      // If an incoming Value has an AbstractValue in the already merged
      // state, meet it with the new one. Otherwise, copy the new value over,
      // implicitly meeting with bottom.

      // FIXME: State is carried around at returns and callsites
      // Even after removing them at the call/return
      //if (valueStatePair.first) {
      //  llvm::errs() << "\nBefore, key " << *valueStatePair.first;
      //} else {
      //  llvm::errs() << "\nBefore key nullptr";
      //}
      //valueStatePair.second[PLEDGE_CPATH].print(llvm::errs());

      auto withEdges = valueStatePair.second;
      if (valueStatePair.first == nullptr) { // only rewrite state at nullptr
        auto* checkLoc = transformer.setLocation(branchAsValue);
        withEdges = transformer(withEdges, branchAsValue, destination);
        transformer.removeLocation(checkLoc);
      }
      auto [found, newlyAdded] = destinationState.insert({valueStatePair.first, withEdges});

      //if (valueStatePair.first) {
      //  llvm::errs() << "\nFor key " << *valueStatePair.first;
      //} else {
      //  llvm::errs() << "\nFor key nullptr";
      //}
      //llvm::errs() << "\nAfter bb transformation args";
      //temp[PLEDGE_CPATH].print(llvm::errs());
      if (!newlyAdded) {
        found->second = meet({found->second, withEdges});
      }
    }
  }

  State
  mergeStateFromPredecessors(llvm::BasicBlock* bb, FunctionResults& results) {
    // Make a copy of the predecessors, modify copy
    llvm::DenseMap<llvm::Instruction*, State> predFacts;
    for (auto* p : Direction::getPredecessors(*bb)) { // for each predecessor
      auto predecessorFacts = results.find(Direction::getExitKey(*p)); // get exit key
      if (results.end() == predecessorFacts) {
        continue;
      }

      auto predState = predecessorFacts->second;        
      predFacts[Direction::getExitKey(*p)] = predState;
    }

    State mergedState = State{};
    if (predFacts.size() > 1) {
      mergedState[nullptr] = meet.getIntersection(predFacts);
    } 
    // Rely on meet for reachability
    mergeInState(mergedState, results[bb]); // prev bb results

    // checking for equality from here won't work since
    // states for each privilege is independent

    for (auto* p : Direction::getPredecessors(*bb)) {
      //auto predecessorFacts = results.find(Direction::getExitKey(*p));
      //if (results.end() == predecessorFacts) {
      auto predecessorFacts = predFacts.find(Direction::getExitKey(*p));
      if (predFacts.end() == predecessorFacts) {
        continue;
      }
      auto* branchInst = bb->getTerminator();
      mergeInState(mergedState, predecessorFacts->second, p, branchInst);
    }
    return mergedState;
  }

  AbstractValue
  meetOverPHI(State& state, const llvm::PHINode& phi, const Context& context) {
    auto phiValue = AbstractValue();
    for (auto& value : phi.incoming_values()) {
      auto found = state.find(value.get());
      if (state.end() == found) {
        transfer(*value.get(), state, context);
        found = state.find(value.get());
      }
      phiValue = meet({phiValue, found->second});
    }
    return phiValue;
  }

  void
  applyTransfer(llvm::Instruction& i, State& state, const Context& context) {
    if (auto* phi = llvm::dyn_cast<llvm::PHINode>(&i);
        phi && Direction::shouldMeetPHI()) {
      // Phis can be explicit meet operations
      state[phi] = meetOverPHI(state, *phi, context);
    } else {
      auto* checkLoc = transfer.setLocation(&i);
      transfer(i, state, context);
      transfer.removeLocation(checkLoc);
    }
  }
};

} // end namespace


#endif
