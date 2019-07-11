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
  static auto getSuccessors(llvm::BasicBlock& bb) {
    return llvm::predecessors(&bb);
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
    [[maybe_unused]]
    auto* passedConcrete = cs.getInstruction();
    auto& passedAbstract = state.FindAndConstruct(nullptr);
    //auto& passedAbstract = state[nullptr];

    /*Marshalling abstract state into functions*/
    for (auto& bb : *callee) {
      if (auto* ret = llvm::dyn_cast<llvm::ReturnInst>(bb.getTerminator());
          ret /*&& ret->getReturnValue()*/ ) {
        // Pick up returns with return values
        // auto& retState = summaryState[ret->getReturnValue()];
        // auto  newState = meet({passedAbstract, retState});
        auto& retState = summaryState[ret];
        auto  newState = meet({passedAbstract.second, retState});
        needsUpdate |= !(newState == retState);
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


  DataflowAnalysis(llvm::Module& m,
                   llvm::ArrayRef<llvm::Function*> entryPoints,
                   bool isInterprocedural) {
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
          analyzeCall(cs, state, context);
        }
        // ska: uncomment to skip function calls
        // else {
        applyTransfer(i, state, context);
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
                  if (size >= 8000) {
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
        //Debugger debugger{PLEDGE_CPATH, llvm::outs()};
        //debugger.dump(llvm::dyn_cast<llvm::Instruction>(key), context, results, 0);
      }
    }

    // The overall results for the given function and context are updated if
    // necessary. Updating the results for this (function,context) means that
    // all callers must be updated as well.
    auto& oldResults = allResults[context][&f];
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
  analyzeCall(llvm::CallSite cs, State &state, const Context& context) {
    Context newContext;
    if (newContext.size() > 0) {
      std::copy(context.begin() + 1, context.end(), newContext.begin());
      newContext.back() = cs.getInstruction();
    }

    auto* caller  = cs.getInstruction()->getFunction();
    auto* callee  = getCalledFunction(cs);
    auto toCall   = std::make_pair(newContext, callee);
    auto toUpdate = std::make_pair(context, caller);

    auto& calledState  = allResults[newContext][callee];
    auto& summaryState = calledState[callee];
    bool needsUpdate   = summaryState.size() == 0;

    needsUpdate 
      |= Direction::prepareSummaryState(cs, callee, state,
                                        summaryState, transfer, 
                                        meet, context);

    if (!active.count(toCall) && needsUpdate) {
      computeDataflow(*callee, newContext);
    }

    state[cs.getInstruction()] = calledState[callee][callee]; // getSummaryKey()
    callers[toCall].insert(toUpdate);
  }

private:
  // These property objects determine the behavior of the dataflow analysis.
  // They should by replaced by concrete implementation classes on a per
  // analysis basis.
  Meet meet;
  Transfer transfer;

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
               llvm::Value* branchAsValue,
               bool isSame) {
    EdgeTransformer edgeTransformer;
    for (auto& valueStatePair : toMerge) {
      // If an incoming Value has an AbstractValue in the already merged
      // state, meet it with the new one. Otherwise, copy the new value over,
      // implicitly meeting with bottom.

      auto temp =
          edgeTransformer(valueStatePair.second, branchAsValue, destination, isSame);
      auto [found, newlyAdded] = destinationState.insert({valueStatePair.first,temp});
      //temp.print(llvm::errs());
      if (!newlyAdded) {
        found->second = meet({found->second, temp});
        //found->second.print(llvm::errs());
      }
    }
  }

  State
  mergeStateFromPredecessors(llvm::BasicBlock* bb, FunctionResults& results) {
    State mergedState = State{};
    mergeInState(mergedState, results[bb]);
  
    std::vector<AbstractValue*> facts;
    for (auto* p : Direction::getPredecessors(*bb)) {
      auto predecessorFacts = results.find(Direction::getExitKey(*p));
      //llvm::errs() << "\n Exit Key =" << *p;
      if (results.end() == predecessorFacts) {
        continue;
      }
      facts.push_back(&(predecessorFacts->second[nullptr]));
    }
    bool isSame = true;
    auto prevFact = facts.begin();
    for (auto* fact : facts) {
      isSame &= *fact == **prevFact;
    }
    for (auto* p : Direction::getPredecessors(*bb)) {
      auto predecessorFacts = results.find(Direction::getExitKey(*p));
      if (results.end() == predecessorFacts) {
        continue;
      }
      auto* branchInst = bb->getTerminator();
      mergeInState(mergedState, predecessorFacts->second, p, branchInst, isSame);
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
      transfer(i, state, context);
    }
  }
};

} // end namespace


#endif