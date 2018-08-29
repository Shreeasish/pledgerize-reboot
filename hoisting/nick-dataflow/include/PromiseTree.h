#ifndef PROMISETREE_H
#define PROMISETREE_H

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

#include "DataflowAnalysis.h"
#include "FutureFunctions.h"
#include "IndirectCallResolver.h"
#include "TaintAnalysis.h"
#include "PromiseTree.h"

#include <memory>
#include <queue>

using std::shared_ptr;
using std::queue;

using namespace llvm;

//TODO: Move functions outside classes

static llvm::Function*
getCalledFunction(llvm::CallSite cs) {
  if (!cs.getInstruction()) {
    return nullptr;
  }

  llvm::Value* called = cs.getCalledValue()->stripPointerCasts();

  if (called->getName().contains("llvm")) {
    return nullptr;
  }

  return llvm::dyn_cast<llvm::Function>(called);
}


struct PromiseSummary {

  int id;
  PromiseBitset promises;

  struct Edge {
    size_t position;
    PromiseSummary* child;
  };

  struct Branch {
    Instruction* condition = nullptr;
    llvm::SmallVector<Edge,2> edges;
  } branch;

  PromiseSummary(PromiseBitset pb)
    : promises{pb}
      {}

  // PromiseSummary(llvm::BranchInst* newCondition, PromiseBitset bitset)
  //   : branch{newCondition,{}},
  //     promises{bitset},
  //     isNatal{true}
  //     {}

  PromiseSummary(llvm::BranchInst* newCondition)
     : branch{newCondition, {}}
       {}

  ~PromiseSummary(){
    llvm::errs() << "kill myself";
    llvm::errs() << id;
  }

  void
  dump(llvm::raw_ostream& out) {
    out << this << "[label=\"";
    if(branch.condition) {
      out << *branch.condition;
    }
    else {
      out << "None" ;
    }
    out << "\"];\n";

    out << this << "->" << &promises << "\n";
    out << &promises
        << "[shape=\"box\",label=\""
        << promises.to_ullong()
        << "\"];\n";

    for (auto edge : branch.edges) {
      out << this << "->" << edge.child << "\n" ;
    }
  }
};

class PromiseTree {
public:
  PromiseTree() {
    root = new PromiseSummary{PromiseBitset{0}};
    root->id = 999;
    }

  PromiseTree(PromiseBitset incomingBitset) {
    root = new PromiseSummary{incomingBitset};
    root->id = 999;
  }

  PromiseTree(PromiseTree* lhsTree, PromiseTree* rhsTree){
    PromiseSummary* newRoot =
      new PromiseSummary{*( lhsTree->getRootPromise() )|*( rhsTree->getRootPromise() )};
    newRoot->id = 600;
    newRoot->branch.edges.push_back(PromiseSummary::Edge{0,&lhsTree->getRootNode()});
    newRoot->branch.edges.push_back(PromiseSummary::Edge{1,&rhsTree->getRootNode()});
    setRootNode(newRoot);
  }

  PromiseSummary*
  getRootPointer() const {
    if (root)
      return root;
    else
      llvm::errs().changeColor(raw_ostream::Colors::RED);
      llvm::errs() << "\nbroken pointer\n";
      return nullptr; //ska: add handling for this
  }

  PromiseBitset*
  getRootPromise() const {
    auto rootptr = getRootPointer();
    return &(rootptr->promises);
  }

  PromiseSummary&
  getRootNode() const{
    return *root;
  }

  void
  setRootNode(PromiseSummary* newRoot){
    this->root = newRoot;
  }

//  llvm::BranchInst*
//    getTempBranch() const{
//      return this->tempBranchInst;
//    }

//  void
//  setTempBranch(llvm::BranchInst* branchInst){
//    this->tempBranchInst = branchInst;
//  }

  bool
  operator==(PromiseTree other) const{
    if (this->root != other.getRootPointer()) {
      return false;
    }
    return true;
  }

  PromiseTree&
  insert(llvm::BranchInst* branchInst) {
    this->getRootNode().branch.condition = branchInst;
    return *this;
  }

  PromiseTree&
  insert(PromiseTree* ptree, size_t position){
    this->getRootNode().branch.edges.push_back( {position, &ptree->getRootNode()} );
    return *this;
  }



  template <typename Lambda>
  PromiseTree&
  traverseDo(Lambda& action) {
    auto dfs = [&action] (PromiseSummary* node, auto& dfs) -> void {
      if(!node) {return;}
      //Recursion
      for (auto& edge : node->branch.edges) {
        dfs(edge.child, dfs);
      }
      action(node);
    };
    dfs(root,dfs);

    return *this;
  };

  template <typename Matcher>
  PromiseSummary*
  find(Matcher& matcher) {
    std::queue<PromiseSummary*> bfsQueue;
    std::set<PromiseSummary*>   seenSet;

    auto bfs = [&bfsQueue, &matcher, &seenSet](PromiseSummary* node, auto& bfs) -> PromiseSummary* {
      bfsQueue.pop();
      if(!node || !seenSet.insert(node).second){
        return nullptr;
      }
      if(matcher(node)){
        return node;
      }
      for (auto& edge : node->branch.edges) {
        bfsQueue.push(edge.child);
      }
      return bfs(bfsQueue.front(), bfs);
    };

    return bfs(&getRootNode(), bfs);
  }

private:
  PromiseSummary* root;
  //llvm::BranchInst* tempBranchInst = nullptr;
};

class SharedPromiseTree {
public:
  //Constructors
  SharedPromiseTree()
    : promiseTreePtr{std::make_shared<PromiseTree>(PromiseTree())}
    {}

  SharedPromiseTree(PromiseTree* other)
    : promiseTreePtr{other}
    {}

  SharedPromiseTree(PromiseBitset incomingBitset)
    : promiseTreePtr{std::make_shared<PromiseTree>(PromiseTree{incomingBitset})}
    {}

  SharedPromiseTree(SharedPromiseTree& s1, SharedPromiseTree& s2){
    PromiseTree newPTree{&*(s1.getPointer()), &*(s2.getPointer())};
    this->promiseTreePtr = std::make_shared<PromiseTree>(newPTree);
  }


  std::shared_ptr<PromiseTree>
  getPointer() const {
    return promiseTreePtr;
  }

  //Methods
  PromiseSummary*
  findBranch(llvm::BranchInst* branchInst) const {
    auto matcher = [branchInst](PromiseSummary* PromiseSummary) {
      return PromiseSummary->branch.condition == branchInst;
    };

    return promiseTreePtr->find(matcher);
  }

  PromiseSummary*
  getRootNode() const {
    return &(promiseTreePtr->getRootNode());
  }

  PromiseBitset*
  getRootPromise() const {
    return &(this->getRootNode()->promises);
  }

  void
  setRootPromise(PromiseBitset* incomingBitset){
    auto* rootPromise = this->getRootPromise();
    (*rootPromise)|= *incomingBitset;
    return;
  }

  PromiseSummary*
  getBranch(llvm::BranchInst* incomingBranch) const {
    // if it exists return the node
    if (auto* foundBranch = findBranch(incomingBranch)) {
      return foundBranch;
    }
    return nullptr;
  }

  SharedPromiseTree
  mergeAtMeet(SharedPromiseTree& leftTree, SharedPromiseTree& rightTree, llvm::BranchInst* condition){
    SharedPromiseTree newSharedPromiseTree{};
    newSharedPromiseTree.getPointer()->insert(condition).insert(leftTree.promiseTreePtr.get(),0).insert(rightTree.promiseTreePtr.get(),1);
    return newSharedPromiseTree;
  }

  // Operators
  bool
  operator==( SharedPromiseTree other) const {
    return this->promiseTreePtr->getRootPromise() == other.getRootPromise();
  }

  void
  operator=(SharedPromiseTree other) {
    this->promiseTreePtr = other.getPointer();
  }

  void
  operator|=(PromiseBitset bitset) {
    *(this->getRootPromise()) |= bitset;
  }

  SharedPromiseTree
  operator|(SharedPromiseTree& rightTree){
    this->setRootPromise(rightTree.getRootPromise());
    return *this;
  }

  //Printers
  void
  dump(llvm::raw_ostream& out) const{
    out << "digraph{\n" ;
    auto printer = [&out](PromiseSummary* node) {
      if (node) {
        node->dump(out);
      }
    };
    if(promiseTreePtr){
      if (promiseTreePtr) {
        promiseTreePtr->traverseDo(printer);
      }
    }
    out << "}\n" ;
  }

private:
  std::shared_ptr<PromiseTree> promiseTreePtr;
};

#endif
