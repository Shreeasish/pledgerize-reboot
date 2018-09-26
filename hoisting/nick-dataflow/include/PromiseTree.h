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

  PromiseSummary(llvm::BranchInst* newCondition)
     : branch{newCondition, {}}
       {}

  ~PromiseSummary(){
    llvm::errs() << "kill myself";
    llvm::errs() << id;
  }

  void
  insertEdge(size_t position, PromiseSummary* node){
    branch.edges.push_back({position, node});

    return;
  }

  void
  dump(llvm::raw_ostream& out) {
    out << "  n" << this << "[label=\"";
    if(branch.condition) {
      out << "  n" << *branch.condition;
    }
    else {
      out << "None" ;
    }
    out << "\"];\n";

    out << "  n" << this << "->n" << &promises
        << ";\n";

    out << "  n" << &promises
        << "[shape=\"box\",label=\""
        << promises.to_ullong()
        << "\"];\n";

    for (auto edge : branch.edges) {
      out << "  n" << this << "->n" << edge.child
          << ";\n";
    }
  }
};

class PromiseTree {
public:
  PromiseTree() {
    root = new PromiseSummary{PromiseBitset{0}};
    }

  PromiseTree(PromiseBitset incomingBitset) {
    root = new PromiseSummary{incomingBitset};
  }

  PromiseTree(PromiseSummary* node){
    root = node;
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

  bool
  operator==(PromiseTree other) const{
    if (this->root != other.getRootPointer()) {
      return false;
    }
    return true;
  }

  PromiseTree&
  setRootBranch(llvm::BranchInst* branchInst);

  PromiseTree&
  insert(PromiseTree* ptree, size_t position){
    this->getRootNode().branch.edges.push_back( {position, &ptree->getRootNode()} );
    return *this;
  }

//  PromiseTree&  // ska: fix this later
//  insert(llvm::ArrayRef<ArrayRef<PromiseSummary::Edge>> incomingEdges){
//    std::accumulate(incomingEdges.begin(), incomingEdges.end(),
//       *this,
//       [this] (auto& tree, auto edge) {
//          tree.getRootNode().branch.edges.push_back(edge);
//          return *this;
//       });
//    return *this;
//  }

  PromiseTree&
  insert(PromiseSummary::Edge ledge, PromiseSummary::Edge redge){
    this->getRootNode().branch.edges.push_back(ledge);
    this->getRootNode().branch.edges.push_back(redge);
    return *this;
  }

  template <typename Lambda>
  PromiseTree&
  traverseDo(Lambda& action) {
    auto dfs = [&action] (PromiseSummary* node, auto& dfs) -> void {
      if(!node) {return;}
      //Recursion
      action(node);
      for (auto& edge : node->branch.edges) {
        dfs(edge.child, dfs);
      }
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

  SharedPromiseTree(PromiseSummary* node)
    : promiseTreePtr{std::make_shared<PromiseTree>(PromiseTree(node))}
    {}

  SharedPromiseTree(PromiseBitset incomingBitset)
    : promiseTreePtr{std::make_shared<PromiseTree>(PromiseTree{incomingBitset})}
    {}

//  SharedPromiseTree(SharedPromiseTree& s1, SharedPromiseTree& s2){
//    PromiseTree newPTree{&*(s1.getPointer()), &*(s2.getPointer())};
//    this->promiseTreePtr = std::make_shared<PromiseTree>(newPTree);
//  }

  SharedPromiseTree
  mergeAtMeet(SharedPromiseTree& leftTree, SharedPromiseTree& rightTree, llvm::BranchInst* condition);

  std::shared_ptr<PromiseTree>
  getPointer() const {
    return promiseTreePtr;
  }

  //Methods

  PromiseSummary*
  getRootNode() const {
    return &(promiseTreePtr->getRootNode());
  }

  PromiseBitset*
  getRootPromise() const {
    return &(this->getRootNode()->promises);
  }

  void
  addToRootPromise(PromiseBitset* incomingBitset){
    auto* rootPromise = this->getRootPromise();
    (*rootPromise)|= *incomingBitset;
    return;
  }

  PromiseSummary*
  getOrInsertConditionNode(llvm::BranchInst* branchInst);

  // Operators
  bool
  operator==(SharedPromiseTree other) const {
    return ( this->promiseTreePtr == other.promiseTreePtr );
  }

  void
  operator=(SharedPromiseTree other) {
    this->promiseTreePtr = std::make_shared<PromiseTree>(*other.promiseTreePtr);
  }

  void
  operator|=(PromiseBitset bitset) {
    *(this->getRootPromise()) |= bitset;
  }

  SharedPromiseTree
  operator|(SharedPromiseTree& rightTree){
    this->addToRootPromise(rightTree.getRootPromise());
    return *this;
  }

  //Printers
  void
  dump(llvm::raw_ostream& out, llvm::Value* location ) const{
    out << "  n" <<  location
        << "[label=\"" ;

    if(location){
      out << *location << "\"];\n" ;

      out << "  n" << location
          << "->n"  << this->getRootNode()
          << ";\n" ;
    } else {
      out << "unknown" << "\"];\n" ;

      out << "  n" << location
          << "->"  << this->getRootNode()
          << ";\n" ;
    }
    auto printer = [&out](PromiseSummary* node) {
      if (node) {
        node->dump(out);
      }
    };
    if (promiseTreePtr) {
      promiseTreePtr->traverseDo(printer);
    }
  }

private:
  std::shared_ptr<PromiseTree> promiseTreePtr;

  SharedPromiseTree&
  restructurePromises(){
    auto* rootNode = this->getRootNode();

    PromiseBitset commonPromises{};
    commonPromises.flip();
    for(auto edge : rootNode->branch.edges){
      commonPromises &= edge.child->promises;
    }
    rootNode->promises = commonPromises;

    commonPromises.flip();
    auto subtractFromPromise = [commonPromises](PromiseSummary* node) {
      node->promises &= commonPromises;
    };

    this->promiseTreePtr->traverseDo(subtractFromPromise);
    return *this;
  }

};


PromiseTree&
PromiseTree::setRootBranch(llvm::BranchInst* branchInst) {
  this->getRootNode().branch.condition = branchInst;
  return *this;
}


PromiseSummary*
SharedPromiseTree::getOrInsertConditionNode(llvm::BranchInst* brInst) {
  if (!brInst) {
    llvm::errs() << "Inserting nullptr as conditional" ; //llvm::unreachable?
    return nullptr;
  }

  PromiseSummary* foundNode = nullptr;
  auto checkBranch = [&foundNode, &brInst]( PromiseSummary* node) -> void {
    if (brInst == node->branch.condition) {
      foundNode = node;
      return;
    }
  };
  this->promiseTreePtr->traverseDo(checkBranch);

  if (!foundNode) {
    auto* newNode = new PromiseSummary{brInst};
    auto* oldRoot = this->getRootNode();

    newNode->insertEdge(0, oldRoot);
    return newNode;
  }
  return foundNode;
}


SharedPromiseTree
SharedPromiseTree::mergeAtMeet(SharedPromiseTree& leftTree, SharedPromiseTree& rightTree, llvm::BranchInst* condition) {
  auto* conditionNode = leftTree.getOrInsertConditionNode(condition);
  conditionNode->branch.edges.push_back({ 1, rightTree.getRootNode()});

  return SharedPromiseTree{conditionNode}.restructurePromises();
}


#endif
