
#pragma once

#include <deque>

#include "llvm/IR/CallingConv.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

using PossibleTargets = llvm::SmallVector<llvm::Function*, 8>;

class IndirectCallResolver {
public:
  IndirectCallResolver(llvm::Module& m)
    : storage{},
      roots{buildTypeTree(m)}
      { }

  llvm::SmallVector<llvm::Function*,8>
  getPossibleTargets(llvm::CallSite cs) {
    llvm::SmallVector<llvm::Function*,8> targets;
    auto root = roots.find(cs.getCallingConv());
    if (root == roots.end()) {
      return {};
    }

    auto* functionType = cs.getFunctionType();
    auto* currentNode = root->second;
    auto collectNextType = [&currentNode, &targets, this] (llvm::Type* ty) {
      auto id = getTypeID(ty);
      auto found = currentNode->nextType.find(id);
      if (found == currentNode->nextType.end()) {
        return false; // False stands in for NOT_FOUND
      }
      currentNode = found->second;
      auto& variadic = currentNode->variadic;
      targets.append(variadic.begin(), variadic.end());
      return true;
    };

    if (!collectNextType(functionType->getReturnType())) {
      return {};
    }
    for (auto* ty : functionType->params()) {
      if (!collectNextType(ty)) {
        return targets;
      }
    }
    auto& standard = currentNode->standard;
    targets.append(standard.begin(), standard.end());
    return targets;
  }

private:
  struct TypeNode;
  using TypeID = uintptr_t;
  using TypeTree = llvm::DenseMap<llvm::CallingConv::ID, TypeNode*>;

  struct TypeNode {
    llvm::DenseMap<TypeID, TypeNode*> nextType;
    llvm::SmallVector<llvm::Function*,8> standard;
    llvm::SmallVector<llvm::Function*,8> variadic;
  };


  TypeID
  getTypeID(llvm::Type* ty) const {
    constexpr TypeID COMMON_POINTER_ID = 0x01010101;
    return llvm::isa<llvm::PointerType>(ty)
        ? COMMON_POINTER_ID : reinterpret_cast<TypeID>(ty);
  }

  TypeTree
  buildTypeTree(llvm::Module& m) {
    TypeTree typeTree;
    for (auto& f : m) {
      if (!f.hasAddressTaken()) {
        continue;
      }

      auto [root, added] = typeTree.insert({f.getCallingConv(), nullptr});
      if (added) {
        root->second = createTypeNode();
      }

      auto* functionType = f.getFunctionType();
      TypeNode* currentNode = root->second;
      auto addNode = [&currentNode, this] (llvm::Type* ty) {
        auto id = getTypeID(ty);
        auto [node, added] = currentNode->nextType.insert({id, nullptr});
        if (added) {
          node->second = createTypeNode();
        }
        currentNode = node->second;
      };

      addNode(functionType->getReturnType());
      for (llvm::Type* ty : functionType->params()) {
        addNode(ty);
      }

      // Now we have the deepest node in the tree for the type.
      auto& list = (functionType->isVarArg())
                 ? currentNode->variadic : currentNode->standard;
      list.push_back(&f);
    }
    return typeTree;
  }

  TypeNode*
  createTypeNode() {
    storage.emplace_back();
    return &storage.back();
  }

  std::deque<TypeNode> storage;
  TypeTree roots;
};


