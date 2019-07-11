#ifndef TAINTANALYSIS_H
#define TAINTANALYSIS_H

//Rename to tmpanalysis

#include "llvm/IR/Module.h"

#include <bitset>

namespace tmpanalysis {

using FileValue  = std::bitset<1>;

using tmppathResultsTy = llvm::DenseMap<std::array<llvm::Instruction *, 2>, llvm::DenseMap<llvm::Function *, llvm::DenseMap<llvm::Value *, llvm::DenseMap<llvm::Value *, FileValue,
      llvm::DenseMapInfo<llvm::Value *>, llvm::detail::DenseMapPair<llvm::Value *, FileValue > >, llvm::DenseMapInfo<llvm::Value *>, llvm::detail::DenseMapPair<llvm::Value *,
      llvm::DenseMap<llvm::Value *, FileValue, llvm::DenseMapInfo<llvm::Value *>, llvm::detail::DenseMapPair<llvm::Value *, FileValue > > > >, llvm::DenseMapInfo<llvm::Function *>,
      llvm::detail::DenseMapPair<llvm::Function *, llvm::DenseMap<llvm::Value *, llvm::DenseMap<llvm::Value *, FileValue, llvm::DenseMapInfo<llvm::Value *>,
      llvm::detail::DenseMapPair<llvm::Value *, FileValue > >, llvm::DenseMapInfo<llvm::Value *>, llvm::detail::DenseMapPair<llvm::Value *, llvm::DenseMap<llvm::Value *, FileValue,
      llvm::DenseMapInfo<llvm::Value *>, llvm::detail::DenseMapPair<llvm::Value *, FileValue > > > > > >, llvm::DenseMapInfo<std::array<llvm::Instruction *, 2> >,
      llvm::detail::DenseMapPair<std::array<llvm::Instruction *, 2>, llvm::DenseMap<llvm::Function *, llvm::DenseMap<llvm::Value *, llvm::DenseMap<llvm::Value *, FileValue,
      llvm::DenseMapInfo<llvm::Value *>, llvm::detail::DenseMapPair<llvm::Value *, FileValue > >, llvm::DenseMapInfo<llvm::Value *>, llvm::detail::DenseMapPair<llvm::Value *,
      llvm::DenseMap<llvm::Value *, FileValue, llvm::DenseMapInfo<llvm::Value *>, llvm::detail::DenseMapPair<llvm::Value *, FileValue > > > >, llvm::DenseMapInfo<llvm::Function *>,
      llvm::detail::DenseMapPair<llvm::Function *, llvm::DenseMap<llvm::Value *, llvm::DenseMap<llvm::Value *, FileValue, llvm::DenseMapInfo<llvm::Value *>,
      llvm::detail::DenseMapPair<llvm::Value *, FileValue > >, llvm::DenseMapInfo<llvm::Value *>, llvm::detail::DenseMapPair<llvm::Value *, llvm::DenseMap<llvm::Value *, FileValue,
      llvm::DenseMapInfo<llvm::Value *>, llvm::detail::DenseMapPair<llvm::Value *, FileValue > > > > > > > >;

tmppathResultsTy
gettmpAnalysisResults(llvm::Module& module);

}
#endif