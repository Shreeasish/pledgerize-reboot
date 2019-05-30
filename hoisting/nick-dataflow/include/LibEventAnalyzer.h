#pragma once

using MayCallMap = llvm::DenseMap<llvm::Function*, llvm::Function*>;
namespace libEventAnalyzer {
  void
  getResults(llvm::Module&);
}

[[maybe_unused]]
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
