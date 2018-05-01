
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfo.h"
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

#include "DataflowAnalysis.h"


using namespace llvm;
using std::string;
using std::unique_ptr;


static cl::OptionCategory filePolicyCategory{"file policy options"};

static cl::opt<string> inPath{cl::Positional,
                              cl::desc{"<Module to analyze>"},
                              cl::value_desc{"bitcode filename"},
                              cl::init(""),
                              cl::Required,
                              cl::cat{filePolicyCategory}};

// This exists
static const llvm::Function *
getCalledFunction(const llvm::CallSite cs) {
  if (!cs.getInstruction()) {
    return nullptr;
  }

  const llvm::Value *called = cs.getCalledValue()->stripPointerCasts();
  return llvm::dyn_cast<llvm::Function>(called);
}

// Needed
enum PossibleFileValues {
  IS_TMP
};

// Maybe rename these
using FileValue  = std::bitset<1>;
using FileState  = analysis::AbstractState<FileValue>;
using FileResult = analysis::DataflowResult<FileValue>;

// Need this
class FilePolicyMeet : public analysis::Meet<FileValue, FilePolicyMeet> {
public:
  FileValue
  meetPair(FileValue& s1, FileValue& s2) const {
    return s1 | s2;
  }
};


static bool
isFOpenFamily(llvm::StringRef name) {
  return name == "fopen" || name == "fdopen" || name == "freopen";
}

static bool
takesPathToMakeTemp(llvm::StringRef name) {
  return name == "tempnam"
    || name == "mktemp"
    || name == "mkdtemp"
    || name == "mkostemp"
    || name == "mkstemps"
    || name == "mkostemps";
}


static llvm::ConstantDataArray*
getUnderlyingData(llvm::Value* value) {
  if (auto* c = llvm::dyn_cast<llvm::ConstantExpr>(value)) {
    value = c->getAsInstruction();
  }
  auto* gep = llvm::dyn_cast<llvm::GetElementPtrInst>(value);
  if (!gep) {
    return nullptr;
  }
  auto* global = llvm::dyn_cast<llvm::GlobalVariable>(gep->getPointerOperand());
  if (!global) {
    return nullptr;
  }
  auto* init = global->getInitializer();
  return llvm::dyn_cast_or_null<llvm::ConstantDataArray>(init);
}

static bool
isConstantTempPath(llvm::Value* value) {
  auto* data = getUnderlyingData(value);
  return data && data->getAsString().startswith("/tmp/");
}


class FilePolicyTransfer {
public:
  void
  operator()(llvm::Value& v, FileState& state) {
    // Conservatively model all loaded info as unknown
    if (auto* li = dyn_cast<LoadInst>(&v)) {
      state[li].set();
      return;
    }

    const CallSite cs{&v};
    const auto* fun = getCalledFunction(cs);
    // Pretend that indirect calls & non calls don't exist for this analysis
    if (!fun) {
      state[&v].set();
      return;
    }

    // Apply the transfer function to the absract state
    if (isFOpenFamily(fun->getName())) {
      auto& value = state[&v];
      value.reset();
      if (state[cs.getArgument(0)].test(IS_TMP)
          || isConstantTempPath(cs.getArgument(0))) {
        value.set(IS_TMP);
      }

    } else if (fun->getName() == "fclose") {
      auto *closed = cs.getArgument(0);
      auto& value = state[closed];
      value.reset();
    } else if (fun->getName() == "tmpfile") {
      auto& value = state[&v];
      value.set(IS_TMP);

    // Conservative use would check that arg(0) sits in /tmp/.
    // We assume good practices that all temporaries live in /tmp/.
    } else if (takesPathToMakeTemp(fun->getName())) {
      auto& value = state[&v];
      value.set(IS_TMP);

    // These two are not template based and may see different patterns.
    } else if (fun->getName() == "tmpnam" || fun->getName() == "tmpnam_r") {
      auto& value = state[&v];
      value.set(IS_TMP);
    }
  }
};


static bool
mayBeTemp(FileState& state, Value* arg) {
  const auto found = state.find(arg);
  return state.end() != found && found->second.test(IS_TMP);
}


template <typename OutIterator>
static void
collectFileRights(FileResult& fileStates, OutIterator tempRW) {
  for (auto& [value,localState] : fileStates) {
    auto* inst = llvm::dyn_cast<llvm::Instruction>(value);
    if (!inst) {
      continue;
    }

    llvm::CallSite cs{inst};
    auto* fun = getCalledFunction(cs);
    if (!fun) {
      continue;
    }

    // Check the incoming state for errors
    auto& state = analysis::getIncomingState(fileStates, *inst);
    if ((fun->getName() == "fread" || fun->getName() == "fwrite")
        && mayBeTemp(state, cs.getArgument(3))) {
      *tempRW++ = std::make_pair(inst, 3);
      
    } else if ((fun->getName() == "fprintf"
             || fun->getName() == "fflush")
          && mayBeTemp(state, cs.getArgument(0))) {
      *tempRW++ = std::make_pair(inst, 0);
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
printErrors(llvm::ArrayRef<std::pair<llvm::Instruction*, unsigned>> errors) {
  for (auto& [fileOperation, argNum] : errors) {
    llvm::outs().changeColor(raw_ostream::Colors::RED);
    printLineNumber(llvm::outs(), *fileOperation);

    auto* called = getCalledFunction(llvm::CallSite{fileOperation});
    llvm::outs().changeColor(raw_ostream::Colors::YELLOW);
    llvm::outs() << "In call to \"" << called->getName() << "\""
                 << " argument (" << argNum << ") may be a temporary file.\n";
  }

  if (errors.empty()) {
    llvm::outs().changeColor(raw_ostream::Colors::GREEN);
    llvm::outs() << "No errors detected\n";
  }
  llvm::outs().resetColor();
}


int
main(int argc, char** argv) {
  // This boilerplate provides convenient stack traces and clean LLVM exit
  // handling. It also initializes the built in support for convenient
  // command line option handling.
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  llvm::PrettyStackTraceProgram X(argc, argv);
  llvm_shutdown_obj shutdown;
  cl::HideUnrelatedOptions(filePolicyCategory);
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

  auto* mainFunction = module->getFunction("main");
  if (!mainFunction) {
    llvm::report_fatal_error("Unable to find main function.");
  }

  using Value    = FileValue;
  using Transfer = FilePolicyTransfer;
  using Meet     = FilePolicyMeet;
  using Analysis = analysis::DataflowAnalysis<Value, Transfer, Meet>;
  Analysis analysis{*module, mainFunction};
  auto results = analysis.computeDataflow();

  std::vector<std::pair<llvm::Instruction*, unsigned>> errors;
  for (auto& [context, contextResults] : results) {
    for (auto& [function, functionResults] : contextResults) {
      collectFileRights(functionResults, std::back_inserter(errors));
    }
  }

  printErrors(errors);

  return 0;
}
