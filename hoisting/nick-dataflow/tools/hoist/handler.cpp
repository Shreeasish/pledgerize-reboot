#include "llvm/IR/CallSite.h"
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
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/Constants.h"

#include <bitset>
#include <memory>
#include <string>
#include <iostream>
#include <variant>
#include <unordered_map>

#include "Handler.h"
#include "DataflowAnalysis.h"

// Headers included for checking flags
#include <netinet/in.h>  //mcast

//static llvm::Function*
//getCalledFunction(llvm::CallSite cs) {
//  if (!cs.getInstruction()) {
//    return nullptr;
//  }
//  llvm::Value* called = cs.getCalledValue()->stripPointerCasts();
//  return llvm::dyn_cast<llvm::Function>(called);
//}

class CheckMCAST : public PrivilegeCheckerBase {
public:
  CheckMCAST(int ap) : PrivilegeCheckerBase(ap) {}

  Privileges
  operator()(const llvm::CallSite cs,
             const Context& context,
             AnalysisPackage* AnalysisPackage) override {
    auto* arg   = cs.getArgument(getArgPosition());
    auto* argInt = llvm::dyn_cast<llvm::ConstantInt>(arg);
    if (argInt->getSExtValue() == IP_MULTICAST_IF) {
      return 1 << PLEDGE_MCAST;
    }

    return 0;
  };
};

void
LibCHandlersMap::buildLibCHandlers(AnalysisPackage& package) {
  libCHandlers.try_emplace("fread", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace( "setsockopt", FunctionPrivilegesBuilder(16).add(std::make_unique<CheckMCAST>(2)).build());
  libCHandlers.try_emplace("err", FunctionPrivilegesBuilder(16).build()); //Should only use stderr
  libCHandlers.try_emplace("errx",FunctionPrivilegesBuilder(16).build()); //Like err, also exits the program 
  libCHandlers.try_emplace("exit",FunctionPrivilegesBuilder(0).build());  //Program can always exit
  libCHandlers.try_emplace( "fprintf", FunctionPrivilegesBuilder(16).build()); //Check the first argument(file pointer) for tmppath
  libCHandlers.try_emplace("getopt",FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("isdigit",FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("isspace",FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("localtime",FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("printf",FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("snprintf",FunctionPrivilegesBuilder(16).build()); // According to manpage, works on the char * provided
  libCHandlers.try_emplace("strptime",FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("strtol",FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("strtonum",FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("time",FunctionPrivilegesBuilder(16).build());
  
  libCHandlers.try_emplace("asprintf", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("daemon", FunctionPrivilegesBuilder(8210).build());
  libCHandlers.try_emplace("__errno", FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("errx", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("execvp", FunctionPrivilegesBuilder(1048592).build());
  libCHandlers.try_emplace("exit", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("fflush", FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("fork", FunctionPrivilegesBuilder(8208).build());
  libCHandlers.try_emplace("fprintf", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("fputc", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("free", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("getopt", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("getpwnam", FunctionPrivilegesBuilder(4199418).build());
  libCHandlers.try_emplace("inet_pton", FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("malloc", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("openlog", FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("setproctitle", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("signal", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("strerror", FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("strlcpy", FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("tzset", FunctionPrivilegesBuilder(18).build());
  libCHandlers.try_emplace("vfprintf", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("vsnprintf", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("vsyslog", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("wait", FunctionPrivilegesBuilder(16).build());

  // PULL the remaining try_emplaces from the CallgraphAnalyzer.h
  libCHandlers.try_emplace("__sclose", 16);
  libCHandlers.try_emplace("__smakebuf", 16);
  libCHandlers.try_emplace("__swhatbuf", 16);
  libCHandlers.try_emplace("__sseek", 16);
  libCHandlers.try_emplace("__sread", 16);
  libCHandlers.try_emplace("__srefill", 16);
  libCHandlers.try_emplace("__srget", 16);
  libCHandlers.try_emplace("__svfscanf", 16);
  libCHandlers.try_emplace("__swrite", 16);
  libCHandlers.try_emplace("__swsetup", 16);
  libCHandlers.try_emplace("__vfprintf", 16);
  libCHandlers.try_emplace("__vfwprintf", 16);
  libCHandlers.try_emplace("_mktemp", 62);
  libCHandlers.try_emplace("mktemp_internal", 62);
  libCHandlers.try_emplace("asprintf", 16);
  libCHandlers.try_emplace("dprintf", 16);
  libCHandlers.try_emplace("vdprintf", 16);
  libCHandlers.try_emplace("fdopen", 16);
  libCHandlers.try_emplace("fgetln", 16);
  libCHandlers.try_emplace("fgets", 16);
  libCHandlers.try_emplace("fopen", 16);
  libCHandlers.try_emplace("fprintf", 16);
  libCHandlers.try_emplace("vfprintf", 16);
  libCHandlers.try_emplace("freopen", 16);
  libCHandlers.try_emplace("fscanf", 16);
  libCHandlers.try_emplace("vfscanf", 16);
  libCHandlers.try_emplace("fseek", 16);
  libCHandlers.try_emplace("fseeko", 16);
  libCHandlers.try_emplace("fsetpos", 16);
  libCHandlers.try_emplace("fwprintf", 16);
  libCHandlers.try_emplace("vfwprintf", 16);
  libCHandlers.try_emplace("getdelim", 16);
  libCHandlers.try_emplace("getline", 16);
  libCHandlers.try_emplace("getw", 16);
  libCHandlers.try_emplace("mkdtemp", 62);
  libCHandlers.try_emplace("mkostemp", 62);
  libCHandlers.try_emplace("mkostemps", 62);
  libCHandlers.try_emplace("mkstemp", 62);
  libCHandlers.try_emplace("mkstemps", 62);
  libCHandlers.try_emplace("mktemp", 62);
  libCHandlers.try_emplace("perror", 16);
  libCHandlers.try_emplace("printf", 16);
  libCHandlers.try_emplace("remove", 46);
  libCHandlers.try_emplace("rewind", 16);
  libCHandlers.try_emplace("scanf", 16);
  libCHandlers.try_emplace("setbuf", 16);
  libCHandlers.try_emplace("setvbuf", 16);
  libCHandlers.try_emplace("setbuffer", 16);
  libCHandlers.try_emplace("setlinebuf", 16);
  libCHandlers.try_emplace("snprintf", 16);
  libCHandlers.try_emplace("sprintf", 16);
  libCHandlers.try_emplace("sscanf", 16);
  libCHandlers.try_emplace("swprintf", 16);
  libCHandlers.try_emplace("vswprintf", 16);
  libCHandlers.try_emplace("tempnam", 62);
  libCHandlers.try_emplace("tmpnam", 62);
  libCHandlers.try_emplace("vasprintf", 16);
  libCHandlers.try_emplace("vprintf", 16);
  libCHandlers.try_emplace("vscanf", 16);
  libCHandlers.try_emplace("vsnprintf", 16);
  libCHandlers.try_emplace("vsprintf", 16);
  libCHandlers.try_emplace("vsscanf", 16);
  libCHandlers.try_emplace("vwprintf", 16);
  libCHandlers.try_emplace("wprintf", 16);
};
