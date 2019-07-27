#include "llvm/IR/CallSite.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Operator.h"

#include <bitset>
#include <string>
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
//}A

/* Unfortunately #defines cannot be limited in scope
 * like O_CREAT, O_READ, MTIOCGET... */
#include <sys/mtio.h>
#include <fcntl.h>

namespace sysdefs {
  
}

class CheckMCAST : public PrivilegeCheckerBase {
public:
  CheckMCAST(int ap) : PrivilegeCheckerBase(ap) {}

  Privileges
  operator()(const llvm::CallSite& cs,
             const Context& context,
             AnalysisPackage* AnalysisPackage) override {
    auto* arg   = cs.getArgument(getArgPosition());
    auto* argInt = llvm::dyn_cast<llvm::ConstantInt>(arg);
    if (argInt->getSExtValue() == IP_MULTICAST_IF) {
      return 1 << PLEDGE_MCAST;
    }

    return 0;
  }
};

/* The dd coreutil, uses ioctl which can interface with tape drives
 * Pledge Handler for ioctl pulls in the global values from sys/mtio.h */
class Check_ioctl : public PrivilegeCheckerBase {
public:
  Check_ioctl(int argPosition) : PrivilegeCheckerBase(argPosition) {}

  Privileges
  operator()(const llvm::CallSite& cs,
             const Context& context,
             AnalysisPackage* AnalysisPackage) override {
    auto* arg     = cs.getArgument(2);
    auto* argType = arg->getType();
    auto* asTypeObj = argType->isPointerTy() 
                      ? argType->getPointerElementType() : argType;
    if (auto* asStructTy = llvm::dyn_cast<llvm::StructType>(asTypeObj)) {
      auto name = asStructTy->getName();
      // struct.mtop indicates a magnetic tape operation
      // struct.mtget corresponds to a magnetic tape
      // check on the fd
      if (name.endswith("mtop")) { 
        return 1 << PLEDGE_TAPE;   
      }
    }
    return 0;
  }
};

class Check_open : public PrivilegeCheckerBase {
public:
  Check_open(int argPosition) : PrivilegeCheckerBase(argPosition) {}

  Privileges
  operator()(const llvm::CallSite& cs,
             const Context& context,
             AnalysisPackage* AnalysisPackage) override {
    auto* arg = cs.getArgument(getArgPosition());
    if (auto* asllvmCInt = llvm::dyn_cast<llvm::ConstantInt>(arg)) {
      auto asInt = asllvmCInt->getSExtValue();
      switch (asInt) {
        case O_RDWR: {
          Privileges privileges{1 << PLEDGE_RPATH};
          privileges |= {1 << PLEDGE_WPATH};
          llvm::outs() << "\n\nFOUND RDWR FOR OPEN";
          llvm::outs() << *cs.getInstruction();
          //exit(0);
          return privileges;
          break;
        }

        case O_RDONLY: {
          return {1 << PLEDGE_RPATH};
          break;
        }

        case O_WRONLY: {
          llvm::outs() << "\n\nFOUND WRONLY FOR OPEN";
          llvm::outs() << cs.getInstruction();
          exit(0);
          return {1 << PLEDGE_WPATH};
          break;
        }

        default: {
          llvm::outs() << "\n\n FOUND NOTHING";
          llvm::outs() << *cs.getInstruction();
          //exit(0);
          return 0;
          break;
        }
      }
    } else {
      llvm::outs() << "\n Could not get argument for: " << *cs.getInstruction();
      llvm::outs() << "\nArgument " << *arg;
      Privileges privileges{1 << PLEDGE_RPATH};
      privileges |= {1 << PLEDGE_WPATH};
      // TODO: CPATH
      return privileges;
    }
  }
};

class Check_fopen : public PrivilegeCheckerBase {
public:
  Check_fopen(int argPosition) : PrivilegeCheckerBase(argPosition) {}
  Privileges
  operator()(const llvm::CallSite& cs,
             const Context& context,
             AnalysisPackage* AnalysisPackage) override {
    auto* arg = cs.getArgument(getArgPosition());
    auto* operand = arg->stripPointerCasts(); // Removes GEPs 0index
    if (auto* gv = llvm::dyn_cast<llvm::GlobalVariable>(operand)) {
      llvm::Constant* gvInitializer = gv->getInitializer();
      /* Prefer to fail if fopen model does not match actual ll. 
       * Else case should conservatively assign all privileges in
       * lieu of a full fledged analysis */
      //llvm::outs() << "\nFound as gv::\"" << *cdSeq;
      auto* asCDA = llvm::dyn_cast<llvm::ConstantDataArray>(gvInitializer); 
      llvm::StringRef permString = asCDA->getAsString();
      llvm::outs() << "\nas String " << permString;

      if (permString.contains("r")) {
        return 1 << PLEDGE_RPATH;
      } else if (permString.contains("w")) {
        return 1 << PLEDGE_WPATH;
      } else if (permString.contains("w") && permString.contains("r")) {
        Privileges privileges{1 << PLEDGE_RPATH};
        privileges |= {1 << PLEDGE_WPATH};
        return privileges;
      } else {
        // TODO: CPATH
        llvm::outs() << "\nfopen strings all funky: " << permString;
        exit(0);
      }
    } else { /* The arguments to fopen might not be constant in which case it
              * would require additional analysis */
      llvm::outs() << "\nBad argument to fopen" << *cs.getInstruction();
      llvm::outs() << "\nin Basic Block\n" << *cs.getInstruction()->getParent();
      llvm::outs() << "\nin Function\n"
                   << cs.getInstruction()->getParent()->getParent()->getName();
      exit(0);
      Privileges privileges{1 << PLEDGE_RPATH};
      privileges |= {1 << PLEDGE_WPATH};
      return privileges;
      // TODO: CPATH
    }
  }
};

class Check_socket : public PrivilegeCheckerBase {
public:
  Check_socket(int argPosition) : PrivilegeCheckerBase(argPosition) {}
  Privileges
  operator()(const llvm::CallSite& cs,
             const Context& context,
             AnalysisPackage* AnalysisPackage) override {
    auto* socketAnalyzer = AnalysisPackage->getSocketAnalyzer();
    auto domain = socketAnalyzer->getSocketResults(cs);
    switch (domain) {
      case SocketType::NONE: {
        return {0};
        break;
      }
      case SocketType::UNIX: {
        return {1 << PLEDGE_UNIX};
        break;
      }
      case SocketType::INET: {
        return {1 << PLEDGE_INET};
        break;
      }
      case SocketType::INET6: {
        return {1 << PLEDGE_INET};
        break;
      }
      default: {
        llvm::errs() << "Unknown domain for " << *cs.getInstruction();
        Privileges privileges{1 << PLEDGE_UNIX};
        privileges |= {1 << PLEDGE_INET};
        return privileges;
      }
    }
  }
};

void
LibCHandlersMap::buildLibCHandlers(AnalysisPackage* package) {
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

  // Generated from final-closures
  libCHandlers.try_emplace("fileno", 0);
  libCHandlers.try_emplace("errc", 16);
  libCHandlers.try_emplace("__swbuf", 16);
  libCHandlers.try_emplace("memcmp", 0);
  libCHandlers.try_emplace("strncat", 0);
  libCHandlers.try_emplace("clearerr", 0);
  libCHandlers.try_emplace("warnc", 16);
  libCHandlers.try_emplace("calloc", 16);
  libCHandlers.try_emplace("warnx", 16);
  libCHandlers.try_emplace("ferror", 0);
  libCHandlers.try_emplace("fts_open", 18);
  libCHandlers.try_emplace("warn", 16);
  libCHandlers.try_emplace("fts_read", 18);
  libCHandlers.try_emplace("fts_set", 0);
  libCHandlers.try_emplace("getc", 16);
  libCHandlers.try_emplace("fclose", 16);
  libCHandlers.try_emplace("fts_close", 16);
  libCHandlers.try_emplace("putc", 16);
 
  // These functions don't need privileges
  // However every program implicitly needs stdio or error
  libCHandlers.try_emplace("strcmp", 0);
  libCHandlers.try_emplace("strcpy", 0);
  libCHandlers.try_emplace("pledge", 0);
  libCHandlers.try_emplace("strrchr", 0);
  libCHandlers.try_emplace("strlen", 0);
	
  
	/* Generated by frpinter.v2 
   * or manual testing on OpenBSD */
  libCHandlers.try_emplace("clock_gettime", 16);
  libCHandlers.try_emplace("strdup", 0);
  libCHandlers.try_emplace("strchr", 0);
  libCHandlers.try_emplace("bsearch", 0);
  libCHandlers.try_emplace("isatty", 0);
  libCHandlers.try_emplace("atexit", 16); 
  // dd only needs stdio since the callback doesn't do anything else

  //libCHandlers.try_emplace( "setsockopt", FunctionPrivilegesBuilder(16).add(std::make_unique<CheckMCAST>(2)).build());
  libCHandlers.try_emplace("ioctl", FunctionPrivilegesBuilder(16).add(std::make_unique<Check_ioctl>(2)).build());
  libCHandlers.try_emplace("open", FunctionPrivilegesBuilder(16).add(std::make_unique<Check_open>(1)).build());
  /* Needs to use wpath, rpath depending on arguments
   * An fwrite/fread used after this doesn't need wpath/rpath
   * since the fd is already open and must have been opened
   * while holding appropriate privileges. */
  libCHandlers.try_emplace("fopen", FunctionPrivilegesBuilder(16).add(std::make_unique<Check_fopen>(1)).build()); 
  
  /* Generated from tests on md5 
   * fwrite generated as stdio only
   * need to test it */
  libCHandlers.try_emplace("strsep", 0);
  libCHandlers.try_emplace("__b64_ntop", 0);
  libCHandlers.try_emplace("strncasecmp", 0);
  libCHandlers.try_emplace("freezero", 16);
  libCHandlers.try_emplace("strncmp", 0);
  libCHandlers.try_emplace("strcasecmp", 0);
  libCHandlers.try_emplace("strpbrk", 0);
  libCHandlers.try_emplace("fwrite", 16);

  /*Used by md5, fprinter generated with no privilege
   * requirements. Makes sense. Hash generation functions
   * shouwl be strictly compute only.*/

  libCHandlers.try_emplace("MD5Final", 0);
  libCHandlers.try_emplace("RMD160Final", 0);
  libCHandlers.try_emplace("SHA1Final", 0);
  libCHandlers.try_emplace("SHA224Final", 0);
  libCHandlers.try_emplace("SHA256Final", 0);
  libCHandlers.try_emplace("SHA384Final", 0);
  libCHandlers.try_emplace("SHA512_256Final", 0);
  libCHandlers.try_emplace("SHA512Final", 0);
  libCHandlers.try_emplace("MD5End",16);
  libCHandlers.try_emplace("RMD160End",16);
  libCHandlers.try_emplace("SHA1End", 16);
  libCHandlers.try_emplace("SHA224End", 16);
  libCHandlers.try_emplace("SHA256End", 16);
  libCHandlers.try_emplace("SHA384End", 16);
  libCHandlers.try_emplace("SHA512_256End", 16);
  libCHandlers.try_emplace("SHA512End", 16);
  libCHandlers.try_emplace("MD5Update", 0);
  libCHandlers.try_emplace("RMD160Update", 0);
  libCHandlers.try_emplace("SHA224End", 16);
  libCHandlers.try_emplace("SHA1Update", 0);
  libCHandlers.try_emplace("SHA256Update", 0);
  libCHandlers.try_emplace("SHA512Update", 0);
  libCHandlers.try_emplace("MD5Init", 0);
  libCHandlers.try_emplace("RMD160Init", 0);
  libCHandlers.try_emplace("SHA1Init", 0);
  libCHandlers.try_emplace("SHA224Init", 0);
  libCHandlers.try_emplace("SHA256Init", 0);
  libCHandlers.try_emplace("SHA384Init", 0);
  libCHandlers.try_emplace("SHA512_256Init", 0);
  libCHandlers.try_emplace("SHA512Init", 0);

  // Handrolled. Doesn't exist in spec.
  libCHandlers.try_emplace("SHA224Update", 0);  
  libCHandlers.try_emplace("SHA384Update", 0);  
  libCHandlers.try_emplace("SHA512_256Update", 0);  

  // For slaacd
  /* Stubbed functions instead of including 
   * them x_errx exits rest just need stdio */
  libCHandlers.try_emplace("event_err", 16);   
  libCHandlers.try_emplace("event_errx", 16);
  libCHandlers.try_emplace("event_msgx", 16); 

  libCHandlers.try_emplace("getenv", 16);
  libCHandlers.try_emplace("imsg_compose", 16);
  libCHandlers.try_emplace("imsg_flush", 16);
  libCHandlers.try_emplace("imsg_init", 16);
  libCHandlers.try_emplace("msgbuf_clear", 16);
  libCHandlers.try_emplace("msgbuf_write", 16);
  libCHandlers.try_emplace("realloc", 16);

  /*TODO: chroot seems to require no privs. Confirm*/
  libCHandlers.try_emplace("chroot", 0);

  /* More handlers for slaacd */
  //libCHandlers.try_emplace("getpwnam", 1 << PLEDGE_GETPW);
  libCHandlers.try_emplace("geteuid", 16);

  /* AF_UNIX, AF_INET and AF_INET6 handlers*/
  libCHandlers.try_emplace("socket", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  libCHandlers.try_emplace("socketpair", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  libCHandlers.try_emplace("listen", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  libCHandlers.try_emplace("accept4", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  libCHandlers.try_emplace("setsockopt", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  libCHandlers.try_emplace("bind", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
};
