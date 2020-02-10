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
          return privileges;
          break;
        }
        case O_RDONLY: {
          return {1 << PLEDGE_RPATH};
          break;
        }
        case O_WRONLY: {
          return {1 << PLEDGE_WPATH};
          break;
        }
        //O_WRONLY|O_TRUNC
        case (1025): {
          return {1 << PLEDGE_WPATH};
          break;
        }
        //O_WRONLY | O_TRUNC | O_CREAT 
        case (1537): {
          return {1 << PLEDGE_WPATH | 1 << PLEDGE_CPATH};
          break;
        }
        default: {
          llvm::errs() << "\n\n FOUND NOTHING";
          llvm::errs() << *cs.getInstruction();
          //exit(0);
          return 0;
          break;
        }
      }
    } else {
      llvm::outs() << "\n Could not get argument for: " << *cs.getInstruction();
      llvm::outs() << "\nArgument " << *arg;
      Privileges privileges{1 << PLEDGE_RPATH};
      privileges |= {1 << PLEDGE_WPATH };
      privileges |= {1 << PLEDGE_CPATH };
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
  libCHandlers.try_emplace( "fprintf", FunctionPrivilegesBuilder(16).build());
  //Check the first argument(file pointer) for tmppath
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
  libCHandlers.try_emplace("fflush", FunctionPrivilegesBuilder(0).build());
  libCHandlers.try_emplace("fork", FunctionPrivilegesBuilder(8208).build());
  libCHandlers.try_emplace("fprintf", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("fputc", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("free", FunctionPrivilegesBuilder(16).build());
  libCHandlers.try_emplace("getopt", FunctionPrivilegesBuilder(16).build());
  //libCHandlers.try_emplace("getpwnam", FunctionPrivilegesBuilder(4199418).build());
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
  libCHandlers.try_emplace("freezero", 16);
  libCHandlers.try_emplace("fwrite", 16);
  libCHandlers.try_emplace("strsep", 0);
  libCHandlers.try_emplace("__b64_ntop", 0);
  libCHandlers.try_emplace("strncasecmp", 0);
  libCHandlers.try_emplace("strncmp", 0);
  libCHandlers.try_emplace("strcasecmp", 0);
  libCHandlers.try_emplace("strpbrk", 0);

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

  /* From slaacd */
  libCHandlers.try_emplace("login_getclass", 0);
  libCHandlers.try_emplace("auth_check_change", 0);
  libCHandlers.try_emplace("auth_close", 0);
  libCHandlers.try_emplace("auth_open", 0);
  libCHandlers.try_emplace("auth_setpwd", 0);
  libCHandlers.try_emplace("bcopy", 0);
  //libCHandlers.try_emplace("execv", 0); in CallGraphAnalyzer.h
  //libCHandlers.try_emplace("execvp", 0); in CallGraphAnalyzer.h
  libCHandlers.try_emplace("fcntl", 0);
  libCHandlers.try_emplace("getenv", 16);
  libCHandlers.try_emplace("llvm.va_copy", 0);
  libCHandlers.try_emplace("llvm.va_end", 0);
  libCHandlers.try_emplace("llvm.va_start", 0);
  libCHandlers.try_emplace("login_getclass", 0);
  libCHandlers.try_emplace("realloc", 16);
  libCHandlers.try_emplace("recallocarray", 16);
  libCHandlers.try_emplace("tcsetattr", 0);
  libCHandlers.try_emplace("ttyslot", 0);
  //libCHandlers.try_emplace("llvm.dbg.declare", 0);
  //libCHandlers.try_emplace("llvm.dbg.value", 0);
  //libCHandlers.try_emplace("llvm.lifetime.end.p0i8", 0);
  //libCHandlers.try_emplace("llvm.lifetime.start.p0i8", 0);
  //libCHandlers.try_emplace("llvm.memcpy.p0i8.p0i8.i32", 0);
  //libCHandlers.try_emplace("llvm.memcpy.p0i8.p0i8.i64", 0);
  //libCHandlers.try_emplace("llvm.memmove.p0i8.p0i8.i64", 0);
  //libCHandlers.try_emplace("llvm.memset.p0i8.i64", 0);
  /*TODO: chroot seems to require no privs. Confirm*/
  libCHandlers.try_emplace("chroot", 0);
  libCHandlers.try_emplace("geteuid", 16);

  /* More handlers for slaacd */
  //libCHandlers.try_emplace("getpwnam", 1 << PLEDGE_GETPW);

  /* AF_UNIX, AF_INET and AF_INET6 handlers*/
  libCHandlers.try_emplace("socket", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  libCHandlers.try_emplace("socketpair", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  libCHandlers.try_emplace("listen", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  libCHandlers.try_emplace("accept4", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  libCHandlers.try_emplace("setsockopt", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  libCHandlers.try_emplace("bind", FunctionPrivilegesBuilder(16, package).add(std::make_unique<Check_socket>(0)).build());
  
  /* imsg handlers for slaacd. None of them should actually require any privileges*/
  libCHandlers.try_emplace("imsg_init", 16);
  libCHandlers.try_emplace("imsg_read", 16);
  libCHandlers.try_emplace("imsg_get", 16);
  libCHandlers.try_emplace("imsg_composev",16);
  libCHandlers.try_emplace("imsg_create", 16);
  libCHandlers.try_emplace("imsg_add", 16);
  libCHandlers.try_emplace("imsg_close", 16);
  libCHandlers.try_emplace("imsg_free", 16);
  libCHandlers.try_emplace("imsg_flush", 16);
  libCHandlers.try_emplace("imsg_clear", 16);
  libCHandlers.try_emplace("ibuf_open", 16);
  libCHandlers.try_emplace("ibuf_dynamic", 16);
  libCHandlers.try_emplace("ibuf_add", 16);
  libCHandlers.try_emplace("ibuf_reserve", 16);
  libCHandlers.try_emplace("ibuf_seek", 16);
  libCHandlers.try_emplace("ibuf_size", 16);
  libCHandlers.try_emplace("ibuf_left", 16);
  libCHandlers.try_emplace("ibuf_close", 16);
  libCHandlers.try_emplace("ibuf_write", 16);
  libCHandlers.try_emplace("ibuf_free", 16);
  libCHandlers.try_emplace("msgbuf_init", 16);
  libCHandlers.try_emplace("msgbuf_clear", 16);
  libCHandlers.try_emplace("msgbuf_write", 16);
  libCHandlers.try_emplace("msgbuf_drain", 16);
  /*More from slaacd.bc*/
  libCHandlers.try_emplace("strerror", 16);
  libCHandlers.try_emplace("gai_strerror", 16);
  libCHandlers.try_emplace("arc4random_uniform", 16);
  libCHandlers.try_emplace("arc4random", 16);
  libCHandlers.try_emplace("arc4random_buf", 16);
  /* Network helpers which seem to require no privileges 
   * Checked with generated privileges and handchecked*/
  libCHandlers.try_emplace("getnameinfo", 16);
  libCHandlers.try_emplace("inet_ntop", 16);
  libCHandlers.try_emplace("asr_run", 16);
  libCHandlers.try_emplace("asr_abort", 16); // From libc/asr
  libCHandlers.try_emplace("asr_abort", 16); // From libc/asr
  libCHandlers.try_emplace("if_freenameindex", 16); // From libc/asr
  libCHandlers.try_emplace("if_nametoindex", 16);
  libCHandlers.try_emplace("if_indextoname", 16);
  libCHandlers.try_emplace("if_nameindex", 16);
  libCHandlers.try_emplace("if_freenameindex", 16);
  libCHandlers.try_emplace("freeifaddrs", 16);
  /* libc pledge generation scripts point towards need for special handling
   * since it calls sysctl According to man page getifaddrs should not 
   * require additional privs */
  libCHandlers.try_emplace("getifaddrs", 16);
  libCHandlers.try_emplace("reallocarray", 16);
  /* WhiteList fork */
  libCHandlers.try_emplace("fork",0);
  /*Tmux stuff*/
  libCHandlers.try_emplace("getprogname",16);
  libCHandlers.try_emplace("stravis",16);

  libCHandlers.try_emplace("send", 0);
  libCHandlers.try_emplace("recv", 0);
  libCHandlers.try_emplace("strtoll", 0);
  libCHandlers.try_emplace("explicit_bzero", 0);
  libCHandlers.try_emplace("Blowfish_initstate", 0);
  libCHandlers.try_emplace("Blowfish_expandstate", 0);
  libCHandlers.try_emplace("Blowfish_expand0state", 0);
  libCHandlers.try_emplace("Blowfish_stream2word", 0);
  libCHandlers.try_emplace("blf_enc", 0);
  libCHandlers.try_emplace("login_getcaptime", 16);
  libCHandlers.try_emplace("login_close", 16);
  libCHandlers.try_emplace("endpwent", 16);
  libCHandlers.try_emplace("pw_dup", 16);
  libCHandlers.try_emplace("auth_mkvalue", 16);
  libCHandlers.try_emplace("ctime", 18);
  libCHandlers.try_emplace("waitpid", 16);
  libCHandlers.try_emplace("fputs", 16);
  libCHandlers.try_emplace("atoll", 0);
  libCHandlers.try_emplace("setusershell", 18);
  libCHandlers.try_emplace("getusershell", 18);
  libCHandlers.try_emplace("endusershell", 16);
  libCHandlers.try_emplace("sleep", 16);
  libCHandlers.try_emplace("getdtablesize", 0);
  libCHandlers.try_emplace("strtok_r", 0);
  libCHandlers.try_emplace("syslog", 16);
  libCHandlers.try_emplace("opendir", 18);
  libCHandlers.try_emplace("readdir", 16);
  libCHandlers.try_emplace("strlcat", 0);
  libCHandlers.try_emplace("closedir", 16);
  libCHandlers.try_emplace("strncpy", 0);
  libCHandlers.try_emplace("raise", 0);
  libCHandlers.try_emplace("strtoul", 0);
  libCHandlers.try_emplace("strcspn", 0);
  libCHandlers.try_emplace("gethostname", 0);
  libCHandlers.try_emplace("wctomb", 0);
  libCHandlers.try_emplace("wcwidth", 0);
  libCHandlers.try_emplace("mbtowc", 0);
  libCHandlers.try_emplace("__mb_cur_max", 0);
  libCHandlers.try_emplace("usleep", 16);
  libCHandlers.try_emplace("setenv", 16);
  libCHandlers.try_emplace("ctime_r", 18);
  libCHandlers.try_emplace("strspn", 0);
  libCHandlers.try_emplace("vis", 0);
  libCHandlers.try_emplace("__b64_pton", 0);
  libCHandlers.try_emplace("globfree", 16);
  libCHandlers.try_emplace("fnmatch", 0);
  libCHandlers.try_emplace("basename", 0);
  libCHandlers.try_emplace("dirname", 0);
  libCHandlers.try_emplace("strndup", 16);
  libCHandlers.try_emplace("strnvis", 0);
  libCHandlers.try_emplace("strvisx", 0);
  libCHandlers.try_emplace("realpath", 22);
  libCHandlers.try_emplace("strstr", 0);
  libCHandlers.try_emplace("qsort", 16);
  libCHandlers.try_emplace("gmtime_r", 18);
  libCHandlers.try_emplace("memmem", 0);
  libCHandlers.try_emplace("getcwd", 16);
  libCHandlers.try_emplace("cfmakeraw", 0);
  libCHandlers.try_emplace("cfgetispeed", 0);
  libCHandlers.try_emplace("cfsetispeed", 0);
  libCHandlers.try_emplace("cfgetospeed", 0);
  libCHandlers.try_emplace("cfsetospeed", 0);
  libCHandlers.try_emplace("strsignal", 0);
  libCHandlers.try_emplace("killpg", 8192);
  libCHandlers.try_emplace("uname", 0);
  libCHandlers.try_emplace("setlocale", 18);
  libCHandlers.try_emplace("nl_langinfo", 0);
  libCHandlers.try_emplace("strcasestr", 0);

  /* tmux unknowns */
  //libCHandlers.try_emplace("strunvis", 0);
  //libCHandlers.try_emplace("setupterm", 0);
  //libCHandlers.try_emplace("tigetstr", 0);
  //libCHandlers.try_emplace("tigetnum", 0);
  //libCHandlers.try_emplace("tigetflag", 0);
  //libCHandlers.try_emplace("killpg", 0);
  //libCHandlers.try_emplace("auth_open",0);
  //libCHandlers.try_emplace("auth_setpwd",0);
  //libCHandlers.try_emplace("auth_check_change",0);
  //libCHandlers.try_emplace("auth_close",0);
  //libCHandlers.try_emplace("del_curterm", 0);
  //libCHandlers.try_emplace("llvm.memmove.p0i8.p0i8.i64", 0);
  //libCHandlers.try_emplace("llvm.va_copy", 0);
  //libCHandlers.try_emplace("memchr", 0);
  //libCHandlers.try_emplace("fcntl", 0);
  //libCHandlers.try_emplace("bcopy", 0);
  //libCHandlers.try_emplace("tparm", 0);
  //
  ///* Bad Privilge Specifications */
  //libCHandlers.try_emplace("execv", 0);
  //libCHandlers.try_emplace("ttyslot", 0);
  //libCHandlers.try_emplace("tcsetattr", 0);
  //libCHandlers.try_emplace("tcgetattr", 0);
  //libCHandlers.try_emplace("tcflush", 0);
  //libCHandlers.try_emplace("execl", 0);
  //libCHandlers.try_emplace("glob", 0);
  //libCHandlers.try_emplace("tcgetpgrp", 0);
  //libCHandlers.try_emplace("system", 0);
  //libCHandlers.try_emplace("ttyname", 0);

  /* Unknown Functions KSH */
  //libCHandlers.try_emplace("llvm.lifetime.start.p0i8", 0);
  //libCHandlers.try_emplace("llvm.lifetime.end.p0i8", 0);
  //libCHandlers.try_emplace("llvm.memcpy.p0i8.p0i8.i64", 0);
  //libCHandlers.try_emplace("llvm.memset.p0i8.i64", 0);
  //libCHandlers.try_emplace("llvm.va_start", 0);
  //libCHandlers.try_emplace("llvm.va_end", 0);
  //libCHandlers.try_emplace("srand_deterministic", 0);
  //libCHandlers.try_emplace("tcsetpgrp", 0);
  //libCHandlers.try_emplace("sigsetjmp", 0);
  //libCHandlers.try_emplace("siglongjmp", 0);
  //libCHandlers.try_emplace("atoi", 0);
  //libCHandlers.try_emplace("fgetc", 0);
  //libCHandlers.try_emplace("feof", 0);
  //libCHandlers.try_emplace("nice", 0);
  //libCHandlers.try_emplace("alarm", 0);
  //libCHandlers.try_emplace("confstr", 0);
  //libCHandlers.try_emplace("getlogin", 0);
  //libCHandlers.try_emplace("isalnum", 0);
  //libCHandlers.try_emplace("isalpha", 0);
  //libCHandlers.try_emplace("isblank", 0);
  //libCHandlers.try_emplace("iscntrl", 0);
  //libCHandlers.try_emplace("isgraph", 0);
  //libCHandlers.try_emplace("islower", 0);
  //libCHandlers.try_emplace("isprint", 0);
  //libCHandlers.try_emplace("ispunct", 0);
  //libCHandlers.try_emplace("isupper", 0);
  //libCHandlers.try_emplace("isxdigit", 0);
  //libCHandlers.try_emplace("rand", 0);
  
  // From cat
  libCHandlers.try_emplace("iscntrl", 0);
  libCHandlers.try_emplace("isascii", 0);
  libCHandlers.try_emplace("toascii", 0);

  // From
  libCHandlers.try_emplace("__errno", 16);
  libCHandlers.try_emplace("err", 16); //Should only use stderr
  libCHandlers.try_emplace("errx",16); //Like err, also exits the program 
  libCHandlers.try_emplace("exit",16);  //Program can always exit
  /* For PAX */
  libCHandlers.try_emplace("_exit", 16);
  //libCHandlers.try_emplace("llvm.cttz.i32", 0);
  libCHandlers.try_emplace("feof", 0);
  libCHandlers.try_emplace("memchr", 0);
  libCHandlers.try_emplace("putenv", 0);
  libCHandlers.try_emplace("strmode", 0);
  libCHandlers.try_emplace("user_from_uid", 0);
  libCHandlers.try_emplace("group_from_gid", 0);
  libCHandlers.try_emplace("regcomp", 0);
  libCHandlers.try_emplace("regerror", 0);
  libCHandlers.try_emplace("regfree", 0);
  libCHandlers.try_emplace("regexec", 0);
  libCHandlers.try_emplace("mktime", 0);
  /* BlackListed from PAX */
  //libCHandlers.try_emplace("options", 16);
  libCHandlers.try_emplace("options", 0); // Cpath etc. Experiment via stub
  libCHandlers.try_emplace("ar_write", 16);
  libCHandlers.try_emplace("fn_match", 0);
  libCHandlers.try_emplace("str_offt", 0);
  // From date
  libCHandlers.try_emplace("puts", 0);
  libCHandlers.try_emplace("getlogin", 0);
  //libCHandlers.try_emplace("logwtmp", 0);
  libCHandlers.try_emplace("atoi", 0);
  libCHandlers.try_emplace("strftime", 0);
  // Moving tmp to cpath
  //libCHandlers.try_emplace("mktemp_internal", 62);
  //libCHandlers.try_emplace("tempnam", 62);
  //libCHandlers.try_emplace("tmpnam", 62);
  //libCHandlers.try_emplace("_mktemp", 62);
  //libCHandlers.try_emplace("mkdtemp", 62);
  //libCHandlers.try_emplace("mkostemp", 62);
  //libCHandlers.try_emplace("mkostemps", 62);
  //libCHandlers.try_emplace("mkstemp", 62);
  //libCHandlers.try_emplace("mkstemps", 62);
  
  //ioctl only used to check if file is tape in DD
  //libCHandlers.try_emplace("ioctl", 0);
  
  //libCHandlers.try_emplace("strftime", 0);
  libCHandlers.try_emplace("freeaddrinfo", 16);
  libCHandlers.try_emplace("getaddrinfo", 16);
  libCHandlers.try_emplace("getservbyname", 16);
  libCHandlers.try_emplace("getservbyport", 16);
  libCHandlers.try_emplace("llvm.dbg.declare", 16);
  libCHandlers.try_emplace("llvm.dbg.value", 16);
  libCHandlers.try_emplace("llvm.lifetime.end.p0i8", 16);
  libCHandlers.try_emplace("llvm.lifetime.start.p0i8", 16);
  libCHandlers.try_emplace("llvm.memcpy.p0i8.p0i8.i64", 16);
  libCHandlers.try_emplace("llvm.memmove.p0i8.p0i8.i64", 16);
  libCHandlers.try_emplace("llvm.memset.p0i8.i64", 16);
  libCHandlers.try_emplace("mktemp", 16);
  libCHandlers.try_emplace("readpassphrase", 16);
  libCHandlers.try_emplace("setrtable", 16);
  libCHandlers.try_emplace("tls_accept_socket", 16);
  libCHandlers.try_emplace("tls_client", 16);
  libCHandlers.try_emplace("tls_close", 16);
  libCHandlers.try_emplace("tls_config_error", 16);
  libCHandlers.try_emplace("tls_config_free", 16);
  libCHandlers.try_emplace("tls_config_insecure_noverifycert", 16);
  libCHandlers.try_emplace("tls_config_insecure_noverifyname", 16);
  libCHandlers.try_emplace("tls_config_new", 16);
  libCHandlers.try_emplace("tls_config_ocsp_require_stapling", 16);
  libCHandlers.try_emplace("tls_config_parse_protocols", 16);
  libCHandlers.try_emplace("tls_config_set_ca_file", 16);
  libCHandlers.try_emplace("tls_config_set_cert_file", 16);
  libCHandlers.try_emplace("tls_config_set_ciphers", 16);
  libCHandlers.try_emplace("tls_config_set_key_file", 16);
  libCHandlers.try_emplace("tls_config_set_ocsp_staple_file", 16);
  libCHandlers.try_emplace("tls_config_set_protocols", 16);
  libCHandlers.try_emplace("tls_config_verify_client_optional", 16);
  libCHandlers.try_emplace("tls_configure", 16);
  libCHandlers.try_emplace("tls_conn_cipher", 16);
  libCHandlers.try_emplace("tls_conn_version", 16);
  libCHandlers.try_emplace("tls_connect_socket", 16);
  libCHandlers.try_emplace("tls_error", 16);
  libCHandlers.try_emplace("tls_free", 16);
  libCHandlers.try_emplace("tls_handshake", 16);
  libCHandlers.try_emplace("tls_init", 16);
  libCHandlers.try_emplace("tls_peer_cert_chain_pem", 16);
  libCHandlers.try_emplace("tls_peer_cert_contains_name", 16);
  libCHandlers.try_emplace("tls_peer_cert_hash", 16);
  libCHandlers.try_emplace("tls_peer_cert_issuer", 16);
  libCHandlers.try_emplace("tls_peer_cert_notafter", 16);
  libCHandlers.try_emplace("tls_peer_cert_notbefore", 16);
  libCHandlers.try_emplace("tls_peer_cert_provided", 16);
  libCHandlers.try_emplace("tls_peer_cert_subject", 16);
  libCHandlers.try_emplace("tls_peer_ocsp_cert_status", 16);
  libCHandlers.try_emplace("tls_peer_ocsp_crl_reason", 16);
  libCHandlers.try_emplace("tls_peer_ocsp_next_update", 16);
  libCHandlers.try_emplace("tls_peer_ocsp_response_status", 16);
  libCHandlers.try_emplace("tls_peer_ocsp_result", 16);
  libCHandlers.try_emplace("tls_peer_ocsp_revocation_time", 16);
  libCHandlers.try_emplace("tls_peer_ocsp_this_update", 16);
  libCHandlers.try_emplace("tls_peer_ocsp_url", 16);
  libCHandlers.try_emplace("tls_read", 16);
  libCHandlers.try_emplace("tls_server", 16);
  libCHandlers.try_emplace("tls_write", 16);

  // For netcat, infinite loop.
  libCHandlers.try_emplace("readwrite", 16);
};
