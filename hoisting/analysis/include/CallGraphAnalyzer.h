#ifndef CALLGRAPHANALYZER_H
#define CALLGRAPHANALYZER_H

#include "llvm/ADT/StringRef.h"

#include "PromiseDeclarations.h"

#include <string>
#include <bitset>


using SyscallBitsetMap = llvm::StringMap<std::bitset<COUNT>>;
using Privileges = std::bitset<COUNT>;

static SyscallBitsetMap syscallBitsetMap{
    {"mktemp_internal", Privileges{}.set(PLEDGE_CPATH)},
    {"tempnam", Privileges{}.set(PLEDGE_CPATH)},
    {"tmpnam", Privileges{}.set(PLEDGE_CPATH)},
    {"_mktemp", Privileges{}.set(PLEDGE_CPATH)},
    {"mkdtemp", Privileges{}.set(PLEDGE_CPATH)},
    {"mkostemp", Privileges{}.set(PLEDGE_CPATH)},
    {"mkostemps", Privileges{}.set(PLEDGE_CPATH)},
    {"mkstemp", Privileges{}.set(PLEDGE_CPATH)},
    {"mkstemps", Privileges{}.set(PLEDGE_CPATH)},
    {"options",
     Privileges{}.set(PLEDGE_STDIO) | Privileges{}.set(PLEDGE_CPATH)},
    {"fork", Privileges{}.set(PLEDGE_PROC)},
    {"vfork", Privileges{}.set(PLEDGE_PROC)},
    {"execlp", Privileges{}.set(PLEDGE_PROC)},
    {"execv", Privileges{}.set(PLEDGE_EXEC)},
    {"execve", Privileges{}.set(PLEDGE_EXEC)},
    {"execvp", Privileges{}.set(PLEDGE_EXEC)},
    {"sendmsg", Privileges{}.set(PLEDGE_SENDFD)},
    {"recvmsg", Privileges{}.set(PLEDGE_RECVFD)},
    {"recvfrom", Privileges{}.set(PLEDGE_STDIO)},
    {"sendto", Privileges{}.set(PLEDGE_STDIO) | Privileges{}.set(PLEDGE_DNS)},
    /* Custom Input */
    /* End Custom */
    // {"exit", Privileges{}.set(PLEDGE_ALWAYS)}, //Set nothing for
    // ALWAYS
    // {"kbind", Privileges{}.set(PLEDGE_ALWAYS)},
    // {"__get_tcb", Privileges{}.set(PLEDGE_ALWAYS)},
    // {"__set_tcb", Privileges{}.set(PLEDGE_ALWAYS)},
    // {"pledge", Privileges{}.set(PLEDGE_ALWAYS)},
    // {"sendsyslog", Privileges{}.set(PLEDGE_ALWAYS)},
    // {"thrkill", Privileges{}.set(PLEDGE_ALWAYS)},
    // {"utrace", Privileges{}.set(PLEDGE_ALWAYS)},
    {"getuid", Privileges{}.set(PLEDGE_STDIO)},
    {"geteuid", Privileges{}.set(PLEDGE_STDIO)},
    {"getresuid", Privileges{}.set(PLEDGE_STDIO)},
    {"getgid", Privileges{}.set(PLEDGE_STDIO)},
    {"getegid", Privileges{}.set(PLEDGE_STDIO)},
    {"getresgid", Privileges{}.set(PLEDGE_STDIO)},
    {"getgroups", Privileges{}.set(PLEDGE_STDIO)},
    {"getlogin_r", Privileges{}.set(PLEDGE_STDIO)},
    {"getpgrp", Privileges{}.set(PLEDGE_STDIO)},
    {"getpgid", Privileges{}.set(PLEDGE_STDIO)},
    {"getppid", Privileges{}.set(PLEDGE_STDIO)},
    {"getsid", Privileges{}.set(PLEDGE_STDIO)},
    {"getthrid", Privileges{}.set(PLEDGE_STDIO)},
    {"getrlimit", Privileges{}.set(PLEDGE_STDIO)},
    {"getrtable", Privileges{}.set(PLEDGE_STDIO)},
    {"gettimeofday", Privileges{}.set(PLEDGE_STDIO)},
    {"getdtablecount", Privileges{}.set(PLEDGE_STDIO)},
    {"getrusage", Privileges{}.set(PLEDGE_STDIO)},
    {"issetugid", Privileges{}.set(PLEDGE_STDIO)},
    {"clock_getres", Privileges{}.set(PLEDGE_STDIO)},
    {"clock_gettime", Privileges{}.set(PLEDGE_STDIO)},
    {"getpid", Privileges{}.set(PLEDGE_STDIO)},
    {"sysctl", Privileges{}.set(PLEDGE_STDIO)},
    {"getentropy", Privileges{}.set(PLEDGE_STDIO)},
    {"madvise", Privileges{}.set(PLEDGE_STDIO)},
    {"minherit", Privileges{}.set(PLEDGE_STDIO)},
    {"mmap", Privileges{}.set(PLEDGE_STDIO)},
    {"mprotect", Privileges{}.set(PLEDGE_STDIO)},
    {"mquery", Privileges{}.set(PLEDGE_STDIO)},
    {"munmap", Privileges{}.set(PLEDGE_STDIO)},
    {"msync", Privileges{}.set(PLEDGE_STDIO)},
    {"break", Privileges{}.set(PLEDGE_STDIO)},
    {"umask", Privileges{}.set(PLEDGE_STDIO)},
    {"read", Privileges{}.set(PLEDGE_STDIO)},
    {"readv", Privileges{}.set(PLEDGE_STDIO)},
    {"pread", Privileges{}.set(PLEDGE_STDIO)},
    {"preadv", Privileges{}.set(PLEDGE_STDIO)},
    {"write", Privileges{}.set(PLEDGE_STDIO)},
    {"writev", Privileges{}.set(PLEDGE_STDIO)},
    {"pwrite", Privileges{}.set(PLEDGE_STDIO)},
    {"pwritev", Privileges{}.set(PLEDGE_STDIO)},
    // {"ftruncate", Privileges{}.set(PLEDGE_STDIO)},
    {"lseek", Privileges{}.set(PLEDGE_STDIO)},
    // {"fpathconf", Privileges{}.set(PLEDGE_STDIO)},
    {"nanosleep", Privileges{}.set(PLEDGE_STDIO)},
    {"sigaltstack", Privileges{}.set(PLEDGE_STDIO)},
    {"sigprocmask", Privileges{}.set(PLEDGE_STDIO)},
    {"sigsuspend", Privileges{}.set(PLEDGE_STDIO)},
    {"sigaction", Privileges{}.set(PLEDGE_STDIO)},
    {"sigreturn", Privileges{}.set(PLEDGE_STDIO)},
    {"sigpending", Privileges{}.set(PLEDGE_STDIO)},
    {"getitimer", Privileges{}.set(PLEDGE_STDIO)},
    {"setitimer", Privileges{}.set(PLEDGE_STDIO)},
    {"poll", Privileges{}.set(PLEDGE_STDIO)},
    {"ppoll", Privileges{}.set(PLEDGE_STDIO)},
    {"kevent", Privileges{}.set(PLEDGE_STDIO)},
    {"kqueue", Privileges{}.set(PLEDGE_STDIO)},
    {"select", Privileges{}.set(PLEDGE_STDIO)},
    {"pselect", Privileges{}.set(PLEDGE_STDIO)},
    // {"fstat", Privileges{}.set(PLEDGE_STDIO)},
    {"fsync", Privileges{}.set(PLEDGE_STDIO)},
    {"setsockopt", Privileges{}.set(PLEDGE_STDIO)},
    {"getsockopt", Privileges{}.set(PLEDGE_STDIO)},
    // {"fcntl",
    //  Privileges{}.set(
    //      PLEDGE_STDIO)},  // fcntl Intercepted in code, Commented out here
    {"close", Privileges{}.set(PLEDGE_STDIO)},
    {"dup", Privileges{}.set(PLEDGE_STDIO)},
    {"dup2", Privileges{}.set(PLEDGE_STDIO)},
    {"dup3", Privileges{}.set(PLEDGE_STDIO)},
    {"closefrom", Privileges{}.set(PLEDGE_STDIO)},
    {"shutdown", Privileges{}.set(PLEDGE_STDIO)},
    {"fchdir", Privileges{}.set(PLEDGE_STDIO)},
    {"pipe", Privileges{}.set(PLEDGE_STDIO)},
    {"pipe2", Privileges{}.set(PLEDGE_STDIO)},
    //{"socketpair", Privileges{}.set(PLEDGE_STDIO)},
    {"wait4", Privileges{}.set(PLEDGE_STDIO)},
    {"kill", Privileges{}.set(PLEDGE_STDIO)},
    // {"ioctl", Privileges{}.set(PLEDGE_STDIO)},
    // {"open", Privileges{}.set(PLEDGE_SPCL_OPEN)},
    // {"stat", Privileges{}.set(PLEDGE_STDIO)},
    // {"access", Privileges{}.set(PLEDGE_STDIO)},
    // {"readlink", Privileges{}.set(PLEDGE_STDIO)},
    {"adjtime", Privileges{}.set(PLEDGE_STDIO)},
    {"adjfreq", Privileges{}.set(PLEDGE_SETTIME)},
    {"settimeofday", Privileges{}.set(PLEDGE_SETTIME)},
    {"__tfork", Privileges{}.set(PLEDGE_STDIO)},
    {"sched_yield", Privileges{}.set(PLEDGE_STDIO)},
    {"futex", Privileges{}.set(PLEDGE_STDIO)},
    {"__thrsleep", Privileges{}.set(PLEDGE_STDIO)},
    {"__thrwakeup", Privileges{}.set(PLEDGE_STDIO)},
    {"__threxit", Privileges{}.set(PLEDGE_STDIO)},
    {"__thrsigdivert", Privileges{}.set(PLEDGE_STDIO)},
    {"setpgid", Privileges{}.set(PLEDGE_PROC)},
    {"setsid", Privileges{}.set(PLEDGE_PROC)},
    {"setrlimit", Privileges{}.set(PLEDGE_PROC) | Privileges{}.set(PLEDGE_ID)},
    {"getpriority",
     Privileges{}.set(PLEDGE_PROC) | Privileges{}.set(PLEDGE_ID)},
    {"setpriority",
     Privileges{}.set(PLEDGE_PROC) | Privileges{}.set(PLEDGE_ID)},
    {"setuid", Privileges{}.set(PLEDGE_ID)},
    {"seteuid", Privileges{}.set(PLEDGE_ID)},
    {"setreuid", Privileges{}.set(PLEDGE_ID)},
    {"setresuid", Privileges{}.set(PLEDGE_ID)},
    {"setgid", Privileges{}.set(PLEDGE_ID)},
    {"setegid", Privileges{}.set(PLEDGE_ID)},
    {"setregid", Privileges{}.set(PLEDGE_ID)},
    {"setresgid", Privileges{}.set(PLEDGE_ID)},
    {"setgroups", Privileges{}.set(PLEDGE_ID)},
    {"setlogin", Privileges{}.set(PLEDGE_ID)},
    // {"chdir", Privileges{}.set(PLEDGE_RPATH)},
    // {"openat",
    //  Privileges{}.set(PLEDGE_SPCL_OPEN)},
    //  | Privileges{}.set(PLEDGE_WPATH)},
    // {"fstatat",
    // //  Privileges{}.set(PLEDGE_RPATH)
    //      | Privileges{}.set(PLEDGE_WPATH)},
    {"faccessat",
     Privileges{}.set(PLEDGE_RPATH) | Privileges{}.set(PLEDGE_WPATH)},
    // {"readlinkat",
    //  Privileges{}.set(PLEDGE_RPATH)
    //      | Privileges{}.set(PLEDGE_WPATH)},
    // {"lstat",
    //  Privileges{}.set(PLEDGE_RPATH)},
    //  | Privileges{}.set(PLEDGE_WPATH)
    //  | Privileges{}.set(PLEDGE_TMPPATH)},
    {"truncate", Privileges{}.set(PLEDGE_WPATH)},
    // {"rename",
    //  Privileges{}.set(PLEDGE_RPATH)
    //      | Privileges{}.set(PLEDGE_CPATH)},
    {"rmdir", Privileges{}.set(PLEDGE_CPATH)},
    // {"renameat", Privileges{}.set(PLEDGE_CPATH)},
    {"link", Privileges{}.set(PLEDGE_CPATH)},
    {"linkat", Privileges{}.set(PLEDGE_CPATH)},
    {"symlink", Privileges{}.set(PLEDGE_CPATH)},
    {"symlinkat", Privileges{}.set(PLEDGE_CPATH)},
    {"unlink", Privileges{}.set(PLEDGE_CPATH)},
    //  | Privileges{}.set(PLEDGE_TMPPATH)},
    {"unlinkat", Privileges{}.set(PLEDGE_CPATH)},
    {"mkdir", Privileges{}.set(PLEDGE_CPATH)},
    {"mkdirat", Privileges{}.set(PLEDGE_CPATH)},
    // {"mkfifo", Privileges{}.set(PLEDGE_DPATH)},
    // {"mknod", Privileges{}.set(PLEDGE_DPATH)},
    // {"revoke", Privileges{}.set(PLEDGE_TTY)},
    {"__getcwd",
     Privileges{}.set(PLEDGE_RPATH) | Privileges{}.set(PLEDGE_WPATH)},
    {"getdents", Privileges{}.set(PLEDGE_RPATH)},
    {"getfsstat", Privileges{}.set(PLEDGE_RPATH)},
    // {"statfs", Privileges{}.set(PLEDGE_RPATH)},
    {"fstatfs", Privileges{}.set(PLEDGE_RPATH)},
    // {"pathconf", Privileges{}.set(PLEDGE_RPATH)},
    // {"utimes", Privileges{}.set(PLEDGE_FATTR)},
    // {"futimes", Privileges{}.set(PLEDGE_FATTR)},
    // {"utimensat", Privileges{}.set(PLEDGE_FATTR)},
    // {"futimens", Privileges{}.set(PLEDGE_FATTR)},
    // {"chmod", Privileges{}.set(PLEDGE_FATTR)},
    // {"fchmod", Privileges{}.set(PLEDGE_FATTR)},
    // {"fchmodat", Privileges{}.set(PLEDGE_FATTR)},
    // {"chflags", Privileges{}.set(PLEDGE_FATTR)},
    // {"chflagsat", Privileges{}.set(PLEDGE_FATTR)},
    // {"fchflags", Privileges{}.set(PLEDGE_FATTR)},
    // {"chown", Privileges{}.set(PLEDGE_CHOWN)},
    // {"fchownat", Privileges{}.set(PLEDGE_CHOWN)},
    // {"lchown", Privileges{}.set(PLEDGE_CHOWN)},
    // {"fchown", Privileges{}.set(PLEDGE_CHOWN)},
    //{"socket",
    // Privileges{}.set(PLEDGE_INET)
    //     | Privileges{}.set(PLEDGE_UNIX)
    //     | Privileges{}.set(PLEDGE_DNS)
    //     | Privileges{}.set(PLEDGE_YPACTIVE)},
    {"connect",
     Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)
         | Privileges{}.set(PLEDGE_DNS) | Privileges{}.set(PLEDGE_YPACTIVE)},
    {"bind",
     Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)
         | Privileges{}.set(PLEDGE_DNS) | Privileges{}.set(PLEDGE_YPACTIVE)},
    {"getsockname",
     Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)
         | Privileges{}.set(PLEDGE_DNS) | Privileges{}.set(PLEDGE_YPACTIVE)},
    {"listen", Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)},
    {"accept4", Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)},
    {"accept", Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)},
    {"getpeername",
     Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)},
    {"flock", Privileges{}.set(PLEDGE_FLOCK)},
    {"swapctl", Privileges{}.set(PLEDGE_VMINFO)},

    // Merged from manpledges
    {"getpeername",
     Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)},
    {"getsockname",
     Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)},
    {"setsockopt",
     Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)
         | Privileges{}.set(PLEDGE_MCAST)},
    {"getsockopt",
     Privileges{}.set(PLEDGE_INET) | Privileges{}.set(PLEDGE_UNIX)},
    {"getpwnam", Privileges{}.set(PLEDGE_GETPW)},
    {"getpwuid", Privileges{}.set(PLEDGE_GETPW)},
    {"getpwnam_r", Privileges{}.set(PLEDGE_GETPW)},
    {"getpwuid_r", Privileges{}.set(PLEDGE_GETPW)},
    {"getpwnam_shadow", Privileges{}.set(PLEDGE_GETPW)},
    {"getpwuid_shadow", Privileges{}.set(PLEDGE_GETPW)},
    {"setpassent", Privileges{}.set(PLEDGE_GETPW)},
    {"getgrent", Privileges{}.set(PLEDGE_GETPW)},
    {"getgrnam", Privileges{}.set(PLEDGE_GETPW)},
    {"getgrnam_r", Privileges{}.set(PLEDGE_GETPW)},
    {"getgrgid", Privileges{}.set(PLEDGE_GETPW)},
    {"getgrgid_r", Privileges{}.set(PLEDGE_GETPW)},
    {"setgroupent", Privileges{}.set(PLEDGE_GETPW)},
    {"setgrent", Privileges{}.set(PLEDGE_GETPW)},
    {"endgrent", Privileges{}.set(PLEDGE_GETPW)},
    {"getgrouplist", Privileges{}.set(PLEDGE_GETPW)},

    // Namei stuff
    {"statfs", Privileges{}.set(PLEDGE_RPATH)},   // C2 - namei additions
    {"mknod", Privileges{}.set(PLEDGE_DPATH)},    // C2 - Present in kernmap
    {"mknodat", Privileges{}.set(PLEDGE_DPATH)},  // C2
    {"mkfifo", Privileges{}.set(PLEDGE_DPATH)},   // C2 - Present in kernmap
    {"access", Privileges{}.set(PLEDGE_RPATH) | Privileges{}.set(PLEDGE_STDIO)},
    {"faccessat",
     Privileges{}.set(PLEDGE_RPATH)},  // C2 - faccessat calls dofaccessat,
                                       // Basis pledge is RPATH, STDIO removed
    // {"unlink", Privileges{}.set(PLEDGE_CPATH)  // C2 - Base pledge
    // set to cpath
    //   | Privileges{}.set(PLEDGE_TMPPATH)}, // cpath or tmppath may be
    //   used
    // {"unlinkat", Privileges{}.set(PLEDGE_CPATH)},  // C2 - Base
    // pledge set to cpath
    //  Privileges{}.set(PLEDGE_RPATH)
    //      | Privileges{}.set(PLEDGE_WPATH)},
    {"fstat", Privileges{}.set(PLEDGE_STDIO)},
    {"fstatat", Privileges{}.set(PLEDGE_RPATH)},
    {"lstat", Privileges{}.set(PLEDGE_RPATH)},
    {"stat", Privileges{}.set(PLEDGE_RPATH) | Privileges{}.set(PLEDGE_STDIO)},
    {"fpathconf", Privileges{}.set(PLEDGE_STDIO)},
    {"pathconf", Privileges{}.set(PLEDGE_RPATH)},
    {"readlink",
     Privileges{}.set(PLEDGE_STDIO) | Privileges{}.set(PLEDGE_RPATH)},
    {"readlinkat",
     Privileges{}.set(PLEDGE_STDIO) | Privileges{}.set(PLEDGE_RPATH)},
    {"chflags", Privileges{}.set(PLEDGE_FATTR)},
    {"chflagsat", Privileges{}.set(PLEDGE_FATTR)},
    {"fchflags", Privileges{}.set(PLEDGE_FATTR)},
    {"chmod", Privileges{}.set(PLEDGE_FATTR) | Privileges{}.set(PLEDGE_RPATH)},
    {"fchmod", Privileges{}.set(PLEDGE_FATTR) | Privileges{}.set(PLEDGE_RPATH)},
    {"fchmodat",
     Privileges{}.set(PLEDGE_FATTR) | Privileges{}.set(PLEDGE_RPATH)},
    {"chown", Privileges{}.set(PLEDGE_CHOWN) | Privileges{}.set(PLEDGE_RPATH)},
    {"fchownat",
     Privileges{}.set(PLEDGE_CHOWN) | Privileges{}.set(PLEDGE_RPATH)},
    {"lchown", Privileges{}.set(PLEDGE_CHOWN) | Privileges{}.set(PLEDGE_RPATH)},
    {"fchown", Privileges{}.set(PLEDGE_CHOWN)},
    {"utimes", Privileges{}.set(PLEDGE_FATTR) | Privileges{}.set(PLEDGE_RPATH)},
    {"utimensat",
     Privileges{}.set(PLEDGE_FATTR) | Privileges{}.set(PLEDGE_RPATH)},
    {"futimens", Privileges{}.set(PLEDGE_FATTR)},
    {"futimes", Privileges{}.set(PLEDGE_FATTR)},
    {"ftruncate", Privileges{}.set(PLEDGE_STDIO)},
    {"truncate",
     Privileges{}.set(PLEDGE_WPATH) | Privileges{}.set(PLEDGE_RPATH)
         | Privileges{}.set(PLEDGE_FATTR)},
    {"rename", Privileges{}.set(PLEDGE_CPATH) | Privileges{}.set(PLEDGE_RPATH)},
    {"renameat",
     Privileges{}.set(PLEDGE_CPATH) | Privileges{}.set(PLEDGE_RPATH)},
    {"revoke", Privileges{}.set(PLEDGE_TTY) | Privileges{}.set(PLEDGE_RPATH)},
    {"chdir", Privileges{}.set(PLEDGE_RPATH)},

    // Handle Manually.
    //{"open", Privileges{}.set(PLEDGE_SPCL_OPEN)},
    //{"openat", Privileges{}.set(PLEDGE_SPCL_OPEN)},

    /* Handrolled */
    {"getpwnam", Privileges{}.set(PLEDGE_GETPW)}};

static SyscallBitsetMap syscallManMap {
    {"openat", Privileges{}.set(PLEDGE_SPCL_OPEN)},
};
// Verify one off error for count. Should be COUNT - 1 for range
//
template <typename llvmOStream>
static void
printBitset(std::bitset<COUNT> bitv, llvmOStream& outs) {
  for (int i = COUNT - 1; i >= 0; i--) {
    outs << bitv[i];
  }
  outs.flush();
};

//static void
//printBitset(std::bitset<COUNT> bitv, llvm::raw_ostream& outs) {
//  for (int i = COUNT - 1; i >= 0; i--) {
//    outs << bitv[i];
//  }
//  outs.flush();
//};


#endif
