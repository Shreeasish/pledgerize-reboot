#ifndef CALLGRAPHANALYZER_H
#define CALLGRAPHANALYZER_H


#include "llvm/ADT/StringRef.h"

#include <string>
#include <bitset>


enum Promises {
  PLEDGE_ALWAYS, 
  PLEDGE_RPATH,
  PLEDGE_WPATH,
  PLEDGE_CPATH,
  PLEDGE_STDIO,
  PLEDGE_TMPPATH,
  PLEDGE_DNS,
  PLEDGE_INET,
  PLEDGE_FLOCK,
  PLEDGE_UNIX,
  PLEDGE_ID,
  PLEDGE_TAPE,
  PLEDGE_GETPW,
  PLEDGE_PROC,
  PLEDGE_SETTIME,
  PLEDGE_FATTR,
  PLEDGE_PROTEXEC,
  PLEDGE_TTY,
  PLEDGE_SENDFD,
  PLEDGE_RECVFD,
  PLEDGE_EXEC,
  PLEDGE_ROUTE,
  PLEDGE_MCAST,
  PLEDGE_VMINFO,
  PLEDGE_PS,
  PLEDGE_DISKLABEL,
  PLEDGE_PF,
  PLEDGE_AUDIO,
  PLEDGE_DPATH,
  PLEDGE_DRM,
  PLEDGE_VMM,
  PLEDGE_CHOWN,
  PLEDGE_CHOWNUID,
  PLEDGE_BPF,
  PLEDGE_ERROR,
  PLEDGE_USERSET,
  PLEDGE_STATLIE,
  PLEDGE_YPACTIVE,
  COUNT,
};

using SyscallBitsetMap = llvm::StringMap<std::bitset<COUNT>>;

const llvm::StringRef 
PromiseNames[] {
  "rpath", "wpath", "cpath", "stdio", "tmppath", "dns", "inet", "flock", "unix",
      "id", "tape", "getpw", "proc", "settime", "fattr", "protexec", "tty",
      "sendfd", "recvfd", "exec", "route", "mcast", "vminfo", "ps", "disklabel",
      "pf", "audio", "dpath", "drm", "vmm", "chown", "bpf", "error"
};

// struct syscallStruct {
//     std::string syscallName;
//     std::bitset<COUNT> pledges;
// };


static SyscallBitsetMap
syscallBitsetMap{
    // {"exit", std::bitset<COUNT>().set(PLEDGE_ALWAYS)}, //Set nothing for ALWAYS
    // {"kbind", std::bitset<COUNT>().set(PLEDGE_ALWAYS)},
    // {"__get_tcb", std::bitset<COUNT>().set(PLEDGE_ALWAYS)},
    // {"__set_tcb", std::bitset<COUNT>().set(PLEDGE_ALWAYS)},
    // {"pledge", std::bitset<COUNT>().set(PLEDGE_ALWAYS)},
    // {"sendsyslog", std::bitset<COUNT>().set(PLEDGE_ALWAYS)},
    // {"thrkill", std::bitset<COUNT>().set(PLEDGE_ALWAYS)},
    // {"utrace", std::bitset<COUNT>().set(PLEDGE_ALWAYS)},
    {"getuid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"geteuid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getresuid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getgid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getegid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getresgid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getgroups", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getlogin_r", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getpgrp", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getpgid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getppid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getsid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getthrid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getrlimit", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getrtable", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"gettimeofday", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getdtablecount", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getrusage", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"issetugid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"clock_getres", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"clock_gettime", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getpid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sysctl", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getentropy", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"madvise", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"minherit", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"mmap", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"mprotect", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"mquery", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"munmap", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"msync", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"break", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"umask", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"read", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"readv", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pread", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"preadv", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"write", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"writev", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pwrite", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pwritev", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"recvmsg", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"recvfrom",
     std::bitset<COUNT>().set(PLEDGE_STDIO)
         | std::bitset<COUNT>().set(PLEDGE_YPACTIVE)},
    {"ftruncate", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"lseek", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fpathconf", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sendto",
     std::bitset<COUNT>().set(PLEDGE_STDIO)
         | std::bitset<COUNT>().set(PLEDGE_YPACTIVE)},
    {"sendmsg", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"nanosleep", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sigaltstack", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sigprocmask", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sigsuspend", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sigaction", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sigreturn", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sigpending", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getitimer", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"setitimer", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"poll", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"ppoll", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"kevent", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"kqueue", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"select", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pselect", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fstat", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fsync", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"setsockopt", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getsockopt", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fcntl", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"close", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"dup", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"dup2", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"dup3", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"closefrom", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"shutdown", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fchdir", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pipe", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pipe2", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"socketpair", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"wait4", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"kill", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"ioctl", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"open", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"stat", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"access", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"readlink", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"adjtime", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"adjfreq", std::bitset<COUNT>().set(PLEDGE_SETTIME)},
    {"settimeofday", std::bitset<COUNT>().set(PLEDGE_SETTIME)},
    {"__tfork", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sched_yield", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"futex", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"__thrsleep", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"__thrwakeup", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"__threxit", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"__thrsigdivert", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fork", std::bitset<COUNT>().set(PLEDGE_PROC)},
    {"vfork", std::bitset<COUNT>().set(PLEDGE_PROC)},
    {"setpgid", std::bitset<COUNT>().set(PLEDGE_PROC)},
    {"setsid", std::bitset<COUNT>().set(PLEDGE_PROC)},
    {"setrlimit",
     std::bitset<COUNT>().set(PLEDGE_PROC)
         | std::bitset<COUNT>().set(PLEDGE_ID)},
    {"getpriority",
     std::bitset<COUNT>().set(PLEDGE_PROC)
         | std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setpriority",
     std::bitset<COUNT>().set(PLEDGE_PROC)
         | std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setuid", std::bitset<COUNT>().set(PLEDGE_ID)},
    {"seteuid", std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setreuid", std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setresuid", std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setgid", std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setegid", std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setregid", std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setresgid", std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setgroups", std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setlogin", std::bitset<COUNT>().set(PLEDGE_ID)},
    {"execve", std::bitset<COUNT>().set(PLEDGE_EXEC)},
    {"chdir", std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"openat",
     std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_WPATH)},
    {"fstatat",
     std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_WPATH)},
    {"faccessat",
     std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_WPATH)},
    {"readlinkat",
     std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_WPATH)},
    {"lstat",
     std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_WPATH)
         | std::bitset<COUNT>().set(PLEDGE_TMPPATH)},
    {"truncate", std::bitset<COUNT>().set(PLEDGE_WPATH)},
    {"rename",
     std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"rmdir", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"renameat", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"link", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"linkat", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"symlink", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"symlinkat", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"unlink",
     std::bitset<COUNT>().set(PLEDGE_CPATH)
         | std::bitset<COUNT>().set(PLEDGE_TMPPATH)},
    {"unlinkat", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"mkdir", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"mkdirat", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"mkfifo", std::bitset<COUNT>().set(PLEDGE_DPATH)},
    {"mknod", std::bitset<COUNT>().set(PLEDGE_DPATH)},
    {"revoke", std::bitset<COUNT>().set(PLEDGE_TTY)},
    {"__getcwd",
     std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_WPATH)},
    {"getdents", std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"getfsstat", std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"statfs", std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"fstatfs", std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"pathconf", std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"utimes", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"futimes", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"utimensat", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"futimens", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"chmod", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"fchmod", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"fchmodat", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"chflags", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"chflagsat", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"fchflags", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"chown", std::bitset<COUNT>().set(PLEDGE_CHOWN)},
    {"fchownat", std::bitset<COUNT>().set(PLEDGE_CHOWN)},
    {"lchown", std::bitset<COUNT>().set(PLEDGE_CHOWN)},
    {"fchown", std::bitset<COUNT>().set(PLEDGE_CHOWN)},
    {"socket",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)
         | std::bitset<COUNT>().set(PLEDGE_DNS)
         | std::bitset<COUNT>().set(PLEDGE_YPACTIVE)},
    {"connect",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)
         | std::bitset<COUNT>().set(PLEDGE_DNS)
         | std::bitset<COUNT>().set(PLEDGE_YPACTIVE)},
    {"bind",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)
         | std::bitset<COUNT>().set(PLEDGE_DNS)
         | std::bitset<COUNT>().set(PLEDGE_YPACTIVE)},
    {"getsockname",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)
         | std::bitset<COUNT>().set(PLEDGE_DNS)
         | std::bitset<COUNT>().set(PLEDGE_YPACTIVE)},
    {"listen",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"accept4",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"accept",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"getpeername",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"flock",
     std::bitset<COUNT>().set(PLEDGE_FLOCK)
         | std::bitset<COUNT>().set(PLEDGE_YPACTIVE)},
    {"swapctl", std::bitset<COUNT>().set(PLEDGE_VMINFO)},
};

#endif