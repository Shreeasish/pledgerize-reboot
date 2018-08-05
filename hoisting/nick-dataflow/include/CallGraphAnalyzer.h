#ifndef CALLGRAPHANALYZER_H
#define CALLGRAPHANALYZER_H

#include "llvm/ADT/StringRef.h"

#include "PromiseDeclarations.h"

#include <string>
#include <bitset>


using SyscallBitsetMap = llvm::StringMap<std::bitset<COUNT>>;

static SyscallBitsetMap 
syscallBitsetMap{
    // {"exit", std::bitset<COUNT>().set(PLEDGE_ALWAYS)}, //Set nothing for
    // ALWAYS
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
    {"fcntl", std::bitset<COUNT>().set(PLEDGE_STDIO)}, // fcntl Intercepted in code, Commented out here
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

static SyscallBitsetMap syscallManMap{
    {"clock_getres", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"clock_gettime", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"close", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"closefrom", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"dup", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"dup2", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"dup3", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fchdir", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fcntl",
     std::bitset<COUNT>().set(PLEDGE_FLOCK)
         | std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fsync", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getdents", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getdtablecount", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getegid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getentropy", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"geteuid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getgid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getgroups", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getitimer", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getlogin", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getpgid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getpgrp", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getpid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getppid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getresgid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getresuid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getrlimit", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getrtable", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getsid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getthrid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"gettimeofday", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getuid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"issetugid", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"kevent", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"kqueue", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"lseek", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"madvise", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"minherit", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"mmap",
     //  std::bitset<COUNT>().set(PLEDGE_PROTEXEC) /* mmap does not need
     //  PLEDGE_PROTEXEC inside libc */
     std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"mprotect",
     //  std::bitset<COUNT>().set(PLEDGE_PROTEXEC) /* mprotect does not need
     //  PLEDGE_PROTEXEC inside libc */
     std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"mquery", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"munmap", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"nanosleep", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pipe", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pipe2", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"poll", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pread", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"preadv", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pwrite", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pwritev", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"read", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"readv", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"recvfrom",
     std::bitset<COUNT>().set(PLEDGE_STDIO)
         | std::bitset<COUNT>().set(PLEDGE_DNS)},
    {"recvmsg",
     std::bitset<COUNT>().set(PLEDGE_RECVFD)
         | std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"select", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sendmsg",
     std::bitset<COUNT>().set(PLEDGE_STDIO)
         | std::bitset<COUNT>().set(PLEDGE_SENDFD)},
    {"sendsyslog", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sendto",
     std::bitset<COUNT>().set(PLEDGE_STDIO)
         | std::bitset<COUNT>().set(PLEDGE_DNS)},
    {"setitimer", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"shutdown", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sigaction", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sigprocmask", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"sigreturn", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"socketpair", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"umask", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"wait4", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"write", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"writev", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"getcwd",
     std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_WPATH)},

    {"getfsstat", std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"link", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"linkat", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"symlink", std::bitset<COUNT>().set( PLEDGE_CPATH)},  // C2 - Appropriate pledges already set
    {"symlinkat", std::bitset<COUNT>().set(PLEDGE_CPATH)},  // C2 - Appropriate pledges already set
    {"mkdir", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"mkdirat", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"rmdir", std::bitset<COUNT>().set(PLEDGE_CPATH)},
    {"mkfifo", std::bitset<COUNT>().set(PLEDGE_DPATH)},
    {"mknod", std::bitset<COUNT>().set(PLEDGE_DPATH)},
    {"socket",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)
         | std::bitset<COUNT>().set(PLEDGE_DNS)},
    {"listen",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"bind",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"connect",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)
         | std::bitset<COUNT>().set(PLEDGE_DNS)},
    {"accept4",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"accept",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"getpeername",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"getsockname",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"setsockopt",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)
         | std::bitset<COUNT>().set(PLEDGE_MCAST)},
    {"getsockopt",
     std::bitset<COUNT>().set(PLEDGE_INET)
         | std::bitset<COUNT>().set(PLEDGE_UNIX)},
    {"flock", std::bitset<COUNT>().set(PLEDGE_FLOCK)},
    {"lockf",
     std::bitset<COUNT>().set(
         PLEDGE_FLOCK)},  // lockf Was already here from before
    {"getpwnam", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getpwuid", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getpwnam_r", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getpwuid_r", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getpwnam_shadow", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getpwuid_shadow", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"setpassent", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getgrent", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getgrnam", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getgrnam_r", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getgrgid", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getgrgid_r", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"setgroupent", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"setgrent", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"endgrent", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"getgrouplist", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"initgroups", std::bitset<COUNT>().set(PLEDGE_GETPW)},
    {"MTIOCG", std::bitset<COUNT>().set(PLEDGE_TAPE)},
    {"MTIOCT", std::bitset<COUNT>().set(PLEDGE_TAPE)},
    {"ioctl",
     std::bitset<COUNT>().set(PLEDGE_AUDIO)
         | std::bitset<COUNT>().set(PLEDGE_PF)
         | std::bitset<COUNT>().set(PLEDGE_TTY)},
    {"fork", std::bitset<COUNT>().set(PLEDGE_PROC)},
    {"vfork", std::bitset<COUNT>().set(PLEDGE_PROC)},
    {"kill", std::bitset<COUNT>().set(PLEDGE_PROC)},
    {"getpriority",
     std::bitset<COUNT>().set(PLEDGE_PROC)
         | std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setpriority",
     std::bitset<COUNT>().set(PLEDGE_PROC)
         | std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setrlimit",
     std::bitset<COUNT>().set(PLEDGE_PROC)
         | std::bitset<COUNT>().set(PLEDGE_ID)},
    {"setpgid", std::bitset<COUNT>().set(PLEDGE_PROC)},
    {"setsid", std::bitset<COUNT>().set(PLEDGE_PROC)},
    {"execve", std::bitset<COUNT>().set(PLEDGE_EXEC)},
    {"settimeofday", std::bitset<COUNT>().set(PLEDGE_SETTIME)},
    {"adjtime", std::bitset<COUNT>().set(PLEDGE_SETTIME)},
    {"adjfreq", std::bitset<COUNT>().set(PLEDGE_SETTIME)},
    {"sysctl",
     std::bitset<COUNT>().set(
         0)},  // Sysctl is handled on a per flag basis in the system
    //  std::bitset<COUNT>().set(PLEDGE_PS)
    //      | std::bitset<COUNT>().set(PLEDGE_VMINFO)},
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
    {"pf", std::bitset<COUNT>().set(PLEDGE_PF)},
    {"bpf", std::bitset<COUNT>().set(PLEDGE_BPF)},

    {"statfs", std::bitset<COUNT>().set(PLEDGE_RPATH)},  // C2 - namei additions
    {"mknod", std::bitset<COUNT>().set(PLEDGE_DPATH)},  // C2 - Present in kernmap
    {"mknodat", std::bitset<COUNT>().set(PLEDGE_DPATH)},  // C2
    {"mkfifo", std::bitset<COUNT>().set(PLEDGE_DPATH)},  // C2 - Present in kernmap
    {"mkfifoat", std::bitset<COUNT>().set(PLEDGE_DPATH)},  // C2
    {"access", std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"faccessat", std::bitset<COUNT>().set(PLEDGE_RPATH)}, // C2 - faccessat calls dofaccessat, Basis pledge is RPATH, STDIO removed
    {"unlink", std::bitset<COUNT>().set(PLEDGE_CPATH)  // C2 - Base pledge set to cpath
      | std::bitset<COUNT>().set(PLEDGE_TMPPATH)}, // cpath or tmppath may be used
    {"unlinkat", std::bitset<COUNT>().set(PLEDGE_CPATH)},  // C2 - Base pledge set to cpath
    //  std::bitset<COUNT>().set(PLEDGE_RPATH)
    //      | std::bitset<COUNT>().set(PLEDGE_WPATH)},
    {"fstat", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fstatat", std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"lstat", std::bitset<COUNT>().set(PLEDGE_RPATH)}, 
    {"stat", std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"fpathconf", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"pathconf", std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"readlinkat",
     std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"readlink", std::bitset<COUNT>().set(PLEDGE_STDIO)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"readlinkat", std::bitset<COUNT>().set(PLEDGE_STDIO)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"chflags", std::bitset<COUNT>().set(PLEDGE_FATTR)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"chflagsat", std::bitset<COUNT>().set(PLEDGE_FATTR)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"fchflags", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"chmod", std::bitset<COUNT>().set(PLEDGE_FATTR)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"fchmod", std::bitset<COUNT>().set(PLEDGE_FATTR)},
        //  | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"fchmodat", std::bitset<COUNT>().set(PLEDGE_FATTR)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"chown", std::bitset<COUNT>().set(PLEDGE_CHOWN)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"fchownat", std::bitset<COUNT>().set(PLEDGE_CHOWN)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"lchown", std::bitset<COUNT>().set(PLEDGE_CHOWN)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"fchown", std::bitset<COUNT>().set(PLEDGE_CHOWN)},
    {"utimes", std::bitset<COUNT>().set(PLEDGE_FATTR)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"utimensat", std::bitset<COUNT>().set(PLEDGE_FATTR)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"futimens", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"futimes", std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"ftruncate", std::bitset<COUNT>().set(PLEDGE_STDIO)},
    {"truncate", std::bitset<COUNT>().set(PLEDGE_WPATH)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)
         | std::bitset<COUNT>().set(PLEDGE_FATTR)},
    {"rename", std::bitset<COUNT>().set(PLEDGE_CPATH)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"renameat", std::bitset<COUNT>().set(PLEDGE_CPATH)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"revoke", std::bitset<COUNT>().set(PLEDGE_TTY)
         | std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"chdir", std::bitset<COUNT>().set(PLEDGE_RPATH)},
    {"open", std::bitset<COUNT>().set(PLEDGE_SPCL_OPEN)}, // Handle Manually.
    {"openat", std::bitset<COUNT>().set(PLEDGE_SPCL_OPEN)},
};
// Verify one off error for count. Should be COUNT - 1 for range
static void
printBitset(std::bitset<COUNT> bitv, llvm::raw_fd_ostream& outs) {
  for (int i = COUNT - 1; i >= 0; i--) {
    outs << bitv[i];
  }
  outs.flush();
};

static void
printBitset(std::bitset<COUNT> bitv, llvm::raw_ostream& outs) {
  for (int i = COUNT - 1; i >= 0; i--) {
    outs << bitv[i];
  }
  outs.flush();
};


#endif