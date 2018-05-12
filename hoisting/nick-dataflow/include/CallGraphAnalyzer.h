#ifndef CALLGRAPHANALYZER_H
#define CALLGRAPHANALYZER_H


#include "llvm/ADT/StringRef.h"

#include <string>
#include <bitset>


enum Promises {
//   PLEDGE_ALWAYS,
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
  COUNT
};


const llvm::StringRef PromiseNames[] {
  "rpath", "wpath", "cpath", "stdio", "tmppath", "dns", "inet", "flock", "unix",
      "id", "tape", "getpw", "proc", "settime", "fattr", "protexec", "tty",
      "sendfd", "recvfd", "exec", "route", "mcast", "vminfo", "ps", "disklabel",
      "pf", "audio", "dpath", "drm", "vmm", "chown", "bpf", "error"
};

struct syscall {
    std::string syscallName;
    std::bitset<COUNT> pledges;
}

const syscall pledge_syscalls[] = {

	"exit" = PLEDGE_ALWAYS,
	"kbind" = PLEDGE_ALWAYS,
	"__get_tcb" = PLEDGE_ALWAYS,
	"__set_tcb" = PLEDGE_ALWAYS,
	"pledge" = PLEDGE_ALWAYS,
	"sendsyslog" = PLEDGE_ALWAYS,	/* stack protector reporting */
	"thrkill" = PLEDGE_ALWAYS,		/* raise, abort, stack pro */
	"utrace" = PLEDGE_ALWAYS,		/* ltrace(1) from ld.so */

	/* "getting" information about self is considered safe */
	"getuid" = PLEDGE_STDIO,
	"geteuid" = PLEDGE_STDIO,
	"getresuid" = PLEDGE_STDIO,
	"getgid" = PLEDGE_STDIO,
	"getegid" = PLEDGE_STDIO,
	"getresgid" = PLEDGE_STDIO,
	"getgroups" = PLEDGE_STDIO,
	"getlogin_r" = PLEDGE_STDIO,
	"getpgrp" = PLEDGE_STDIO,
	"getpgid" = PLEDGE_STDIO,
	"getppid" = PLEDGE_STDIO,
	"getsid" = PLEDGE_STDIO,
	"getthrid" = PLEDGE_STDIO,
	"getrlimit" = PLEDGE_STDIO,
	"getrtable" = PLEDGE_STDIO,
	"gettimeofday" = PLEDGE_STDIO,
	"getdtablecount" = PLEDGE_STDIO,
	"getrusage" = PLEDGE_STDIO,
	"issetugid" = PLEDGE_STDIO,
	"clock_getres" = PLEDGE_STDIO,
	"clock_gettime" = PLEDGE_STDIO,
	"getpid" = PLEDGE_STDIO,

	/*
	 * Almost exclusively read-only, Very narrow subset.
	 * Use of "route", "inet", "dns", "ps", or "vminfo"
	 * expands access.
	 */
	"sysctl" = PLEDGE_STDIO,

	/* Support for malloc(3) family of operations */
	"getentropy" = PLEDGE_STDIO,
	"madvise" = PLEDGE_STDIO,
	"minherit" = PLEDGE_STDIO,
	"mmap" = PLEDGE_STDIO,
	"mprotect" = PLEDGE_STDIO,
	"mquery" = PLEDGE_STDIO,
	"munmap" = PLEDGE_STDIO,
	"msync" = PLEDGE_STDIO,
	"break" = PLEDGE_STDIO,

	"umask" = PLEDGE_STDIO,

	/* read/write operations */
	"read" = PLEDGE_STDIO,
	"readv" = PLEDGE_STDIO,
	"pread" = PLEDGE_STDIO,
	"preadv" = PLEDGE_STDIO,
	"write" = PLEDGE_STDIO,
	"writev" = PLEDGE_STDIO,
	"pwrite" = PLEDGE_STDIO,
	"pwritev" = PLEDGE_STDIO,
	"recvmsg" = PLEDGE_STDIO,
	"recvfrom" = PLEDGE_STDIO | PLEDGE_YPACTIVE,
	"ftruncate" = PLEDGE_STDIO,
	"lseek" = PLEDGE_STDIO,
	"fpathconf" = PLEDGE_STDIO,

	/*
	 * Address selection required a network pledge ("inet",
	 * "unix", "dns".
	 */
	"sendto" = PLEDGE_STDIO | PLEDGE_YPACTIVE,

	/*
	 * Address specification required a network pledge ("inet",
	 * "unix", "dns".  SCM_RIGHTS requires "sendfd" or "recvfd".
	 */
	"sendmsg" = PLEDGE_STDIO,

	/* Common signal operations */
	"nanosleep" = PLEDGE_STDIO,
	"sigaltstack" = PLEDGE_STDIO,
	"sigprocmask" = PLEDGE_STDIO,
	"sigsuspend" = PLEDGE_STDIO,
	"sigaction" = PLEDGE_STDIO,
	"sigreturn" = PLEDGE_STDIO,
	"sigpending" = PLEDGE_STDIO,
	"getitimer" = PLEDGE_STDIO,
	"setitimer" = PLEDGE_STDIO,

	/*
	 * To support event driven programming.
	 */
	"poll" = PLEDGE_STDIO,
	"ppoll" = PLEDGE_STDIO,
	"kevent" = PLEDGE_STDIO,
	"kqueue" = PLEDGE_STDIO,
	"select" = PLEDGE_STDIO,
	"pselect" = PLEDGE_STDIO,

	"fstat" = PLEDGE_STDIO,
	"fsync" = PLEDGE_STDIO,

	"setsockopt" = PLEDGE_STDIO,	/* narrow whitelist */
	"getsockopt" = PLEDGE_STDIO,	/* narrow whitelist */

	/* F_SETOWN requires PLEDGE_PROC */
	"fcntl" = PLEDGE_STDIO,

	"close" = PLEDGE_STDIO,
	"dup" = PLEDGE_STDIO,
	"dup2" = PLEDGE_STDIO,
	"dup3" = PLEDGE_STDIO,
	"closefrom" = PLEDGE_STDIO,
	"shutdown" = PLEDGE_STDIO,
	"fchdir" = PLEDGE_STDIO,	/* XXX consider tightening */

	"pipe" = PLEDGE_STDIO,
	"pipe2" = PLEDGE_STDIO,
	"socketpair" = PLEDGE_STDIO,

	"wait4" = PLEDGE_STDIO,

	/*
	 * Can kill self with "stdio".  Killing another pid
	 * requires "proc"
	 */
	"kill" = PLEDGE_STDIO,

	/*
	 * FIONREAD/FIONBIO for "stdio"
	 * Other ioctl are selectively allowed based upon other pledges.
	 */
	"ioctl" = PLEDGE_STDIO,

	/*
	 * Path access/creation calls encounter many extensive
	 * checks are done during namei()
	 */
	"open" = PLEDGE_STDIO,
	"stat" = PLEDGE_STDIO,
	"access" = PLEDGE_STDIO,
	"readlink" = PLEDGE_STDIO,

	"adjtime" = PLEDGE_STDIO,   /* setting requires "settime" */
	"adjfreq" = PLEDGE_SETTIME,
	"settimeofday" = PLEDGE_SETTIME,

	/*
	 * Needed by threaded programs
	 * XXX should we have a new "threads"?
	 */
	"__tfork" = PLEDGE_STDIO,
	"sched_yield" = PLEDGE_STDIO,
	"futex" = PLEDGE_STDIO,
	"__thrsleep" = PLEDGE_STDIO,
	"__thrwakeup" = PLEDGE_STDIO,
	"__threxit" = PLEDGE_STDIO,
	"__thrsigdivert" = PLEDGE_STDIO,

	"fork" = PLEDGE_PROC,
	"vfork" = PLEDGE_PROC,
	"setpgid" = PLEDGE_PROC,
	"setsid" = PLEDGE_PROC,

	"setrlimit" = PLEDGE_PROC | PLEDGE_ID,
	"getpriority" = PLEDGE_PROC | PLEDGE_ID,

	"setpriority" = PLEDGE_PROC | PLEDGE_ID,

	"setuid" = PLEDGE_ID,
	"seteuid" = PLEDGE_ID,
	"setreuid" = PLEDGE_ID,
	"setresuid" = PLEDGE_ID,
	"setgid" = PLEDGE_ID,
	"setegid" = PLEDGE_ID,
	"setregid" = PLEDGE_ID,
	"setresgid" = PLEDGE_ID,
	"setgroups" = PLEDGE_ID,
	"setlogin" = PLEDGE_ID,

	"execve" = PLEDGE_EXEC,

	"chdir" = PLEDGE_RPATH,
	"openat" = PLEDGE_RPATH | PLEDGE_WPATH,
	"fstatat" = PLEDGE_RPATH | PLEDGE_WPATH,
	"faccessat" = PLEDGE_RPATH | PLEDGE_WPATH,
	"readlinkat" = PLEDGE_RPATH | PLEDGE_WPATH,
	"lstat" = PLEDGE_RPATH | PLEDGE_WPATH | PLEDGE_TMPPATH,
	"truncate" = PLEDGE_WPATH,
	"rename" = PLEDGE_RPATH | PLEDGE_CPATH,
	"rmdir" = PLEDGE_CPATH,
	"renameat" = PLEDGE_CPATH,
	"link" = PLEDGE_CPATH,
	"linkat" = PLEDGE_CPATH,
	"symlink" = PLEDGE_CPATH,
	"symlinkat" = PLEDGE_CPATH,
	"unlink" = PLEDGE_CPATH | PLEDGE_TMPPATH,
	"unlinkat" = PLEDGE_CPATH,
	"mkdir" = PLEDGE_CPATH,
	"mkdirat" = PLEDGE_CPATH,

	"mkfifo" = PLEDGE_DPATH,
	"mknod" = PLEDGE_DPATH,

	"revoke" = PLEDGE_TTY,	/* also requires PLEDGE_RPATH */

	/*
	 * Classify as RPATH|WPATH, because of path information leakage.
	 * WPATH due to unknown use of mk*temp(3) on non-/tmp paths..
	 */
	"__getcwd" = PLEDGE_RPATH | PLEDGE_WPATH,

	/* Classify as RPATH, because these leak path information */
	"getdents" = PLEDGE_RPATH,
	"getfsstat" = PLEDGE_RPATH,
	"statfs" = PLEDGE_RPATH,
	"fstatfs" = PLEDGE_RPATH,
	"pathconf" = PLEDGE_RPATH,

	"utimes" = PLEDGE_FATTR,
	"futimes" = PLEDGE_FATTR,
	"utimensat" = PLEDGE_FATTR,
	"futimens" = PLEDGE_FATTR,
	"chmod" = PLEDGE_FATTR,
	"fchmod" = PLEDGE_FATTR,
	"fchmodat" = PLEDGE_FATTR,
	"chflags" = PLEDGE_FATTR,
	"chflagsat" = PLEDGE_FATTR,
	"fchflags" = PLEDGE_FATTR,

	"chown" = PLEDGE_CHOWN,
	"fchownat" = PLEDGE_CHOWN,
	"lchown" = PLEDGE_CHOWN,
	"fchown" = PLEDGE_CHOWN,

	"socket" = PLEDGE_INET | PLEDGE_UNIX | PLEDGE_DNS | PLEDGE_YPACTIVE,
	"connect" = PLEDGE_INET | PLEDGE_UNIX | PLEDGE_DNS | PLEDGE_YPACTIVE,
	"bind" = PLEDGE_INET | PLEDGE_UNIX | PLEDGE_DNS | PLEDGE_YPACTIVE,
	"getsockname" = PLEDGE_INET | PLEDGE_UNIX | PLEDGE_DNS | PLEDGE_YPACTIVE,

	"listen" = PLEDGE_INET | PLEDGE_UNIX,
	"accept4" = PLEDGE_INET | PLEDGE_UNIX,
	"accept" = PLEDGE_INET | PLEDGE_UNIX,
	"getpeername" = PLEDGE_INET | PLEDGE_UNIX,

	"flock" = PLEDGE_FLOCK | PLEDGE_YPACTIVE,

	"swapctl" = PLEDGE_VMINFO,	/* XXX should limit to "get" operations */
};

#endif