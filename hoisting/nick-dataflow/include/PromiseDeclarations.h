#ifndef PROMISEDECLARATIONS_H
#define PROMISEDECLARATIONS_H

#include "llvm/ADT/StringRef.h"

enum
Promises {
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
  PLEDGE_SPCL_SYSCTL,
  PLEDGE_SPCL_OPEN,
  COUNT,
};

const llvm::StringRef PromiseNames[]{ //always added for uniformity
    "always",  "rpath", "wpath",    "cpath", "stdio",     "tmppath", "dns",
    "inet",    "flock", "unix",     "id",    "tape",      "getpw",   "proc",
    "settime", "fattr", "protexec", "tty",   "sendfd",    "recvfd",  "exec",
    "route",   "mcast", "vminfo",   "ps",    "disklabel", "pf",      "audio",
    "dpath",   "drm",   "vmm",      "chown", "bpf",       "error",   "PLEDGE_USERSET",
    "PLEDGE_STATLIE", "PLEDGE_YPACTIVE", "PLEDGE_SPCL_SYSCTL", "COUNT" }; //FIXME:PLEDGE_USERSET, PLEDGE_STATLIE, PLEDGE_YPACTIVE, COUNT 

#endif
