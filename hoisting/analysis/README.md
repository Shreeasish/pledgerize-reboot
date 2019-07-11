# Notes

The directory /libc-wp/function-closures/ has the specifications for libc calls.
Sysctl seems to be the latest specification generated (Reading commit logs from github to CallGraphAnalyzer.cpp).
Refer to Trello Card, [Fixing Specifications](https://trello.com/c/qe84D73L/49-fixing-specifications) for more information.

---------------------------

```
cflow command used, indent pattern '    |' (4 spaces with a bar)
cflow -i _s --level begin='' --level '0=    |' --level '1=    |' --level end0='' --level end1='' *.c | less
```
promise_value is used to hold the value for a particular promise -- stdio <--> 1<<6  -- From pledge.h -- Done

		syscall\_bitset holds the bitset for each syscall -- open <--> 1<<4 | 1<<6 -- From kern\_pledge.c

promise names also needs to be added -- From kern\_pledge.c -- Done

syscallmacro\_syscall maps SYS\_X to X -- -- from 

## Explosive libc Functions
1. fctl            - flock
2. mmap, mprotect  - mmap, mprotect
3. fstat           - rpath, wpath, tmppath
4. sysctl          - vminfo, ps
5. ioctl           - audio, pf, tty

Tested by removing each corresponding entry from `syscallManMap` in CallGraphAnalyzer.h and comparing against 
naive (sourced from man pages) Syscall-Promise Map for the functions `asprintf, fputs, fopen`

* `getcwd` seems to require rpath and wpath despite removing fctl - confirm later.
* The script finfo can be used to check for promise requirements for a function. Quick reference for using style
 `./finfo -k all-closures-stripped-man -m all-closures-stripped-ioctl fopen` (beware shebanged to use my conda setup)

### `ioctl` Flag Information
1. DTYPE\_SOCKET -- Used in kern\_pledge.c 
Information about it available [here](https://books.google.ca/books?id=6H9AxyFd0v0C&pg=PT85&lpg=PT85&dq=DTYPE_SOCKET&source=bl&ots=b7iH8ubOhG&sig=ctuS9mddT-JD845d-kpvzsMjnC4&hl=en&sa=X&ved=0ahUKEwin37y13YPcAhVSCTQIHYn7BLsQ6AEILjAB#v=onepage&q=DTYPE_SOCKET&f=false)
2. TIOCDRAIN    -- Used in termios/tcdrain.c
No handle for TIOCDRAIN kern\_pledge.c
3. TIOCGSID	-- [Used to get session ID for the tty](http://man7.org/linux/man-pages/man3/tcgetsid.3.html)
No handle in kern\_pledge.c; similar to TIOCDRAIN
4. TIOCSPGRP	-- tcsetpgrp.c
Needs more than one privilege (kerni\_pledge.c:1127) - PLEDGE\_PROC and PLEDGE\_TTY
5. PTMGET	-- posix\_pty.c
Requires PLEDGE\_TTY and either of PLEDGE\_RPATH or PLEDGE\_WPATH. Conservatively requires all three. May handle later in the static analysis


### `fcntl` notes
1. F_SETLK, F_SETLKW, F_GETLK flags require `FLOCK` privileges
2. The only libc function to use any of them is `lockf` (libc/gen/lockf.c)
3. `lockf` itself seems not to be used anywhere in libc.
4. VERIFY: `lockf` is mapped in the function/syscall-bitset map; left to be handled in higher level analysis.
5. F_SETOWN     -- Requires `PROC`.  `net/rcmd.c:137:fcntl(s, F_SETOWN, pid);` 
6. TODO: Cleanup fcntl handler (squash cases) 
7. `fcntl` is mangled and wrapped inside libc 
`libc_x` -> `_libc_fcntl_cancel` -> `fcntl` (actual syscall which is wrapped with the `HIDDEN(X)` macro) 

### Additional Notes about PLEDGE_FLOCK
1. `flock` (System Call) itself is only called twice inside libc, both from yp_dobind
2. No special handling for the syscall itself

### PLEDGE_PROTEXEC notes
1. Allows the use of `PROT_EXEC` flag with `mmap` and `mprotect`
2. `PROT_EXEC` has no uses inside libc. Verified with `ag 'PROT_EXEC'` and CallgraphAnalyzer
3. Required privileges changed for `mmap` and `mprotect` in CallGraphAnalyzer.h. (Set to `PLEDGE_STDIO` only)

### `sysctl` notes
1. Call locations for `sysctl` can be found in sysctl-loc
2. Aside from a call from getifaddr, sysctl requires no privileges inside libc
3. getifaddr seems to be in a catchall in kern_pledge.c (line 830)
4. The sysctl call inside `getifaddr` is allowed with any of `PLEDGE_ROUTE | PLEDGE_INET | PLEDGE_DNS`. Verified on testbench for OpenBSD.
5. `sysconf` holds several calls to sysctl in a switch case style. 
`sysconf` itself however is called only once in libc and does not require any privileges
7. TODO: Document handwritten notes for sysctl

### `fstat` notes
1. fstat belongs to the family of stat, fstat, and lstat.
2. fstat itself does not require any privileges, the privileges are checked beforehand when the file descripter is obtained.
3. Verified on OpenBSD with O_RDONLY, O_WRONLY for open.
4. stat and lstat have a handful of uses inside libc, fstat is more prevalent.
----------------------
The provided `filepolicy` analysis identifies simple errors in using fread,
fwrite, and fclose where they may potentially be called on files that have
already been closed.

The provided `constant-propagation` analysis identifies simple constant values
that can be determined at compile time. It then prints out the computable
constant arguments to all function calls in the module.

The provided `futurefunctions` analysis uses backward dataflow analysis to
identify the functions that may be called in the future at all call sites
in a program.
