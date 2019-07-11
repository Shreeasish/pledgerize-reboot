```
sbin/disklabel/disklabel.c
213:		if (op == EDIT || op == EDITOR || aflag) {
214:			if (pledge("stdio rpath wpath cpath disklabel proc "
217:		} else if (fstabfile) {
218:			if (pledge("stdio rpath wpath cpath disklabel", NULL)

commit ae95b2a9e951ed5805863e6c1ae0324109d178d3
Author: beck <beck@openbsd.org>
Date:   Sat May 28 15:46:43 2016 +0000

    Hoist the opendev() call before the pledge because it can ioctl() when
    the provided path is bogus or not a device.
    ok deraadt
```
- Evidence of hoisting.
----
```
commit 9de322083a54ebe6d546ca3c1f5880185f32faa4
Author: jsg <jsg@openbsd.org>
Date:   Mon Jun 13 09:54:01 2016 +0000
correct pledge for disklabel -R -[fF]
    ok tb@

diff --git a/sbin/disklabel/disklabel.c b/sbin/disklabel/disklabel.c
index 38e785e..2708197 100644
--- a/sbin/disklabel/disklabel.c
+++ b/sbin/disklabel/disklabel.c
@@ -1,4 +1,4 @@
-/*     $OpenBSD: disklabel.c,v 1.220 2016/06/01 16:51:54 tb Exp $      */
+/*     $OpenBSD: disklabel.c,v 1.221 2016/06/13 09:54:01 jsg Exp $     */

 /*
  * Copyright (c) 1987, 1993
@@ -219,6 +219,9 @@ main(int argc, char *argv[])
        if (op == EDIT || op == EDITOR || aflag) {
                if (pledge("stdio rpath wpath cpath disklabel proc exec", NULL) == -1)
                        err(1, "pledge");
+       } else if (fstabfile) {
+               if (pledge("stdio rpath wpath cpath disklabel", NULL) == -1)
+                       err(1, "pledge");
        } else {
                if (pledge("stdio rpath wpath disklabel", NULL) == -1)
                        err(1, "pledge");
```
- The added lines show that they had difficulty in recognizing which flags use which system calls and the effects of `fstabfile` were undiscovered.
---

```
commit 9dcdb68ee76edaf00293aa65ae1a34fb56b8dfe4
Author: deraadt <deraadt@openbsd.org>
Date:   Wed Oct 7 05:59:36 2015 +0000

    tame "stdio rpath wpath".  rpath is for localtime() and mktime(),
    while wpath is for logwtmp(), a bit pessimistically since it is not clear
    what could happen.
    This is done AFTER the time is potentially set, since settimeofday() is
    not available to us.  Improvements and tests would be welcome.
```

## Program: bin/date

- Simple program.
- Commit suggests difficulty in privilege restriction.

## Further Support
`commit 9dcdb68ee76edaf00293aa65ae1a34fb56b8dfe4`

---
```
commit cd1f0c0d74c37771e73aa9d0ff98ad740c6c63c0
Author: deraadt <deraadt@openbsd.org>
Date:   Sat Oct 3 05:05:06 2015 +0000

    the chmod & chflags codepaths can use tame "stdio rpath fattr".  the
    chown codepath obviously cannot use tame -- once tame is activated
    the kernel prohibits changing uid/gid on a fd/file.
    ok guenther
```

## Program: bin/chmod 

- Commit suggests that the pledge implementation makes it impossible to perform restrictions on a certain codepath.
---
```
commit 7d0369c1a96b4874dd10c35e3cbdd264d725628a
Author: deraadt <deraadt@openbsd.org>
Date:   Sun Oct 4 15:01:47 2015 +0000

    after dd has opened it's files and done the tape positioning ioctl, we
    can tame "stdio" it.
    ok semarie

diff --git a/bin/dd/dd.c b/bin/dd/dd.c
index 715a1aa..586b47b 100644
--- a/bin/dd/dd.c
+++ b/bin/dd/dd.c
@@ -1,4 +1,4 @@
-/*     $OpenBSD: dd.c,v 1.21 2015/01/16 06:39:31 deraadt Exp $ */
+/*     $OpenBSD: dd.c,v 1.22 2015/10/04 15:01:47 deraadt Exp $ */
 /*     $NetBSD: dd.c,v 1.6 1996/02/20 19:29:06 jtc Exp $       */

 /*-
@@ -149,6 +149,9 @@ setup(void)
        if (out.offset)
                pos_out();

+       if (tame("stdio", NULL) == -1)
+               err(1, "tame");
```
## Program: bin/dd

- Additional commit to restrict privileges after work is done.
---
```
commit 202bfa07e1fbaf7d210468a2ca6c7f4352fc2b25
Author: deraadt <deraadt@openbsd.org>
Date:   Sat Oct 3 03:28:35 2015 +0000

    right at startup, this can tame "stdio cpath rpath wpath".  after getopt
    -h has handled write/creating a file, we can drop to tame "stdio rpath"
    since md5 will only read files after that.
    i believe i involved lteo for this.

```
## Program: bin/md5

- The set of privileges seem small enough
- Git commit shows time dedicated to additional pledging.
```






