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
