# Loop Conditioned on Load

## Benchmark Commit Exhibiting Bug

```
commit b81641a93c1b3a9551298a592f488aaf0de17e14
Author: Shreeasish Kumar <shreeasish@gmail.com>
Date:   Sat Jul 13 20:42:28 2019 -0700

    minimized.cook_bug/cat.c exhibits the the load dependent loop bug
```

## Description
Loop in src/cat/cat.c:cook_buf depends on a load from a file pointer
via a getc macro expansion. The disjuncts are propagated along the
back edge causing divergence/non-satisfaction of fixed point.

## Fix
Restrict non-topological computation of preconditions
```
commit 539b3275d16a62fa6165a68f077ec3089283048d
Author: Shreeasish Kumar <shreeasish@gmail.com>
Date:   Sat Jul 13 20:49:26 2019 -0700

    Fixed non-termination for cat
```



