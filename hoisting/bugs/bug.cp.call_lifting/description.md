# Symptom
State Explosion: Massive Number of disjuncts even intra-procedurally.

# Cause
State explosion caused by a call inst being lifted past it's def.
```
"I believe what's happening is that a call is being hoisted past it's def. The 
basic block also has a back edge and the condition is appended to itself 
whereas it should have been pushed to true at the callsite. I think this 
isn't detected because I use PhiNodes to check for backedges while the 
value returned from the call is a struct, used via geps and loads.
Basic Block 13, inst %15"
 -Slack
```

# Fix
Check if the CallSite is used and push the state to true if it is.

# Git Commits
* Before Fix
```
commit d2d08702ac574d2e7e40a266302591b8bc73f18f
Author: Shreeasish Kumar <shreeasish@gmail.com>
Date:   Wed Jul 3 01:09:11 2019 -0700

    Expr printing and flattening + scripts
```

* After Fix
```
commit e0a08d4ad6a4506dc59c6762a1699416baba323a
Author: Shreeasish Kumar <shreeasish@gmail.com>
Date:   Thu Jul 4 12:50:51 2019 -0700

    Fixed cp copy bug (calls being lifted past their defs)
```
# Comments
The symptoms exhibited brought to light a few issues which must be dealt with:

1. Havoc Types
Havoc statements pertaining to the abstract state only; i.e. they do not
require privileges themselves but they churn the abstract state through
side effects (external inputs or heap manipulation)
2. Topological Requirements
While lifting call statements is fundamentally wrong, the real cause of 
state explosion was due to abstract values being propagated along the
backedge.
3. Next Step
The actual error needs to be fixed by imposing a topological ordering on
the basic blocks and preventing the flow of dataflow facts along the
backedge.
