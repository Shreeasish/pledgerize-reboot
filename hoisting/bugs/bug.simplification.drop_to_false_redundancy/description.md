# Symptom
Abstract State Goes To Bottom

# Cause
`ConditionList.h::simplifyRedundancies` performs a basic check 
to find disjuncts which have the same conjuncts save for one
which exists in both but is the negation of it's counterpart.
In the event there are only two disjuncts, the method removes
both disjuncts and the state becomes false.

# Fix
Set a flag to check if a disjunct has been removed; if the 
disjunct is empty and the flag is set. Set the conjunct to
be Vacuosly True.

# Git Commits
* Before Fix
```
commit b6de703f6bb1050e105aa8d7855b61b9fbffc6b0
Author: Shreeasish Kumar <shreeasish@gmail.com>
Date:   Thu Jul 4 12:53:11 2019 -0700

    Documentation for cp bug

```
# Comments

1. This is strange since this violation would have been caught
in the very first test case, suggesting breaking changes
would have been made at some point.
Digging through the Git repo might be helpful.

2. Offload to a simplifier. Stronger guarantees on correctness.
