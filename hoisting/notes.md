# Aliasing Writes can interfere with GEPs.
------------------------------------------

Exemplified Below.

```
%115 = load %struct.imsgev*, %struct.imsgev** @iev_frontend, align 8, !dbg !7028
%116 = getelementptr inbounds %struct.imsgev, %struct.imsgev* %115, i64 0, i32 3, !dbg !7029
store i16 2, i16* %116, align 8, !dbg !7030

%137 = load %struct.imsgev*, %struct.imsgev** @iev_frontend, align 8, !dbg !7050
%138 = getelementptr inbounds %struct.imsgev, %struct.imsgev* %137, i64 0, i32 0
```
---------------------------------------
```
17101 (binaryNode.value)   store i8* %101, i8** bitcast (%struct.imsgev** @iev_engine to i8**), align 8
17101 (binaryNode.op.opCode) 100
 lhs @iev_frontend = common local_unnamed_addr global %struct.imsgev* null, align 8
 rhs i8** bitcast (%struct.imsgev** @iev_engine to i8**)

16944 (binaryNode.value)   %138 = getelementptr inbounds %struct.imsgev, %struct.imsgev* %137, i64 0, i32 0
16944 (binaryNode.op.opCode) 32
 lhs i16 2
 rhs i64 0
```
---------------------
```
Handling store at   store i16 2, i16* %116, align 8
Weak load for   %43 = load i8*, i8** @csock, align 8
Weak load for   %38 = load i32, i32* %37, align 4
Weak load for   %5 =  load %struct.imsgev*, %struct.imsgev** @iev_frontend, align 8 // Weak alias for register 137
Weak load for   %11 = load %struct.imsgev*, %struct.imsgev** @iev_engine, align 8
```

Binary Node 16944 is an abstracted GEP Instruction. The third operand to it is a load from a global struct to the virtual register `%137`. 
When the analysis rolls over a store to store to register `%116`, it considers it as a may-clobber to register `%137` and consequently rewrites the load in node 16944 with 

----------------------------------------
# Lowering instructions with out-of-scope defs

In order to perform computations, functions require values or instructions from it's caller (parent function). 
To pass an instruction or value, it must be stored onto a call stack by the parent function and later retrieved from the stack by the callee. 
A convention must also be established such that the callee is aware which elements on the stack correspond to the required values or instructions.
To complicate matters, a callee may require values which are not a part of it's immediate parent, i.e. a callee may be nested within another callee and need instructions/values
from the parent. In this case the value or instruction will have to transitively propagated to the callee via call stacks as a caller can only provide instructions to it's immediate callee.
A function may also be reused and may require a different set of values or instructions depending upon the context it is being called from.

Solution:
From the leaves of callgraphs a specification for the required values can be propagated back to the parents. Performing a reverse topological iteration would work for this.
The leaves would need to have a handle to the callstack and specify which index it expects a value to be placed at. A node would receive specifications from it's children.
The specifications would have to be composed together into a new specification for it's parent. In the base case a parent would have the instruction def and would place that onto the call stack.

Managing the call stack:
The call stack will have to be generated during the analysis but it will be active during runtime. It's important to separate the responsibilities of the runtime from the analysis.
LLVM provides a struct_type which could be used for each stack. A compile time (active during the static analysis) map would be maintained to map values to indices.
Since the values are expected to be unique this works perfectly well for a parent to callee. To manage multiple layers of nesting and differing calling contexts, another layer of indirection
would be necessary which would keep track of the parent and the callee.
This still wouldn't be enough to handle context sensitive lowering (why?)

The actual transformation which would have to be carried would infact break the invarient held thus far, that states ahead of the def of an instruction do not hold abstracted versions of an instruction.
However, by performing evaluations on the constraint which holds uses the def of an instruction interprocedurally, we can abstract the requirement to a single bit.
This bit could be easily pushed into the stack frame for use by the callee.
An implicit requirement for this technique would be that the disjuncts would need to be broken. Conjuncts which are invarient across function boundaries could be easily transformed to a bit.
Conjuncts which are transformed further between the considered point of lowering and parent function which holds the def, would have to have their values stored onto the call stack.

-----------------------------

# Selecting points to lower at
* Points of uncertainty -- Dropping to True
* Function Boundaries?
* Changes in State
  Whenever there are changes in state, especially at locations where the state drops to true.
* Exclusive Paths
  Paths may have exclusive regions. Privileges may be dropped in these exclusive regions.
  The conditional privileges would be the same as unconditional privileges needed in this case.

## Abstract State Behaviours
###### _An abstract state is a disjunction_
### Losing Conjuncts (Backwards Flow)

  A disjunction may lose conjuncts while flowing upwards through the program. 
  Potential causes for this would be: 
  * Simplifications resulting from new alias information,
  * Simplifications over control flow merges (backwards),
  * Conjunct removal due to unknown instructions. (maybe others?)

* Simplifications from alias analysis:
  Consider a disjunction which holds may-alias information.
  ```
  (A aliases B)(B > 5)   finds must alias   (A  aliases B)(B > 5)     simplified to     (B > 5)
  (A !aiases B)(X > 5)  ------------------> (A !aliases B)(B > 5)   ----------------->
  ```
  Benefit to Lowering: Strong Path Guarantees, Simplified Lowering
  Need to find evidence for this

* Merges in Control Flow (Backwards):
  Points of merging: Branches and Function Calls (others?)

  **Branches**
  Backward merges along complementary paths (if with if!) would cause predicates to be dropped
  ```
  backward merge   !(A < B)(X > 5)     simplified to  
  (A < B)(X > 5)  ---------------->  (A < B)(X > 5)   -----------------> (X > 5)
  ```

  Benefit to Lowering: Demonstrated Successful hoist through diamonds, double diamonds etc.
  Basic control flow. Guaranteed to happen.

  **Function Calls With Compile Time Bindings**
  Function Calls are not conditional and it should not affect the semantics of a constraint.
  Function calls present a logical boundary.

  **Runtime Polymorphic Functions**
  Presents branching control flow. Could potentially be resolved with alias analysis.
  Low Priority Concern for now.

  Should essentially be the same as a backward merge in a control flow.
  Interweaving threads and processes would be intersting. Not a concern for now.

* Dropped Conjuncts

  ```
  if ... A                                      dropping B
    if ... B        ---------->   A && B && C  ------------>  A && C
      if ... C
        syscall( )
        /* Needs Privilege X */
  ```

  Implies that we assume B will not be considered in the evaluation of needed predicates. 

  ```
  if ... A                                      dropping B
    if ... B        -----------> A && !B       -------------> A
      if ... C                                 \ dropping A
    else ... !B                                 ------------> !B
      syscall( ) 
      /* Needs Privilege X */
  ```
  Reasons for dropping Conjuncts:
  * Unknown instructions
  * Loop Variants
  * Returns from calls to external/opaque functions

  Semantic Implication:
  Loss in precision. Lowering before (backwards) a drop would be useful. Generally This defines the boundary of hoisting. 

  Conjunctions/Disjuncts are a set of predicates anded together. Dropping a conjunct in a conjunction essentially means that the conjunction will evaluate to true without regard
  to that predicate. In terms of the program, a conjunction is a set of chained if statements.

General Trend
================
  Aside from when conjuncts in the abstract state are dropped due to unknowns, a decline in the size of the predicates would imply an increase in precision.

------------------------
### Gaining Conjuncts (Backwards)
  Potential Reasons:
  * A merge in the control flow
    * Increase in conjunctions  -- Nested conditionals
    * Increase in disjuncts     -- Complementary conditionals
  * A may-alias store instruction
  * Function Call

  * **Branches/Switches** (Conjunct Addition):
    If the abstract states on backward incoming edges of a branch statement are different, a branch condition will introduce a new conjunct (branch condition) to each disjunct 
    and merge it with the abstract state coming in from another branch (new disjuncts).

    Semantic Implication:
    Reduction in surface area of attack, lowering after a branch (backwards) is preferred.

  * **Store Instruction**
    A store instruction may introduce new disjuncts into the abstract state. This introduces potential false positives. 
    This should not affect the correctness of the analysis as it is still conservative
    However, this does introduce overheads. 

    Semantic Implication:
    Reduction in surface area however lowering may not be preferred as it would lead to increase in complexity.

  * **Function Call**
    A function call would introduce more constraints into the caller from internal constraints picked up

  * **Function Call w/ arguments that change behaviour**
    Function calls may have arguments which change their behaviour would create new conjuncts. New conjuncts can be introduced to represent this behaviour.
    A reasonable assumption is that these arguments are hardcoded.
    Non hardcoded arguments would create implicit branches even in straight line programs.

    Semantic Implication:
    Reduction in surface area. Function boundries present a logical seperation. Useful to lower before this or after this.

General Trend
================
  An increase in  conjuncts generally shows that the analysis has gained precision.
  Aside from before store instructions.


State Relations


================
## Deprecated
`Phi < Phi'` 
Needs to be defined as an abstract state which happens before another.

Question 1. If `phi < phi'` does `phi -> phi'`.
Question 2. Assuming the answer to the previous question is yes, i have to define a subset relationship between predicates which shows that one constrains behaviour more than another.
            _note that this does means that the implementation has to be changed to account for some of this behaviour. Specifically the part about propagating past callsites._
Question 3. Generating heuristics for finding subset relationships.

Happens before implies that the previous state implies the next state.
Implication relationships are not what we want to consider.
What we need is something which can identify that the predicates of an abstract state are stronger than the predicates of another abstract state.
i.e. an abstract state subsumes another. Therefore given any two abstract states, we can identify where it would be better to lower. This includes abstract states which are not on the same path.
--------------
## Using Weakest Preconditions and Strongest Postconditions

Simply put,
For a pair of instructions where `i < j` (`i` happens before `j`), if the 
strongest postcondition of i is weaker than the weakest precondition of j,
then it informs us of the benefits of lowering prior to j and ahead of i.
---------------
## Formalizing the System

To formally specify the system I can write it out as the specifications
to a dataflow analysis 

### Formal Grammar

TODO: Copy from notebook

### Transfer Functions

