# Aliasing Writes can interfere with GEPs.

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
----------------------------------------
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










