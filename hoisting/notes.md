Aliasing Writes can interfere with GEPs.
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
