; ModuleID = '02-diamond.c'
source_filename = "02-diamond.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.1 = private unnamed_addr constant [15 x i8] c"\0AFrom if block\00", align 1
@.str.2 = private unnamed_addr constant [17 x i8] c"\0AFrom else block\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 !dbg !7 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  call void @llvm.dbg.declare(metadata i32* %2, metadata !11, metadata !DIExpression()), !dbg !12
  %3 = call i32 (i8*, ...) @__isoc99_scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i32 0, i32 0), i32* %2), !dbg !13
  %4 = load i32, i32* %2, align 4, !dbg !14
  %5 = srem i32 %4, 2, !dbg !16
  %6 = icmp ne i32 %5, 0, !dbg !16
  br i1 %6, label %7, label %9, !dbg !17

; <label>:7:                                      ; preds = %0
  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.1, i32 0, i32 0)), !dbg !18
  br label %11, !dbg !20

; <label>:9:                                      ; preds = %0
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.2, i32 0, i32 0)), !dbg !21
  br label %11

; <label>:11:                                     ; preds = %9, %7
  ret i32 0, !dbg !23
}

; Function Attrs: nounwind readnone speculatable
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare i32 @__isoc99_scanf(i8*, ...) #2

declare i32 @printf(i8*, ...) #2

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 6.0.0 (tags/RELEASE_600/final)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2)
!1 = !DIFile(filename: "02-diamond.c", directory: "/home/shreeasish/pledgerize-reboot/hoisting/nick-dataflow/test/hoist")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
!7 = distinct !DISubprogram(name: "main", scope: !1, file: !1, line: 3, type: !8, isLocal: false, isDefinition: true, scopeLine: 3, isOptimized: false, unit: !0, variables: !2)
!8 = !DISubroutineType(types: !9)
!9 = !{!10}
!10 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!11 = !DILocalVariable(name: "x", scope: !7, file: !1, line: 4, type: !10)
!12 = !DILocation(line: 4, column: 7, scope: !7)
!13 = !DILocation(line: 5, column: 3, scope: !7)
!14 = !DILocation(line: 7, column: 6, scope: !15)
!15 = distinct !DILexicalBlock(scope: !7, file: !1, line: 7, column: 6)
!16 = !DILocation(line: 7, column: 7, scope: !15)
!17 = !DILocation(line: 7, column: 6, scope: !7)
!18 = !DILocation(line: 8, column: 5, scope: !19)
!19 = distinct !DILexicalBlock(scope: !15, file: !1, line: 7, column: 10)
!20 = !DILocation(line: 9, column: 3, scope: !19)
!21 = !DILocation(line: 11, column: 5, scope: !22)
!22 = distinct !DILexicalBlock(scope: !15, file: !1, line: 10, column: 8)
!23 = !DILocation(line: 14, column: 3, scope: !7)
