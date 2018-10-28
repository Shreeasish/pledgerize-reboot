; ModuleID = '02-diamond.ll'
source_filename = "02-diamond.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant [15 x i8] c"\0AFrom if block\00", align 1
@.str.1 = private unnamed_addr constant [17 x i8] c"\0AFrom else block\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 !dbg !7 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  call void @llvm.dbg.declare(metadata i32* %2, metadata !11, metadata !DIExpression()), !dbg !12
  store i32 2, i32* %2, align 4, !dbg !12
  %3 = load i32, i32* %2, align 4, !dbg !13
  %4 = srem i32 %3, 2, !dbg !15
  %5 = icmp ne i32 %4, 0, !dbg !15
  br i1 %5, label %6, label %8, !dbg !16

; <label>:6:                                      ; preds = %0
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str, i32 0, i32 0)), !dbg !17
  br label %10, !dbg !19

; <label>:8:                                      ; preds = %0
  %9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.1, i32 0, i32 0)), !dbg !20
  br label %10

; <label>:10:                                     ; preds = %8, %6
  ret i32 0, !dbg !22
}

; Function Attrs: nounwind readnone speculatable
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

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
!13 = !DILocation(line: 6, column: 6, scope: !14)
!14 = distinct !DILexicalBlock(scope: !7, file: !1, line: 6, column: 6)
!15 = !DILocation(line: 6, column: 7, scope: !14)
!16 = !DILocation(line: 6, column: 6, scope: !7)
!17 = !DILocation(line: 7, column: 5, scope: !18)
!18 = distinct !DILexicalBlock(scope: !14, file: !1, line: 6, column: 10)
!19 = !DILocation(line: 8, column: 3, scope: !18)
!20 = !DILocation(line: 10, column: 5, scope: !21)
!21 = distinct !DILexicalBlock(scope: !14, file: !1, line: 9, column: 8)
!22 = !DILocation(line: 13, column: 3, scope: !7)
