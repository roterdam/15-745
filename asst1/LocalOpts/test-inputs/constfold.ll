; ModuleID = 'constfold.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @compute() #0 {
entry:
  %result = alloca i32, align 4
  %a = alloca i32, align 4
  %b = alloca i32, align 4
  %c = alloca i32, align 4
  store i32 0, i32* %result, align 4
  store i32 2, i32* %a, align 4
  store i32 3, i32* %b, align 4
  %0 = load i32* %a, align 4
  %add = add nsw i32 4, %0
  %1 = load i32* %b, align 4
  %add1 = add nsw i32 %add, %1
  store i32 %add1, i32* %c, align 4
  %2 = load i32* %a, align 4
  %3 = load i32* %result, align 4
  %add2 = add nsw i32 %3, %2
  store i32 %add2, i32* %result, align 4
  %4 = load i32* %b, align 4
  %5 = load i32* %result, align 4
  %add3 = add nsw i32 %5, %4
  store i32 %add3, i32* %result, align 4
  %6 = load i32* %c, align 4
  %7 = load i32* %result, align 4
  %mul = mul nsw i32 %7, %6
  store i32 %mul, i32* %result, align 4
  %8 = load i32* %result, align 4
  %div = sdiv i32 %8, 2
  store i32 %div, i32* %result, align 4
  %9 = load i32* %result, align 4
  ret i32 %9
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
