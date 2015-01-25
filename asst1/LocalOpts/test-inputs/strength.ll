; ModuleID = 'strength.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @compute(i32 %a, i32 %b) #0 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %result = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  store i32 %b, i32* %b.addr, align 4
  store i32 0, i32* %result, align 4
  %0 = load i32* %a.addr, align 4
  %mul = mul nsw i32 %0, 2
  %1 = load i32* %result, align 4
  %add = add nsw i32 %1, %mul
  store i32 %add, i32* %result, align 4
  %2 = load i32* %a.addr, align 4
  %mul1 = mul nsw i32 %2, 3
  %3 = load i32* %result, align 4
  %add2 = add nsw i32 %3, %mul1
  store i32 %add2, i32* %result, align 4
  %4 = load i32* %a.addr, align 4
  %mul3 = mul nsw i32 %4, 8
  %5 = load i32* %result, align 4
  %add4 = add nsw i32 %5, %mul3
  store i32 %add4, i32* %result, align 4
  %6 = load i32* %b.addr, align 4
  %div = sdiv i32 %6, 2
  %7 = load i32* %result, align 4
  %sub = sub nsw i32 %7, %div
  store i32 %sub, i32* %result, align 4
  %8 = load i32* %b.addr, align 4
  %div5 = sdiv i32 %8, 4
  %9 = load i32* %result, align 4
  %sub6 = sub nsw i32 %9, %div5
  store i32 %sub6, i32* %result, align 4
  %10 = load i32* %b.addr, align 4
  %div7 = sdiv i32 %10, 8
  %11 = load i32* %result, align 4
  %sub8 = sub nsw i32 %11, %div7
  store i32 %sub8, i32* %result, align 4
  %12 = load i32* %result, align 4
  ret i32 %12
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
