; ModuleID = 'algebraic.bc'
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
  %0 = load i32* %a.addr, align 4
  %1 = load i32* %a.addr, align 4
  %div = sdiv i32 %0, %1
  store i32 %div, i32* %result, align 4
  %2 = load i32* %b.addr, align 4
  %3 = load i32* %b.addr, align 4
  %div1 = sdiv i32 %2, %3
  %4 = load i32* %result, align 4
  %mul = mul nsw i32 %4, %div1
  store i32 %mul, i32* %result, align 4
  %5 = load i32* %b.addr, align 4
  %6 = load i32* %b.addr, align 4
  %sub = sub nsw i32 %5, %6
  %7 = load i32* %result, align 4
  %add = add nsw i32 %7, %sub
  store i32 %add, i32* %result, align 4
  %8 = load i32* %result, align 4
  %9 = load i32* %result, align 4
  %div2 = sdiv i32 %9, %8
  store i32 %div2, i32* %result, align 4
  %10 = load i32* %result, align 4
  %11 = load i32* %result, align 4
  %sub3 = sub nsw i32 %11, %10
  store i32 %sub3, i32* %result, align 4
  %12 = load i32* %result, align 4
  ret i32 %12
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
