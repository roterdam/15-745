; ModuleID = 'algebraic-m2r.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @compute(i32 %a, i32 %b) #0 {
entry:
  %div = sdiv i32 %a, %a
  %div1 = sdiv i32 %b, %b
  %mul = mul nsw i32 %div, %div1
  %sub = sub nsw i32 %b, %b
  %add = add nsw i32 %mul, %sub
  %div2 = sdiv i32 %add, %add
  %sub3 = sub nsw i32 %div2, %div2
  ret i32 %sub3
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
