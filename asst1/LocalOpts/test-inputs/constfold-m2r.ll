; ModuleID = 'constfold-m2r.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @compute() #0 {
entry:
  %add = add nsw i32 4, 2
  %add1 = add nsw i32 %add, 3
  %add2 = add nsw i32 0, 2
  %add3 = add nsw i32 %add2, 3
  %mul = mul nsw i32 %add3, %add1
  %div = sdiv i32 %mul, 2
  ret i32 %div
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
