; ModuleID = 'loop1.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  %x = alloca i32, align 4
  %i = alloca i32, align 4
  %z = alloca i32, align 4
  %y = alloca i32, align 4
  store i32 0, i32* %retval
  store i32 0, i32* %x, align 4
  store i32 0, i32* %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %0 = load i32* %i, align 4
  %cmp = icmp slt i32 %0, 1000
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  store i32 4000, i32* %z, align 4
  %1 = load i32* %i, align 4
  %2 = load i32* %z, align 4
  %add = add nsw i32 %1, %2
  store i32 %add, i32* %y, align 4
  %3 = load i32* %i, align 4
  %4 = load i32* %z, align 4
  %add1 = add nsw i32 %3, %4
  %5 = load i32* %x, align 4
  %add2 = add nsw i32 %5, %add1
  store i32 %add2, i32* %x, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %6 = load i32* %i, align 4
  %inc = add nsw i32 %6, 1
  store i32 %inc, i32* %i, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  %7 = load i32* %x, align 4
  ret i32 %7
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
