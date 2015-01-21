; ModuleID = 'fib.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define i32 @fib(i32 %n) #0 {
entry:
  %cmp = icmp slt i32 %n, 0
  br i1 %cmp, label %return, label %if.end

if.end:                                           ; preds = %entry
  %cmp1 = icmp slt i32 %n, 2
  br i1 %cmp1, label %return, label %if.end3

if.end3:                                          ; preds = %if.end
  %sub = add nsw i32 %n, -1
  %call = tail call i32 @fib(i32 %sub)
  %sub4 = add nsw i32 %n, -2
  %call5 = tail call i32 @fib(i32 %sub4)
  %add = add nsw i32 %call5, %call
  ret i32 %add

return:                                           ; preds = %if.end, %entry
  %retval.0 = phi i32 [ -1, %entry ], [ %n, %if.end ]
  ret i32 %retval.0
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
