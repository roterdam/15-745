; ModuleID = 'loop-m2r.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @f(i32 %n) #0 {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc5, %entry
  %i.0 = phi i32 [ 0, %entry ], [ %inc6, %for.inc5 ]
  %sum.0 = phi i32 [ 0, %entry ], [ %sum.1, %for.inc5 ]
  %sub = sub nsw i32 %n, 1
  %cmp = icmp slt i32 %i.0, %sub
  br i1 %cmp, label %for.body, label %for.end7

for.body:                                         ; preds = %for.cond
  br label %for.cond1

for.cond1:                                        ; preds = %for.inc, %for.body
  %sum.1 = phi i32 [ %sum.0, %for.body ], [ %add, %for.inc ]
  %j.0 = phi i32 [ 0, %for.body ], [ %inc, %for.inc ]
  %sub2 = sub nsw i32 %n, %i.0
  %cmp3 = icmp slt i32 %j.0, %sub2
  br i1 %cmp3, label %for.body4, label %for.end

for.body4:                                        ; preds = %for.cond1
  %div = sdiv i32 %n, 7
  %add = add nsw i32 %sum.1, %div
  br label %for.inc

for.inc:                                          ; preds = %for.body4
  %inc = add nsw i32 %j.0, 1
  br label %for.cond1

for.end:                                          ; preds = %for.cond1
  br label %for.inc5

for.inc5:                                         ; preds = %for.end
  %inc6 = add nsw i32 %i.0, 1
  br label %for.cond

for.end7:                                         ; preds = %for.cond
  ret i32 %sum.0
}

; Function Attrs: nounwind
define i32 @main() #0 {
entry:
  %call = call i32 @f(i32 1000)
  ret i32 %call
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
