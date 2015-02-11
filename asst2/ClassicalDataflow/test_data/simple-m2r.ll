; ModuleID = 'simple-m2r.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @foo(i32 %a, i32 %b) #0 {
entry:
  %cmp = icmp eq i32 %b, 0
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %add = add nsw i32 %b, 1
  br label %if.end

if.else:                                          ; preds = %entry
  %sub = sub nsw i32 %b, 1
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %a.addr.0 = phi i32 [ %add, %if.then ], [ %sub, %if.else ]
  ret i32 %a.addr.0
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
