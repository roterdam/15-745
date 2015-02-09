; ModuleID = 'nthprime-m2r.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @isPrime(i32 %n) #0 {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %guess.0 = phi i32 [ 2, %entry ], [ %inc, %for.inc ]
  %cmp = icmp slt i32 %guess.0, %n
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %rem = srem i32 %n, %guess.0
  %cmp1 = icmp eq i32 %rem, 0
  br i1 %cmp1, label %if.then, label %if.end

if.then:                                          ; preds = %for.body
  br label %return

if.end:                                           ; preds = %for.body
  br label %for.inc

for.inc:                                          ; preds = %if.end
  %inc = add nsw i32 %guess.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  br label %return

return:                                           ; preds = %for.end, %if.then
  %retval.0 = phi i32 [ 0, %if.then ], [ 1, %for.end ]
  ret i32 %retval.0
}

; Function Attrs: nounwind
define i32 @nthprime(i32 %n) #0 {
entry:
  br label %while.cond

while.cond:                                       ; preds = %if.end, %entry
  %current.0 = phi i32 [ -1, %entry ], [ %add, %if.end ]
  %numFound.0 = phi i32 [ 0, %entry ], [ %numFound.1, %if.end ]
  %cmp = icmp slt i32 %numFound.0, %n
  br i1 %cmp, label %while.body, label %while.end

while.body:                                       ; preds = %while.cond
  %add = add nsw i32 %current.0, 1
  %call = call i32 @isPrime(i32 %add)
  %tobool = icmp ne i32 %call, 0
  br i1 %tobool, label %if.then, label %if.end

if.then:                                          ; preds = %while.body
  %add1 = add nsw i32 %numFound.0, 1
  br label %if.end

if.end:                                           ; preds = %if.then, %while.body
  %numFound.1 = phi i32 [ %add1, %if.then ], [ %numFound.0, %while.body ]
  br label %while.cond

while.end:                                        ; preds = %while.cond
  ret i32 %current.0
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
