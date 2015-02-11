; ModuleID = 'nthprime.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @isPrime(i32 %n) #0 {
entry:
  %retval = alloca i32, align 4
  %n.addr = alloca i32, align 4
  %guess = alloca i32, align 4
  store i32 %n, i32* %n.addr, align 4
  store i32 2, i32* %guess, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %0 = load i32* %guess, align 4
  %1 = load i32* %n.addr, align 4
  %cmp = icmp slt i32 %0, %1
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %2 = load i32* %n.addr, align 4
  %3 = load i32* %guess, align 4
  %rem = srem i32 %2, %3
  %cmp1 = icmp eq i32 %rem, 0
  br i1 %cmp1, label %if.then, label %if.end

if.then:                                          ; preds = %for.body
  store i32 0, i32* %retval
  br label %return

if.end:                                           ; preds = %for.body
  br label %for.inc

for.inc:                                          ; preds = %if.end
  %4 = load i32* %guess, align 4
  %inc = add nsw i32 %4, 1
  store i32 %inc, i32* %guess, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  store i32 1, i32* %retval
  br label %return

return:                                           ; preds = %for.end, %if.then
  %5 = load i32* %retval
  ret i32 %5
}

; Function Attrs: nounwind
define i32 @nthprime(i32 %n) #0 {
entry:
  %n.addr = alloca i32, align 4
  %current = alloca i32, align 4
  %numFound = alloca i32, align 4
  store i32 %n, i32* %n.addr, align 4
  store i32 -1, i32* %current, align 4
  store i32 0, i32* %numFound, align 4
  br label %while.cond

while.cond:                                       ; preds = %if.end, %entry
  %0 = load i32* %numFound, align 4
  %1 = load i32* %n.addr, align 4
  %cmp = icmp slt i32 %0, %1
  br i1 %cmp, label %while.body, label %while.end

while.body:                                       ; preds = %while.cond
  %2 = load i32* %current, align 4
  %add = add nsw i32 %2, 1
  store i32 %add, i32* %current, align 4
  %3 = load i32* %current, align 4
  %call = call i32 @isPrime(i32 %3)
  %tobool = icmp ne i32 %call, 0
  br i1 %tobool, label %if.then, label %if.end

if.then:                                          ; preds = %while.body
  %4 = load i32* %numFound, align 4
  %add1 = add nsw i32 %4, 1
  store i32 %add1, i32* %numFound, align 4
  br label %if.end

if.end:                                           ; preds = %if.then, %while.body
  br label %while.cond

while.end:                                        ; preds = %while.cond
  %5 = load i32* %current, align 4
  ret i32 %5
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
