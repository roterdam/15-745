; ModuleID = 'zmean.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @f(i32 %n, i32 %step, i32 %max) #0 {
entry:
  %n.addr = alloca i32, align 4
  %step.addr = alloca i32, align 4
  %max.addr = alloca i32, align 4
  %saved_stack = alloca i8*
  %i = alloca i32, align 4
  %minVal = alloca i32, align 4
  %maxVal = alloca i32, align 4
  %i2 = alloca i32, align 4
  %x = alloca i32, align 4
  %i16 = alloca i32, align 4
  %cleanup.dest.slot = alloca i32
  store i32 %n, i32* %n.addr, align 4
  store i32 %step, i32* %step.addr, align 4
  store i32 %max, i32* %max.addr, align 4
  %0 = load i32* %n.addr, align 4
  %1 = call i8* @llvm.stacksave()
  store i8* %1, i8** %saved_stack
  %vla = alloca i32, i32 %0, align 4
  store i32 0, i32* %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %2 = load i32* %i, align 4
  %3 = load i32* %n.addr, align 4
  %cmp = icmp slt i32 %2, %3
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %4 = load i32* %i, align 4
  %5 = load i32* %step.addr, align 4
  %mul = mul nsw i32 %4, %5
  %6 = load i32* %max.addr, align 4
  %rem = srem i32 %mul, %6
  %7 = load i32* %i, align 4
  %arrayidx = getelementptr inbounds i32* %vla, i32 %7
  store i32 %rem, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %8 = load i32* %i, align 4
  %inc = add nsw i32 %8, 1
  store i32 %inc, i32* %i, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  %arrayidx1 = getelementptr inbounds i32* %vla, i32 0
  %9 = load i32* %arrayidx1, align 4
  store i32 %9, i32* %maxVal, align 4
  store i32 %9, i32* %minVal, align 4
  store i32 1, i32* %i2, align 4
  br label %for.cond3

for.cond3:                                        ; preds = %for.inc13, %for.end
  %10 = load i32* %i2, align 4
  %11 = load i32* %n.addr, align 4
  %cmp4 = icmp slt i32 %10, %11
  br i1 %cmp4, label %for.body5, label %for.end15

for.body5:                                        ; preds = %for.cond3
  %12 = load i32* %i2, align 4
  %arrayidx6 = getelementptr inbounds i32* %vla, i32 %12
  %13 = load i32* %arrayidx6, align 4
  store i32 %13, i32* %x, align 4
  %14 = load i32* %x, align 4
  %15 = load i32* %minVal, align 4
  %cmp7 = icmp slt i32 %14, %15
  br i1 %cmp7, label %cond.true, label %cond.false

cond.true:                                        ; preds = %for.body5
  %16 = load i32* %x, align 4
  br label %cond.end

cond.false:                                       ; preds = %for.body5
  %17 = load i32* %minVal, align 4
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %cond = phi i32 [ %16, %cond.true ], [ %17, %cond.false ]
  store i32 %cond, i32* %minVal, align 4
  %18 = load i32* %x, align 4
  %19 = load i32* %maxVal, align 4
  %cmp8 = icmp sgt i32 %18, %19
  br i1 %cmp8, label %cond.true9, label %cond.false10

cond.true9:                                       ; preds = %cond.end
  %20 = load i32* %x, align 4
  br label %cond.end11

cond.false10:                                     ; preds = %cond.end
  %21 = load i32* %maxVal, align 4
  br label %cond.end11

cond.end11:                                       ; preds = %cond.false10, %cond.true9
  %cond12 = phi i32 [ %20, %cond.true9 ], [ %21, %cond.false10 ]
  store i32 %cond12, i32* %maxVal, align 4
  br label %for.inc13

for.inc13:                                        ; preds = %cond.end11
  %22 = load i32* %i2, align 4
  %inc14 = add nsw i32 %22, 1
  store i32 %inc14, i32* %i2, align 4
  br label %for.cond3

for.end15:                                        ; preds = %for.cond3
  store i32 0, i32* %i16, align 4
  br label %for.cond17

for.cond17:                                       ; preds = %for.inc22, %for.end15
  %23 = load i32* %i16, align 4
  %24 = load i32* %n.addr, align 4
  %cmp18 = icmp slt i32 %23, %24
  br i1 %cmp18, label %for.body19, label %for.end24

for.body19:                                       ; preds = %for.cond17
  %25 = load i32* %maxVal, align 4
  %26 = load i32* %minVal, align 4
  %sub = sub nsw i32 %25, %26
  %div = sdiv i32 %sub, 2
  %27 = load i32* %i16, align 4
  %arrayidx20 = getelementptr inbounds i32* %vla, i32 %27
  %28 = load i32* %arrayidx20, align 4
  %sub21 = sub nsw i32 %28, %div
  store i32 %sub21, i32* %arrayidx20, align 4
  br label %for.inc22

for.inc22:                                        ; preds = %for.body19
  %29 = load i32* %i16, align 4
  %inc23 = add nsw i32 %29, 1
  store i32 %inc23, i32* %i16, align 4
  br label %for.cond17

for.end24:                                        ; preds = %for.cond17
  %arrayidx25 = getelementptr inbounds i32* %vla, i32 0
  %30 = load i32* %arrayidx25, align 4
  store i32 1, i32* %cleanup.dest.slot
  %31 = load i8** %saved_stack
  call void @llvm.stackrestore(i8* %31)
  ret i32 %30
}

; Function Attrs: nounwind
declare i8* @llvm.stacksave() #1

; Function Attrs: nounwind
declare void @llvm.stackrestore(i8*) #1

; Function Attrs: nounwind
define i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  store i32 0, i32* %retval
  %call = call i32 @f(i32 10000, i32 17, i32 17777)
  ret i32 %call
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
