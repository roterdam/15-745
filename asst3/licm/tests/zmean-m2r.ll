; ModuleID = 'zmean-m2r.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @f(i32 %n, i32 %step, i32 %max) #0 {
entry:
  %0 = call i8* @llvm.stacksave()
  %vla = alloca i32, i32 %n, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  %cmp = icmp slt i32 %i.0, %n
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %mul = mul nsw i32 %i.0, %step
  %rem = srem i32 %mul, %max
  %arrayidx = getelementptr inbounds i32* %vla, i32 %i.0
  store i32 %rem, i32* %arrayidx, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %inc = add nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  %arrayidx1 = getelementptr inbounds i32* %vla, i32 0
  %1 = load i32* %arrayidx1, align 4
  br label %for.cond3

for.cond3:                                        ; preds = %for.inc13, %for.end
  %minVal.0 = phi i32 [ %1, %for.end ], [ %cond, %for.inc13 ]
  %maxVal.0 = phi i32 [ %1, %for.end ], [ %cond12, %for.inc13 ]
  %i2.0 = phi i32 [ 1, %for.end ], [ %inc14, %for.inc13 ]
  %cmp4 = icmp slt i32 %i2.0, %n
  br i1 %cmp4, label %for.body5, label %for.end15

for.body5:                                        ; preds = %for.cond3
  %arrayidx6 = getelementptr inbounds i32* %vla, i32 %i2.0
  %2 = load i32* %arrayidx6, align 4
  %cmp7 = icmp slt i32 %2, %minVal.0
  br i1 %cmp7, label %cond.true, label %cond.false

cond.true:                                        ; preds = %for.body5
  br label %cond.end

cond.false:                                       ; preds = %for.body5
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %cond = phi i32 [ %2, %cond.true ], [ %minVal.0, %cond.false ]
  %cmp8 = icmp sgt i32 %2, %maxVal.0
  br i1 %cmp8, label %cond.true9, label %cond.false10

cond.true9:                                       ; preds = %cond.end
  br label %cond.end11

cond.false10:                                     ; preds = %cond.end
  br label %cond.end11

cond.end11:                                       ; preds = %cond.false10, %cond.true9
  %cond12 = phi i32 [ %2, %cond.true9 ], [ %maxVal.0, %cond.false10 ]
  br label %for.inc13

for.inc13:                                        ; preds = %cond.end11
  %inc14 = add nsw i32 %i2.0, 1
  br label %for.cond3

for.end15:                                        ; preds = %for.cond3
  br label %for.cond17

for.cond17:                                       ; preds = %for.inc22, %for.end15
  %i16.0 = phi i32 [ 0, %for.end15 ], [ %inc23, %for.inc22 ]
  %cmp18 = icmp slt i32 %i16.0, %n
  br i1 %cmp18, label %for.body19, label %for.end24

for.body19:                                       ; preds = %for.cond17
  %sub = sub nsw i32 %maxVal.0, %minVal.0
  %div = sdiv i32 %sub, 2
  %arrayidx20 = getelementptr inbounds i32* %vla, i32 %i16.0
  %3 = load i32* %arrayidx20, align 4
  %sub21 = sub nsw i32 %3, %div
  store i32 %sub21, i32* %arrayidx20, align 4
  br label %for.inc22

for.inc22:                                        ; preds = %for.body19
  %inc23 = add nsw i32 %i16.0, 1
  br label %for.cond17

for.end24:                                        ; preds = %for.cond17
  %arrayidx25 = getelementptr inbounds i32* %vla, i32 0
  %4 = load i32* %arrayidx25, align 4
  call void @llvm.stackrestore(i8* %0)
  ret i32 %4
}

; Function Attrs: nounwind
declare i8* @llvm.stacksave() #1

; Function Attrs: nounwind
declare void @llvm.stackrestore(i8*) #1

; Function Attrs: nounwind
define i32 @main() #0 {
entry:
  %call = call i32 @f(i32 10000, i32 17, i32 17777)
  ret i32 %call
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
