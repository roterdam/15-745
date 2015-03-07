; ModuleID = 'init2d-m2r.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @f(i32 %n) #0 {
entry:
  %0 = call i8* @llvm.stacksave()
  %1 = mul nuw i32 %n, %n
  %vla = alloca float, i32 %1, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc6, %entry
  %i.0 = phi i32 [ 0, %entry ], [ %inc7, %for.inc6 ]
  %cmp = icmp slt i32 %i.0, %n
  br i1 %cmp, label %for.body, label %for.end8

for.body:                                         ; preds = %for.cond
  br label %for.cond1

for.cond1:                                        ; preds = %for.inc, %for.body
  %j.0 = phi i32 [ 0, %for.body ], [ %inc, %for.inc ]
  %cmp2 = icmp slt i32 %j.0, %n
  br i1 %cmp2, label %for.body3, label %for.end

for.body3:                                        ; preds = %for.cond1
  %add = add nsw i32 %i.0, 1
  %conv = sitofp i32 %add to double
  %div = fdiv double 1.000000e+00, %conv
  %conv4 = fptrunc double %div to float
  %2 = mul nsw i32 %i.0, %n
  %arrayidx = getelementptr inbounds float* %vla, i32 %2
  %arrayidx5 = getelementptr inbounds float* %arrayidx, i32 %j.0
  store float %conv4, float* %arrayidx5, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body3
  %inc = add nsw i32 %j.0, 1
  br label %for.cond1

for.end:                                          ; preds = %for.cond1
  br label %for.inc6

for.inc6:                                         ; preds = %for.end
  %inc7 = add nsw i32 %i.0, 1
  br label %for.cond

for.end8:                                         ; preds = %for.cond
  br label %for.cond10

for.cond10:                                       ; preds = %for.inc30, %for.end8
  %j9.0 = phi i32 [ 0, %for.end8 ], [ %inc31, %for.inc30 ]
  %cmp11 = icmp slt i32 %j9.0, %n
  br i1 %cmp11, label %for.body13, label %for.end32

for.body13:                                       ; preds = %for.cond10
  br label %for.cond15

for.cond15:                                       ; preds = %for.inc27, %for.body13
  %i14.0 = phi i32 [ 0, %for.body13 ], [ %inc28, %for.inc27 ]
  %cmp16 = icmp slt i32 %i14.0, %n
  br i1 %cmp16, label %for.body18, label %for.end29

for.body18:                                       ; preds = %for.cond15
  %add19 = add nsw i32 %j9.0, 1
  %conv20 = sitofp i32 %add19 to double
  %div21 = fdiv double 1.000000e+00, %conv20
  %3 = mul nsw i32 %i14.0, %n
  %arrayidx22 = getelementptr inbounds float* %vla, i32 %3
  %arrayidx23 = getelementptr inbounds float* %arrayidx22, i32 %j9.0
  %4 = load float* %arrayidx23, align 4
  %conv24 = fpext float %4 to double
  %add25 = fadd double %conv24, %div21
  %conv26 = fptrunc double %add25 to float
  store float %conv26, float* %arrayidx23, align 4
  br label %for.inc27

for.inc27:                                        ; preds = %for.body18
  %inc28 = add nsw i32 %i14.0, 1
  br label %for.cond15

for.end29:                                        ; preds = %for.cond15
  br label %for.inc30

for.inc30:                                        ; preds = %for.end29
  %inc31 = add nsw i32 %j9.0, 1
  br label %for.cond10

for.end32:                                        ; preds = %for.cond10
  br label %for.cond34

for.cond34:                                       ; preds = %for.inc49, %for.end32
  %sum.0 = phi float [ 0.000000e+00, %for.end32 ], [ %sum.1, %for.inc49 ]
  %i33.0 = phi i32 [ 0, %for.end32 ], [ %inc50, %for.inc49 ]
  %cmp35 = icmp slt i32 %i33.0, %n
  br i1 %cmp35, label %for.body37, label %for.end51

for.body37:                                       ; preds = %for.cond34
  br label %for.cond39

for.cond39:                                       ; preds = %for.inc46, %for.body37
  %sum.1 = phi float [ %sum.0, %for.body37 ], [ %add45, %for.inc46 ]
  %j38.0 = phi i32 [ 0, %for.body37 ], [ %inc47, %for.inc46 ]
  %cmp40 = icmp slt i32 %j38.0, %n
  br i1 %cmp40, label %for.body42, label %for.end48

for.body42:                                       ; preds = %for.cond39
  %5 = mul nsw i32 %i33.0, %n
  %arrayidx43 = getelementptr inbounds float* %vla, i32 %5
  %arrayidx44 = getelementptr inbounds float* %arrayidx43, i32 %j38.0
  %6 = load float* %arrayidx44, align 4
  %add45 = fadd float %sum.1, %6
  br label %for.inc46

for.inc46:                                        ; preds = %for.body42
  %inc47 = add nsw i32 %j38.0, 1
  br label %for.cond39

for.end48:                                        ; preds = %for.cond39
  br label %for.inc49

for.inc49:                                        ; preds = %for.end48
  %inc50 = add nsw i32 %i33.0, 1
  br label %for.cond34

for.end51:                                        ; preds = %for.cond34
  call void @llvm.stackrestore(i8* %0)
  ret i32 ptrtoint (i32 (i32)* @f to i32)
}

; Function Attrs: nounwind
declare i8* @llvm.stacksave() #1

; Function Attrs: nounwind
declare void @llvm.stackrestore(i8*) #1

; Function Attrs: nounwind
define i32 @main() #0 {
entry:
  %call = call i32 @f(i32 500)
  ret i32 %call
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
