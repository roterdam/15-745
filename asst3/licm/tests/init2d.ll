; ModuleID = 'init2d.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind
define i32 @f(i32 %n) #0 {
entry:
  %n.addr = alloca i32, align 4
  %saved_stack = alloca i8*
  %i = alloca i32, align 4
  %j = alloca i32, align 4
  %j9 = alloca i32, align 4
  %i14 = alloca i32, align 4
  %sum = alloca float, align 4
  %i33 = alloca i32, align 4
  %j38 = alloca i32, align 4
  %cleanup.dest.slot = alloca i32
  store i32 %n, i32* %n.addr, align 4
  %0 = load i32* %n.addr, align 4
  %1 = load i32* %n.addr, align 4
  %2 = call i8* @llvm.stacksave()
  store i8* %2, i8** %saved_stack
  %3 = mul nuw i32 %0, %1
  %vla = alloca float, i32 %3, align 4
  store i32 0, i32* %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc6, %entry
  %4 = load i32* %i, align 4
  %5 = load i32* %n.addr, align 4
  %cmp = icmp slt i32 %4, %5
  br i1 %cmp, label %for.body, label %for.end8

for.body:                                         ; preds = %for.cond
  store i32 0, i32* %j, align 4
  br label %for.cond1

for.cond1:                                        ; preds = %for.inc, %for.body
  %6 = load i32* %j, align 4
  %7 = load i32* %n.addr, align 4
  %cmp2 = icmp slt i32 %6, %7
  br i1 %cmp2, label %for.body3, label %for.end

for.body3:                                        ; preds = %for.cond1
  %8 = load i32* %i, align 4
  %add = add nsw i32 %8, 1
  %conv = sitofp i32 %add to double
  %div = fdiv double 1.000000e+00, %conv
  %conv4 = fptrunc double %div to float
  %9 = load i32* %j, align 4
  %10 = load i32* %i, align 4
  %11 = mul nsw i32 %10, %1
  %arrayidx = getelementptr inbounds float* %vla, i32 %11
  %arrayidx5 = getelementptr inbounds float* %arrayidx, i32 %9
  store float %conv4, float* %arrayidx5, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body3
  %12 = load i32* %j, align 4
  %inc = add nsw i32 %12, 1
  store i32 %inc, i32* %j, align 4
  br label %for.cond1

for.end:                                          ; preds = %for.cond1
  br label %for.inc6

for.inc6:                                         ; preds = %for.end
  %13 = load i32* %i, align 4
  %inc7 = add nsw i32 %13, 1
  store i32 %inc7, i32* %i, align 4
  br label %for.cond

for.end8:                                         ; preds = %for.cond
  store i32 0, i32* %j9, align 4
  br label %for.cond10

for.cond10:                                       ; preds = %for.inc30, %for.end8
  %14 = load i32* %j9, align 4
  %15 = load i32* %n.addr, align 4
  %cmp11 = icmp slt i32 %14, %15
  br i1 %cmp11, label %for.body13, label %for.end32

for.body13:                                       ; preds = %for.cond10
  store i32 0, i32* %i14, align 4
  br label %for.cond15

for.cond15:                                       ; preds = %for.inc27, %for.body13
  %16 = load i32* %i14, align 4
  %17 = load i32* %n.addr, align 4
  %cmp16 = icmp slt i32 %16, %17
  br i1 %cmp16, label %for.body18, label %for.end29

for.body18:                                       ; preds = %for.cond15
  %18 = load i32* %j9, align 4
  %add19 = add nsw i32 %18, 1
  %conv20 = sitofp i32 %add19 to double
  %div21 = fdiv double 1.000000e+00, %conv20
  %19 = load i32* %j9, align 4
  %20 = load i32* %i14, align 4
  %21 = mul nsw i32 %20, %1
  %arrayidx22 = getelementptr inbounds float* %vla, i32 %21
  %arrayidx23 = getelementptr inbounds float* %arrayidx22, i32 %19
  %22 = load float* %arrayidx23, align 4
  %conv24 = fpext float %22 to double
  %add25 = fadd double %conv24, %div21
  %conv26 = fptrunc double %add25 to float
  store float %conv26, float* %arrayidx23, align 4
  br label %for.inc27

for.inc27:                                        ; preds = %for.body18
  %23 = load i32* %i14, align 4
  %inc28 = add nsw i32 %23, 1
  store i32 %inc28, i32* %i14, align 4
  br label %for.cond15

for.end29:                                        ; preds = %for.cond15
  br label %for.inc30

for.inc30:                                        ; preds = %for.end29
  %24 = load i32* %j9, align 4
  %inc31 = add nsw i32 %24, 1
  store i32 %inc31, i32* %j9, align 4
  br label %for.cond10

for.end32:                                        ; preds = %for.cond10
  store float 0.000000e+00, float* %sum, align 4
  store i32 0, i32* %i33, align 4
  br label %for.cond34

for.cond34:                                       ; preds = %for.inc49, %for.end32
  %25 = load i32* %i33, align 4
  %26 = load i32* %n.addr, align 4
  %cmp35 = icmp slt i32 %25, %26
  br i1 %cmp35, label %for.body37, label %for.end51

for.body37:                                       ; preds = %for.cond34
  store i32 0, i32* %j38, align 4
  br label %for.cond39

for.cond39:                                       ; preds = %for.inc46, %for.body37
  %27 = load i32* %j38, align 4
  %28 = load i32* %n.addr, align 4
  %cmp40 = icmp slt i32 %27, %28
  br i1 %cmp40, label %for.body42, label %for.end48

for.body42:                                       ; preds = %for.cond39
  %29 = load i32* %j38, align 4
  %30 = load i32* %i33, align 4
  %31 = mul nsw i32 %30, %1
  %arrayidx43 = getelementptr inbounds float* %vla, i32 %31
  %arrayidx44 = getelementptr inbounds float* %arrayidx43, i32 %29
  %32 = load float* %arrayidx44, align 4
  %33 = load float* %sum, align 4
  %add45 = fadd float %33, %32
  store float %add45, float* %sum, align 4
  br label %for.inc46

for.inc46:                                        ; preds = %for.body42
  %34 = load i32* %j38, align 4
  %inc47 = add nsw i32 %34, 1
  store i32 %inc47, i32* %j38, align 4
  br label %for.cond39

for.end48:                                        ; preds = %for.cond39
  br label %for.inc49

for.inc49:                                        ; preds = %for.end48
  %35 = load i32* %i33, align 4
  %inc50 = add nsw i32 %35, 1
  store i32 %inc50, i32* %i33, align 4
  br label %for.cond34

for.end51:                                        ; preds = %for.cond34
  store i32 1, i32* %cleanup.dest.slot
  %36 = load i8** %saved_stack
  call void @llvm.stackrestore(i8* %36)
  ret i32 ptrtoint (i32 (i32)* @f to i32)
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
  %call = call i32 @f(i32 500)
  ret i32 %call
}

attributes #0 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
