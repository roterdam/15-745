; ModuleID = 'nthprime.bc'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define i32 @isPrime(i32 %n) #0 {
entry:
  %cmp5 = icmp sgt i32 %n, 2
  br i1 %cmp5, label %for.body.preheader, label %return

for.body.preheader:                               ; preds = %entry
  br label %for.body

for.cond:                                         ; preds = %for.body
  %cmp = icmp slt i32 %inc, %n
  br i1 %cmp, label %for.body, label %return.loopexit

for.body:                                         ; preds = %for.cond, %for.body.preheader
  %guess.06 = phi i32 [ %inc, %for.cond ], [ 2, %for.body.preheader ]
  %rem = srem i32 %n, %guess.06
  %cmp1 = icmp eq i32 %rem, 0
  %inc = add nsw i32 %guess.06, 1
  br i1 %cmp1, label %return.loopexit, label %for.cond

return.loopexit:                                  ; preds = %for.body, %for.cond
  %retval.0.ph = phi i32 [ 1, %for.cond ], [ 0, %for.body ]
  br label %return

return:                                           ; preds = %return.loopexit, %entry
  %retval.0 = phi i32 [ 1, %entry ], [ %retval.0.ph, %return.loopexit ]
  ret i32 %retval.0
}

; Function Attrs: nounwind readnone
define i32 @nthprime(i32 %n) #0 {
entry:
  %cmp4 = icmp sgt i32 %n, 0
  br i1 %cmp4, label %while.body.preheader, label %while.end

while.body.preheader:                             ; preds = %entry
  br label %while.body

while.body:                                       ; preds = %isPrime.exit, %while.body.preheader
  %numFound.06 = phi i32 [ %numFound.0.add1, %isPrime.exit ], [ 0, %while.body.preheader ]
  %current.05 = phi i32 [ %add, %isPrime.exit ], [ -1, %while.body.preheader ]
  %add = add nsw i32 %current.05, 1
  %cmp5.i = icmp sgt i32 %current.05, 1
  br i1 %cmp5.i, label %for.body.i.preheader, label %isPrime.exit

for.body.i.preheader:                             ; preds = %while.body
  br label %for.body.i

for.cond.i:                                       ; preds = %for.body.i
  %cmp.i = icmp slt i32 %inc.i, %add
  br i1 %cmp.i, label %for.body.i, label %isPrime.exit.loopexit

for.body.i:                                       ; preds = %for.cond.i, %for.body.i.preheader
  %guess.06.i = phi i32 [ %inc.i, %for.cond.i ], [ 2, %for.body.i.preheader ]
  %rem.i = srem i32 %add, %guess.06.i
  %cmp1.i = icmp eq i32 %rem.i, 0
  %inc.i = add nsw i32 %guess.06.i, 1
  br i1 %cmp1.i, label %isPrime.exit.loopexit, label %for.cond.i

isPrime.exit.loopexit:                            ; preds = %for.body.i, %for.cond.i
  %retval.0.i.ph = phi i32 [ 0, %for.body.i ], [ 1, %for.cond.i ]
  br label %isPrime.exit

isPrime.exit:                                     ; preds = %isPrime.exit.loopexit, %while.body
  %retval.0.i = phi i32 [ 1, %while.body ], [ %retval.0.i.ph, %isPrime.exit.loopexit ]
  %numFound.0.add1 = add nsw i32 %retval.0.i, %numFound.06
  %cmp = icmp slt i32 %numFound.0.add1, %n
  br i1 %cmp, label %while.body, label %while.end.loopexit

while.end.loopexit:                               ; preds = %isPrime.exit
  %add.lcssa = phi i32 [ %add, %isPrime.exit ]
  br label %while.end

while.end:                                        ; preds = %while.end.loopexit, %entry
  %current.0.lcssa = phi i32 [ -1, %entry ], [ %add.lcssa, %while.end.loopexit ]
  ret i32 %current.0.lcssa
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.5.0 (branches/release_35 225468)"}
