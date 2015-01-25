#!/bin/bash

f=$1
base=${f%.*}
clang -O0 -emit-llvm -c $base.c
llvm-dis $base.bc
opt -mem2reg $base.bc -o $base-m2r.bc
llvm-dis $base-m2r.bc
