#!/bin/bash

f=$1
base=${f%.*}
clang -O -emit-llvm -c $base.c
llvm-dis $base.bc
