#!/bin/sh
CLANG=clang++
INCLUDE="-I/usr/lib/llvm-7/include -I/usr/include/c++/8 -I/usr/include/x86_64-linux-gnu/c++/8 -I/usr/include -I/usr/lib/llvm-7/lib/clang/7.0.0/include"

$CLANG -cc1 -analyze -analyzer-checker=core,unix,deadcode,security,nullability,cplusplus -stdlib=libc++ $INCLUDE *.cpp
