#!/bin/sh
CLANG=/homebrew/opt/llvm/bin/clang++
INCLUDE="-I/homebrew/opt/llvm/include -I/homebrew/include"

$CLANG -cc1 -analyze -analyzer-checker=core,unix,deadcode,security,nullability,cplusplus -stdlib=libc++ $INCLUDE *.cpp
