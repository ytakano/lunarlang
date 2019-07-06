#!/bin/sh
CLANG=clang++
INCLUDE="-I/usr/include/c++/8 -I/usr/include/clang/8/include -I/usr/include/x86_64-linux-gnu/c++/8 -I/usr/include/x86_64-linux-gnu -I/usr/include"

$CLANG -cc1 -DBOOST_NO_EXCEPTIONS -analyze -analyzer-checker=core,unix,deadcode,security,nullability,cplusplus -stdlib=libc++ $INCLUDE *.cpp
