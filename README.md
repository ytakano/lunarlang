# lunarlang

compile memo (Mac)

```
$ cd src
$ cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=/homebrew/opt/llvm/bin/clang++ -DLLVM_DIR=/homebrew/opt/llvm/share/cmake/modules -G Ninja .
$ ninja -v
```

```
$ cd test/unit
$ cmake -DCMAKE_PREFIX_PATH=/homebrew/opt/llvm -DGTEST_LIBRARY=~/.local/lib/libgtest.a -DGTEST_MAIN_LIBRARY=~/.local/lib/libgtest_main.a -DGTEST_INCLUDE_DIR=~/.local/include -G Ninja .
$ ninja -v
```