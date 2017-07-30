# Learning to use some C++11/14/17 features #

To compile:

```
mkdir build
cd build
cmake .. \
  -DCMAKE_CXX_COMPILER=/usr/bin/clang++ \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON # to use with clang-check, clang-tidy and possibly other clang toolings
make
```
