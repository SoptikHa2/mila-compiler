#!/bin/sh
set -eu
cd /tmp
wget https://github.com/llvm/llvm-project/releases/download/llvmorg-9.0.1/llvm-9.0.1.src.tar.xz
tar xf llvm-9.0.1.src.tar.xz
cd llvm-9.0.1.src
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release -DLLVM_BUILD_LLVM_DYLIB=ON -DLLVM_LINK_LLVM_DYLIB=ON -DLLVM_INSTALL_UTILS=ON -DLLVM_ENABLE_RTTI=ON -DLLVM_ENABLE_FFI=ON -DLLVM_BUILD_TEST=OFF -DLLVM_ENABLE_DOXYGEN=OFF
make -Wno-dev all -j4
cp -r llvm /usr/lib/llvm
echo "/usr/lib/llvm" >> /etc/ld.so.conf
ldconfig
