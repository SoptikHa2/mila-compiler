#!/bin/sh
set -eu
cd mila
stack run -- samples/"$1.mila" | tee /tmp/"$1.ll"
cd ..
clang mila/mila-stdlib.c /tmp/"$1.ll" -o "$1"
./"$1"
