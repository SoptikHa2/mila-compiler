#!/bin/sh
set -e
fullpath="$(realpath "$1")"
fname="$(basename "$1")"
outfname="bins/$fname"
if [ -n "${2-}" ]; then
    outfname="$2"
fi

cd mila
stack run mila-exe -- "$fullpath" > /tmp/"$fname.ll" || cat /tmp/"$fname.ll"
cd ..
clang mila/mila-stdlib.c /tmp/"$fname.ll" -o "$outfname" -Wno-override-module -Wno-implicit-function-declaration
