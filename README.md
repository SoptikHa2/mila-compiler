# Mila Compiler
LLVM frontend for subset of the Pascal programming language, written in Haskell

## Build & run
```
make
./MilaCompiler source.mila
```

## Dependencies
- Haskell (libraries: `base`, `parsec`; should be included in default installation)
    - GHC, tested: 9.0.2
- Haskell library [llvm-hs](https://github.com/llvm-hs/llvm-hs/)
- llvm 9.0
