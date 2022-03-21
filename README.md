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

## Compiler-specific behaviour

This compiler defines custom instruction `comeFrom`. Simmilar to goto:
```
a:; // label A

...

goto a; // when we get here, jump to label A
```
Instruction comeFrom uses labels to jump arbitrarily in the program. Just in reverse way compared to goto.
```
comeFrom a; // when we get to label A, we jump here

...

a:; // label A
```
The key question is: what is the behaviour in case of multiple labels per one `comeFrom`?
```
a:;

...

comeFrom a;

...

comeFrom a;

...

comeFrom a;

...
```
Where does the program jump? The correct answer is `yes`. We simply fork the currently running program, and let it run
simultaneously from each comeFrom instruction at once. Paralelism can be implemented this way!
