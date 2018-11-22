This repository contains some of my own experiments in design and
implementation of programming languages. In the long-term, I would like to
design a practical, imperative language with well-defined semantics and a clean
and easy to parse syntax which can support syntactic extensions. The latter
means adding a few more keywords at important places to avoid ambiguity (see
"Lexer Hack" for an example).

Status
------

Working on a Compiler to support a minimal set of features.

- [x] Build on Linux
- [x] Build on Windows
- [x] Half-assed "AST" data structures (See api.h)
- [x] Half-assed IR data structures (See api.h)
- [x] Half-assed Parser (See parse.c)
- [ ] Half-assed Type checker and inference partly finished. (See typecheck.c)
- [x] Half-assed Compiler from "AST" to IR (See compile.c)
- [x] Half-assed IR to x64 machine code generator mostly finished (See x64asm.c)
- [ ] More intelligent Register allocation needed. Generic or x64 specific?
- [ ] Backend from IR to LLVM, any volunteers?
- [x] ELF-64 object writer for Linux supports all currently implemented features
- [ ] PE object writer for Windows
- [ ] Mach-O object writer for Mac OS, any volunteers?

Build
-----

To build the compiler, run `build.bat` on Windows or `./build.sh` on Linux.

Usage
-----

To run the compiler, run the `build/language` executable (`build\language.exe`
on Windows). This compiles a single code file (default: a file in the `test/`
directory) to x64 machine code.

For now, an ELF-64 object file named `out.o` is created in the current
directory. An object file is a container for machine code. The ELF-64 format is
the standard object file format supported on Linux.

If you are on Linux, you can use the system's compiler and linker to make an
executable file. The executable will contain some additional support functions,
i.e. a runtime, that can be used by the compiled program. You can build the
executable for example like so:

```sh
cc -o out runtime/support.c out.o
```

Then run the executable `./out` to test the program.
