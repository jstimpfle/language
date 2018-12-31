This repository contains some of my own experiments in design and
implementation of programming languages. In the long-term, I would like to
design a practical, imperative language with well-defined semantics. At the
surface should be an easy to parse, extensible syntax. This requires a few more
keywords than C has, but that does not mean giving up its conciseness.

I also want to support syntactic extensions and full metaprogramming by giving
access to the internal data structures of the compiler.

Beyond that, the main feature of the language will be extreme minimalism like C
has. The overarching philosophy is that programs should not be specified using
clever syntactical or semantical tricks of the language, since the information
encoded in such ways is hard to process (or even access, in the first place) by
user-defined code.

Status
------

Working on a Compiler to support a minimal set of features.

- [x] Build on Linux
- [x] Build on Windows
- [x] Half-assed "AST" data structures (`include/syntax.h`)
- [x] Half-assed IR data structures (`include/ir.h`)
- [x] Half-assed Parser (`src/parse.c`)
- [x] Pretty-printer for parsed structures (`src/pprint.c`)
- [x] Symbol and type resolution (`src/resolve.c`)
- [x] A simple and sane "hygienic" expression macro system (`src/macroexpand.c`)
- [ ] Decide what other macro facilities we still want
- [x] Half-assed Type checker and inference. (`src/typecheck.c`)
- [x] Half-assed Compiler from "AST" to IR (`src/compile.c`)
- [x] Pretty-printer for compiled IR code (`src/irprint.c`)
- [x] Half-assed IR to x64 machine code generator mostly finished (`src/x64asm.c`)
- [ ] More intelligent Register allocation needed. Generic or x64 specific?
- [ ] Backend from IR to LLVM, any volunteers?
- [x] ELF-64 object writer for Linux supports all currently implemented features (`src/elf64.c`)
- [ ] PE object writer for Windows (`src/pe64.c`)
- [ ] Mach-O object writer for Mac OS, any volunteers?

Build
-----

To build the compiler, run `build.bat` on Windows or `./build.sh` on Linux.

Usage
-----

You can now try and compile one of the example code files in the `test/`
directory. Run for instance:

```sh
build/language tests/fib.txt tests/EXTSYMS.txt -write-elf-object
```

(note: tests/EXTSYMS.txt is currently needed to access the functions in
runtime/support.c)

You can also try one of the following options accepted by the `build/language`
program: `-debug`, `-prettyprint-ast`, `-dump-ir`, `-write-pe-object`.

If the compilation was successful, an ELF-64 object file named `out.o` was
created in the current directory. An object file is a container for machine
code. The ELF-64 format is the standard object file format supported on Linux.

If you are on Linux, you can use the system's compiler and linker to make an
executable file. The executable will contain some additional support functions,
i.e. a runtime, that can be used by the compiled program. You can build the
executable for example like so:

```sh
cc -o out runtime/support.c out.o
```

Then run the executable `./out` to test the program.

Here's a little function for your Linux shell that does these steps

```sh
compile() { build/language "$@" -write-elf-object && cc -Wall -o out runtime/support.c out.o; }
docompile() { local dash=false; for i in "$@" ; do if [ "$i" = -- ] ; then dash=true; fi; if ! "$dash"; then set -- "$@" "$i"; fi; shift; done; compile "$@"; }
dorun() { while [ "$#" -gt 0 ] && [ "$1" != -- ]; do shift; done; shift; ./out "$@"; }
compileandrun() { docompile "$@" && dorun "$@"; }
```
