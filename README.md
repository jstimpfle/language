The blunt programming language
==============================

Blunt is a practical C-like imperative programming language in development.  It
tries to improve on C with a cleaned up syntax, better ergonomics, and a more
robust compilation model. While it affords a few more special words than C has,
it is still concise.

Like with C, the main feature of the language is simplicity. The overarching
philosophy is that programs should not be specified using clever syntactical or
semantical tricks of the language, since the information encoded in such ways
is hard to process (or even access, in the first place) by user-defined code.

Of important note is that blunt significantly improves on C's compile times by
not relying on textual inclusion to access external interfaces. This leads to a
drastic reduction of the number of times interface files need to be read, at
least compared to C header guards. In many cases, the number of lines that must
be parsed is reduced by 10x or more.

Some support for metaprogramming is offered by expression macros. Blunt
expression macros are similar to C macros, but they are "hygienic" in the sense
that free variables are bound at the macro definition site. For this to work,
blunt expression macros must operate on the expression level, so they are
restricted to valid blunt expressions -- unlike C macros which operate on the
token level.

Important influences for blunt's design: First, the venerable C programming
language. Two more recent projects which have caught some attention: the Jai
language by Jonathan Blow and the Bitwise project (including the ion language)
by Per Vognsen. I agree with many of the points made by these two guys, and
especially Jonathan Blow has done countless rants that are spot-on about the
current situation of our development ecosystems.

blunt is not as feature-happy as Jai, though. It aims to be ergonomic by
offering slightly more built-in features than ion and C have, but it's careful
not to increase complexity. For example, blunt adds operators that return the
number of elements in a static array because it's clear that this feature won't
interact with other features in unforeseen ways. But it doesn't add a complex
object system or similar bells and whistles.


The blunt compiler
------------------

The current implementation is not optimized, but it already compiles more than
1 million lines per second on standard desktop computers (tested with a naive
x64 backend, and only on dummy code. Big projects do not exist yet).

Metaprogramming of data structures and constant data declarations will shortly
be possible by compiling extensions into the compiler. For this to work, blunt
projects should probably keep an in-tree copy of the compiler. Which might be
fine since the compiler is rather small and simple to understand.

Status
------

Working on a Compiler to support a minimal set of features. Features that are
marked as done here are mostly functional and usable. Nevertheless much of the
implementation will be extended or restructured completely.

- [x] Build on Linux
- [x] Build on Windows
- [x] Half-assed "AST" data structures (`include/syntax.h`)
- [x] Half-assed IR data structures (`include/ir.h`)
- [x] Syntax parser (`src/parse.c`)
- [x] Pretty-printer for parsed structures (`src/pprint.c`)
- [x] Symbol and type resolution (`src/resolve.c`)
- [x] A simple and sane "hygienic" expression macro system (`src/macroexpand.c`)
- [x] Interface file generator (`src/ifacegen.c`)
- [ ] Decide what other macro facilities we still want
- [x] Type checker and inference. (`src/typecheck.c`)
- [x] Compiler from "AST" to IR (`src/compile.c`)
- [x] Pretty-printer for compiled IR code (`src/irprint.c`)
- [x] Half-assed IR to x64 machine code generator mostly finished (`src/x64asm.c`)
- [ ] More intelligent Register allocation needed. Generic or x64 specific?
- [ ] Backend from IR to LLVM, any volunteers?
- [x] ELF-64 object writer for Linux (`src/elf64.c`)
- [x] PE object writer for Windows (`src/pecoff.c`)
- [ ] Mach-O object writer for Mac OS, any volunteers?

A list of bugs, structural problems, and cross-cutting concern type missing
things, can be found in TODO.txt.

Build
-----

To build the compiler, run `build.bat` on Windows or `./build.sh` on Linux.

Usage
-----

You can now try and compile one of the example code files in the `test/`
directory. Run for instance:

```sh
./blunt tests/EXTSYMS.bl tests/fib.bl -write-elf-file -elf-file fib.o
```

(note: tests/EXTSYMS.bl is an interface file. It holds the function signatures
of a few functions defined in libc, and of the functions defined in
runtime/support.c)

You can also try one of the following options accepted by the blunt compiler:
`-debug`, `-prettyprint-ast`, `-dump-ir`, `-write-pecoff-file`.

If the compilation was successful, an ELF-64 object file named `fib.o` was
created in the current directory. An object file is a container for machine
code. The ELF-64 format is the standard object file format supported on Linux.

If you are on Linux, you can use the system's compiler and linker to make an
executable file. The executable will contain some additional support functions,
i.e. a runtime, that can be used by the compiled program. You can build the
executable for example like so:

```sh
cc -o out runtime/support.c fib.o
```

Then run the executable `./out` to test the program.

The `devel/dev.sh` script has a few functions that reduce the typing needed to
compile and run a file. Source the script into your running shell with `source
devel/dev.sh`.
