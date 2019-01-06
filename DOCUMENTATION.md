Documentation
=============

The necessary data structure definitions are in the files under include/. These
include some half-baked documentation on their own.

The compiler has a simple staged architecture. As of this writing the following
files exist under src/:

Stages
------

```text
    setup.c                01. Set up the compiler 
    parse.c                02. Parse all files
    pprint.c               03. (Optional) pretty-print parsed structures
    resolve.c              04. Link references by name to definitions. Expand
                               type expressions.
    macroexpand.c          05. Expand macro calls
    typecheck.c            06. Check types of all expressions
    constantfold.c         07. Compute all constant values
    ifacegen.c             08. (Optional) Write interface definition describing
                               the things compiled here
    compile.c              09. Translate syntactic structures to IR
    irprint.c              10. Print the IR (Intermediate representation)
    x64asm.c               11. Generate x86-64 code from IR
    elf64.c                12. (Optional) Write x86-64 code to an ELF file
    pe64.c                 13. (Optional) Write x86-64 code to a PECOFF file
    teardown.c             14. Shutdown the compiler, free all used memory
```

Supporting implementation
-------------------------

```text
    data.c              global data (static and dynamic)
    io.c                OS (platform) adapter
    lex.c               tokenizer
    main.c              main driver
    memory.c            memory management
    messages.c          formatting of user-visible messages
    str.c               string interning
    typemetrics.c       a few operations and tests on types
```
