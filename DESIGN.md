Features and Design decisions made in blunt
===========================================

This document was started kind of late, and by now I've probably forgotten half
of the conscious design decisions I've made, or at least forgotten the
reasoning behind them. Nevertheless I think it is important to talk about
concrete features, and why they are in blunt or why they aren't.

Two big old inspirational languages for the design of blunt were C and recently
Pascal (in the form of Delphi). C is undeniably the more practical language.
Actually I think Delphi is a failure as a language. I had to use
Delphi at work in 2018 and it's a pile of crap that has accreted the worst of
the hyped language features from the last 30 years (especially the 90's
object-oriented craze is represented as incoherent non-orthogonal features).
I also don't have many nice words to say about Pascal's rigidity and
impractical minimalism. Nevertheless, Pascal teaches some important lessons of
simplicity in language design and compiler implementation, and even in Delphi
that still shines through (not only in incredibly faster compilation times). I
definitely want to have some of the spirit from Pascal in blunt, and forgo some
of the hackish things in C.

No preprocessor
---------------

Blunt does not require the use of a lexical preprocessor. Like in C, there is
no technical distinction between a module interface and an implementation file.
Both contain regular Blunt source code. But in blunt, the implementation does
not explicitly import the interface. Interfaces are made known to the
implementation simply by compiling them together in a single job (i.e. list
both files on the command line).

Since there is no preprocessor and no #include statement, the compilation is
not sensitive to where the interface was included and what state the
preprocessor was in at that point. This little restriction allows us to use a
single parse/compilation of the interface file to compile many implementation
files. This brings huge speed-ups compared to C, where huge include files are
often a big issue.

No import statements
--------------------

Blunt takes the idea of giving all dependencies on the compile command-line to
the extreme: Unlike almost all other languages, it does not even have import
statements! Personally I think they are a lot of boilerplate, and it's never
clear who should decide what interface file is actually imported: The build
system (by configuring the import paths in a certain way) or the implementation
(with carefully crafted hierarchical paths in the import statement).

For blunt I've decided that it's not the implementation's job to decide how
it's plumbed in a larger project. A blunt implementation file simply uses the
artifacts that it depends on (constants, data, functions, macros...). The
plumbing is left for an external build system.

In the future, restricted forms of static asserts will probably be added, and
implementations can use those to check that a plumbed interface is actually the
one it expects. For example, by requiring a global constant to be defined to a
certain value or range of values. We could actually consider adding to blunt an
import statement that would be nothing more than a glorified static assert
(with more sensible syntax and error messages).

No namespaces
--------------

Similarly, I think namespaces are not helpful. They mainly make it possible to
use short names without never really declaring them, which is confusing to the
programmer. In general, we should strive to use one and only one name,
throughout the project, for any given thing. And conversely, a given name
should ideally always reference the same thing throughout the project
(function-local variables are an exception here, of course).

This is basic sanity. I have no understanding how namespaces got so popular.
They allow the programmer to write bad code, and are not very helpful
otherwise. (Okay -- they are compile time functions from (namespace,name) to
implementation. Which can be used e.g. for static polymorphism like in C++
templates. But that's not 90% of their use in practice. And I have no intention
to support clever tricks like that).


True compiler constants
-----------------------

Blunt has a "constant" directive (like pascal) that supports limited constant
folding, so we don't need a preprocessor to give names to compile-time
constants.

```
constant FOO = 0;
constant BAR = 1;
constant FOOBAR = FOO + BAR;
```

Constants are not explicitly typed currently. The idea is that they should be
"like expressions". I still need to work on the story for that, though. Maybe
it makes sense to add some form of explicit typing to constants.

Blunt also has an "enum" directive, but it's more restricted than C enums in
that values always start at 0, incrementing by 1.

```
enum {
    FOO;  /* 0 */
    BAR;  /* 1 */
    BAZ;  /* 2 */
}
```

Not allowing whacky things like this:

```
enum {
    FOO;       /*  0 */
    BAR = 42;  /* 42 */
    BAZ;       /* 43 */
}
```

(or similar), enables certain future extensions. If you really need something
like this, just use constants.

Expression macros
-----------------

Since we don't rely on a preprocessor, we need something else to avoid
boilerplate in a quick and dirty fashion. Regular functions cannot help in all
cases since they are executed only at runtime. In blunt I want to see how far
we get with simple expression macros. These macros are somewhat similar to C,
but the expansions are done at the syntactical level instead of the lexical.
In other words, blunt macros must expand to valid blunt expressions.

```
macro PRINTVALUE(x) => _print_value(#stringify x, x);

proc _print_value(varname ^char, x int) void
{
    printf("%s %d\n", varname, x);
}

proc test() void
{
    data foo int = 42;
    PRINTVALUE(foo);  /* prints "foo 42", _not_ "x 42" !! */
}
```

The advantage of expression macros compared to C macros is that expression
macros are "hygienic" in the sense that free variables are bound at the macro
definition site.

The disadvantage is that they are less flexible. If you need to reference
variables from the call site in the replacement expression, you need to
explicitly feed them as arguments to the macro (or define a helper macro in the
calling scope). And you cannot do things like defining an APPLES_FOREACH(name,
container) macro because that is not an expression. So probably, we will need
to consider additional mechanisms in the future.

The #lengthof() builtin
---------------------

The #lengthof() builtin is a feature missing from C for no good reason. It's
easy to implement and compared to the typical (sizeof (a) / (a)[0]) macro that
C programmers define to emulate it, it's not prone to errors (it cannot
mistakenly be used with pointers). I don't think there are any downsides to
having #lengthof except for requiring a few simple lines of code in the
compiler.

```
array arr [3]int;

proc printsizes() int
{
    printf("%d\n", #lengthof(arr));   /* prints 3 */
    printf("%d\n", #sizeof(arr[0]));  /* prints 8 */
    printf("%d\n", #sizeof(arr));     /* prints 24 */
}
```

Processing global directives in order
-------------------------------------

Different from languages with reference-only semantics, in languages with
"value semantics" we cannot have cycles in type definitions. We cannot, for
example, define

```
struct A {
    data blah int;
    data B b;
}

struct B {
    data A a;
}
```

since allowing that would essentially make A and B infinitely large.  When
detecting a cyclic dependency like that, the question is where is the
programmer's mistake? Is A mistakenly including B? Or is B mistakenly including
A? Where should the cycle be broken? In my opinion the compiler should not do
second guessing, and choosing a random order only leads to confusing error
messages.

Any cyclic dependency problem could be due to a simple mistake, like a typo
that the compiler cannot see, and so it could print a confusing error message
or suggest an overly complex fix.

This is why I've decided that types (and constants) can only depend on other
types (and constants) that are already defined (e.g. earlier in the file).
This requires the programmer to be a little more disciplined, but I think it's
worth it. It also leads to better readable programs. And forcing the programmer
to essentially do a manual topographical sort also means that the programmer is
encouraged to think about dependencies properly, which ultimately leads to
simpler and better programs.

So, this is not allowed:

```
constant bar = 2 * foo;
constant foo = 42;

struct B {
    data A x;
    data A y;
}

struct A {
    data int r;
    data int s;
    data int t;
}
```

although there are no cyclic dependencies. Instead the programmer must write

```
constant foo = 42;
constant bar = 2 * foo;

struct A {
    data int r;
    data int s;
    data int t;
}

struct B {
    data A x;
    data A y;
}
```

The problem with allowing declarations in any order is this: While the program
might still be meaningful (no cycles), a simple mistake can create a cycle and
lead to bad error messages. I don't want to go there.

Note: This decision does not apply to the earlier symbol resolution phase.
Symbol resolution is not restricted to earlier definitions. I don't want to
require function and struct prototyping like C requires to allow lexical cycles
that are technically possible (pointer indirection, linker fixups...). The
following would be allowed:

```
data x ^int = &amp;y;
data y ^int = &amp;x;

struct B {
    data A ^x;
    data A ^y;
}

struct A {
    data int r;
    data int s;
    data int t;
}
```

I still need to figure out the fine details of name resolution (again, stricter
rules are beneficial sometimes), but in general the resolver looks at all
symbols in all reachable scopes, including symbols that are defined later in
the scope where the reference was made.
