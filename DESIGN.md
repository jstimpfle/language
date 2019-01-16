Features and Design decisions made in blunt
===========================================

This document was started kind of late, and by now I've probably forgotten half
of the conscious design decisions I've made, or at least forgotten the
reasoning behind them. I'll try to recollect as many as possible because I
think it is important to talk about concrete features and why they are in
blunt or why they aren't.

True compiler constants
-----------------------

Blunt has a `constant` directive that supports limited constant folding, which
can be used to give names to compile-time constants.

```
constant FOO = 3;
constant BAR = 5;
constant FOOBAR = FOO + BAR;
```

Constants are not explicitly typed currently. The idea is that they should be
"like expressions". I still need to work on the story for that, though. Maybe
it makes sense to add some form of explicit typing to constants.

The above constant declarations can be expressed as the following C code:

```
/* C code */
enum {
    FOO = 3,
    BAR = 5,
    FOOBAR = FOO + BAR;
};
```

I believe there isn't any fundamental difference between declaring an integer
constant in blunt and declaring one in a C enum block. But enums are restricted
to integers, while constants can be extended to strings, floats, or maybe even
user-defined types. C only has enums (and preprocessor constants), so constants
are a little improvement there.

Blunt has an `enum` directive, too. But it's more restricted than C enums in
that one cannot assign explicit values to the enum constants. Blunt counts up
strictly in 1-increments, starting at 0 for the first value.

```
enum {
    FOO;  /* 0 */
    BAR;  /* 1 */
    BAZ;  /* 2 */
}
```

There is no technical difference between constants declared with constant
directives and constants declared with enum directives. There is no "enum
type". The only difference is syntactic. The restricted form (compared with
constant directives) might allow for certain future extensions. I'm primarily
thinking of generating string arrays that contain the names of the enum
constants. But if you really need to go wild, just use constants.

Expression macros
-----------------

Since we don't rely on a preprocessor, we need something else to help avoid
repetitive syntax. In blunt I want to see how far we get with simple expression
macros. These macros are somewhat similar to C's macros, but the expansions are
done at the syntactical level instead of the lexical level. In other words,
blunt macros must expand to valid blunt expressions.

```
macro ALLOC(x, n) => _alloc(&x, n, #sizeof(x^));

proc _alloc(r ^^void, nelems int, elemsize int) void
{
    data ptr ^void = calloc(nelems, elemsize);
    if (!ptr)
        FATAL("OOM!\n");
    r^ = ptr;
}

proc test() void
{
    data p ^int;
    ALLOC(p, 42);
    p[41] = 3;
    return 0;
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

proc printsizes() void
{
    printf("%d\n", #lengthof(arr));   /* prints 3 */
    printf("%d\n", #sizeof(arr[0]));  /* prints 8 */
    printf("%d\n", #sizeof(arr));     /* prints 24 */
}
```

Maybe it is, for arrays of explicit size, that the best practice is to simply
declare a constant first and then use that as the array length. 

```
constant ARRLEN = 3;
array arr [ARRLEN]int;

proc printsizes() void
{
    printf("%d\n", ARRLEN);   /* prints 3 */
}
```

But I'm not entirely sold. And there is no way around some kind of #lengthof()
calculation for arrays with implicit size (note, implicitly sized arrays aren't
implemented yet).

```
array arr []int = { 53, 22, 107, 38 };

proc printsizes() void
{
    printf("%d\n", #lengthof(arr));   /* prints 4 */
}
```

First class static arrays and no array decay
--------------------------------------------

In C, there isn't really a first-class type for static arrays. Whenever you
name a static array, that expression is equivalent to taking a pointer to its
first element. This is called array decay.

```
char line[64];

int main(void)
{
    fgets(line, sizeof line, stdout);  /* note: `sizeof line` is 64, but the
                                          first argument is just a pointer */
    return 0;
}
```

I think this is a little weird, so I'm experimenting with first class static
array types. For example, `[3]^int` is the type of arrays containing three
integer pointers.  And `^[3]int` is a pointer type to an array of three
integers.

Since blunt has first class static array types, it forgoes array decay.  In
Blunt you need to explicitly take the address of the array to get a pointer to
the first element.

```
array line [64]char;

int main(void)
{
    fgets(&line[0], #sizeof(line), stdout);
    /* (first argument can be shortened to `&line`. See below) */
    return 0;
}
```

Having first class array types makes a difference for function interfaces:
The following is valid C code, since `int[N]` is equivalent to `*int`.

```
void ptrfunc(int *x);
void arrayfunc(int x[3]);

int x;
int xs[3];

void test(void)
{
    ptrfunc(&xs);   /* allowed */
    arrayfunc(&x);  /* likely a bug, allowed by the compiler */
}
```

In blunt, while `^[N]T` arguments (for any array size N and element type T) are
compatible with `^T` function parameters, the converse is not true. I think it
is worthwhile to be strict here.

```
extern proc ptrfunc   void(x ^int);
extern proc arrayfunc void(x ^[3]int);

data int x;
array xs [3]int;

proc test2(void) void;
{
    ptrfunc(&xs);   /* allowed */
    arrayfunc(&x);  /* likely a bug, NOT allowed by the compiler */
}
```

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
statements! The justification is that I think import statements are a lot of
boilerplate, and it's not clear who actually controls what interface file is
actually imported: The build system (by configuring the import paths in a
certain way) or the implementation (with carefully crafted hierarchical paths
in the import statement).

For blunt I've decided that it's not the implementation's job to decide how
it's plumbed in a larger project. A blunt implementation file simply uses the
artifacts that it depends on (constants, data, functions, macros...). The
plumbing is left for an external build system.

In the future, restricted forms of static asserts will probably be added, and
implementations can use those to check that a plumbed interface is actually the
one it expects by inventing an arbitrary protocol. For example, requiring the
interface to define a global constant to a certain value or range of values.

In fact I might consider adding to blunt an import statement that would be
nothing more than a glorified static assert (with more sensible syntax and
error messages).

No namespaces
--------------

Similarly, I think namespaces are not helpful. They mainly make it possible to
use short names without never really declaring them, which is confusing to the
programmer. In general, we should strive to use one and only one name,
throughout the project, for any given thing. And conversely, a given name
should ideally reference the same thing throughout the project (function-local
variables are an exception here, of course).

This is basic sanity. I have no understanding how namespaces got so popular.
They allow the programmer to write bad code, and are not very helpful
otherwise. (Okay -- a namespace is a compile time function from name to
implementation. Which can be used e.g. for static polymorphism like in C++
templates. But that's not 90% of their use in practice. And I have no intention
to support clever tricks like that).

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

since allowing that would essentially make A and B infinitely large. When
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

The problem with allowing declarations even for "sensible" programs is that a
simple mistake could introduce a cycle and cause a hard to debug error. I don't
want to go there.

Note: This decision does not apply to the earlier symbol resolution phase.
Resolution of symbol references is not restricted to definitions that come
before the reference. I don't want to require function and struct prototyping
like C requires to allow lexical cycles that are technically possible (pointer
indirection, linker fixups...). The following would be allowed:

```
data x ^int = &y;
data y ^int = &x;

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
