#ifndef DEFS_H_
#define DEFS_H_

#include <assert.h>

#ifdef _MSC_VER
#define UNUSED
#define NORETURN __declspec(noreturn)
#define UNREACHABLE() assert(0);
#else
#define UNUSED __attribute__((unused))
#define NORETURN __attribute__((noreturn))
#define UNREACHABLE() __builtin_unreachable()
#endif
#define NOTIMPLEMENTED() fatal("In %s:%d: %s(): not implemented!", \
                               __FILE__, __LINE__, __func__)
#define CLEAR(x) mem_fill(&(x), 0, sizeof (x))
#define LENGTH(a) ((int) (sizeof (a) / sizeof (a)[0]))
#define SORT(a, n, cmp) sort_array(a, n, sizeof *(a), cmp)

#ifndef NULL
#define NULL ((void*)0)
#endif

#endif  // DEFS_H_
