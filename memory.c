#include "defs.h"
#include "api.h"
#include <stdlib.h>

void *mem_realloc(void *ptr, int size)
{
        return realloc(ptr, size);
}

void _buf_init(void **ptr, struct Alloc *alloc, UNUSED int elsize,
        UNUSED const char *file, UNUSED int line)
{
        *ptr = NULL;
        CLEAR(*alloc);
}

void _buf_exit(void **ptr, struct Alloc *alloc, UNUSED int elsize,
        UNUSED const char *file, UNUSED int line)
{
        free(*ptr);
        *ptr = NULL;
        CLEAR(*alloc);
}

void _buf_reserve(void **ptr, struct Alloc *alloc, int nelems, int elsize,
        int clear, UNUSED const char *file, UNUSED int line)
{
        int cnt;
        void *p;
        if (alloc->cap < nelems) {
                cnt = 2 * nelems - 1;
                while (cnt & (cnt - 1))
                        cnt = cnt & (cnt - 1);
                p = mem_realloc(*ptr, cnt * elsize);
                if (!p)
                        FATAL("OOM!");
                if (clear)
                        mem_fill((char*)p + alloc->cap * elsize, 0,
                        (cnt - alloc->cap) * elsize);
                else {
                        /* maybe this helps for debugging?*/
                        mem_fill((char*)p + alloc->cap * elsize, -1,
                                (cnt - alloc->cap) * elsize);
                }
                *ptr = p;
                alloc->cap = cnt;
        }
}

void _resize_global_buffer(int buf, int nelems, int clear)
{
        _buf_reserve(globalBufferInfo[buf].ptr, &globalBufferAlloc[buf],
                nelems, globalBufferInfo[buf].elemsize, clear, "(N/A)", 0);
}

void _resize_global_buffer_dbg(int buf, int nelems, int clear,
        const char *filename, int line)
{
        _buf_reserve(globalBufferInfo[buf].ptr, &globalBufferAlloc[buf],
                nelems, globalBufferInfo[buf].elemsize,
                clear, filename, line);
}
