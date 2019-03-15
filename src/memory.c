#include "defs.h"
#include "api.h"
#include <stdlib.h>

void check_global_buffer_allocations(void)
{
        for (int i = 0; i < NUM_BUFFERS; i++) {
                const char *name = globalBufferInfo[i].__bufferName;
                int cnt = *globalBufferInfo[i].__bufferCnt;
                int cap = globalBufferAlloc[i].cap;
                if (*globalBufferInfo[i].ptr && cnt > cap) {
                        FATAL("Buffer %s: cnt=%d cap=%d\n", name, cnt, cap);
                }
        }
}

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
#ifdef NO_OVERALLOC  /* for debugging purposes */
                cnt = nelems;
#else
                cnt = 2 * nelems - 1;
                while (cnt & (cnt - 1))
                        cnt = cnt & (cnt - 1);
                ASSERT(cnt >= nelems);
#endif
                p = mem_realloc(*ptr, cnt * elsize);
                if (!p)
                        FATAL("OOM!");
                if (clear)
                        clear_mem((char*)p + alloc->cap * elsize,
                                  (cnt - alloc->cap) * elsize);
                *ptr = p;

                if (alloc->cap > 0) {
                        /*
                        DEBUG("Realloc'ing %d more elements (%d bytes). "
                             "increase to %d elems (%d bytes)\n",
                             alloc->cap, alloc->cap * elsize,
                             cnt, cnt * elsize);
                             */
                        DBG_totalbytesrealloced += alloc->cap * elsize;
                        DBG_totalbytesalloced -= alloc->cap * elsize;
                }
                DBG_totalbytesalloced += cnt * elsize;

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
        DEBUG("From %s line %d: Resize buffer %s %d\n",
              filename, line, globalBufferInfo[buf].__bufferName, nelems);
        _buf_reserve(globalBufferInfo[buf].ptr, &globalBufferAlloc[buf],
                nelems, globalBufferInfo[buf].elemsize,
                clear, filename, line);
}
