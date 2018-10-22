#include "defs.h"
#include "api.h"
#include <malloc.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void read_whole_file(File file)
{
        FILE *f;
        size_t nread;
        const int chunksize = 4096;
        String fpath;

        fpath = fileInfo[file].filepath;
        f = fopen(string_buffer(fpath), "rb");
        if (f == NULL)
                FATAL("Failed to open file %s", fpath);

        BUF_INIT(fileInfo[file].buf, fileInfo[file].bufAlloc);
        fileInfo[file].size = 0;
        while (!feof(f) && !ferror(f)) {
                BUF_RESERVE(fileInfo[file].buf,
                            fileInfo[file].bufAlloc,
                            fileInfo[file].size + chunksize);
                nread = fread(fileInfo[file].buf + fileInfo[file].size,
                              1, chunksize, f);
                fileInfo[file].size += (int) nread;
        }
        if (ferror(f))
                FATAL("I/O error while reading from %s\n", fpath);
        fclose(f);
        BUF_RESERVE(fileInfo[file].buf,
                    fileInfo[file].bufAlloc,
                    fileInfo[file].size + 1);
        fileInfo[file].buf[fileInfo[file].size] = '\0';
}

void mem_fill(void *ptr, int val, int size)
{
        memset(ptr, val, size);
}

void mem_copy(void *dst, const void *src, int size)
{
        memcpy(dst, src, size);
}

int mem_compare(const void *m1, const void *m2, int size)
{
        return memcmp(m1, m2, size);
}

int cstr_length(const char *s)
{
        return (int) strlen(s);
}

int cstr_compare(const char *s1, const char *s2)
{
        return strcmp(s1, s2);
}

void *mem_realloc(void *ptr, int size)
{
        return realloc(ptr, size);
}

void sort_array(void *ptr, int nelems, int elemsize,
                int (*cmp)(const void*, const void*))
{
        qsort(ptr, nelems, elemsize, cmp);
}

void NORETURN _fatal(const char *UNUSED filename, int UNUSED line, 
                     const char *msg, ...)
{
        va_list ap;
        fprintf(stderr, "FATAL: ");
#ifndef NODEBUG
        fprintf(stderr, "In %s:%d: ", filename, line);
#endif
        va_start(ap, msg);
        vfprintf(stderr, msg, ap);
        va_end(ap);
        fprintf(stderr, "\n");
        abort();
}

void msg(const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        vfprintf(stderr, fmt, ap);
        va_end(ap);
}

void _buf_init(void **ptr, struct Alloc *alloc, int UNUSED elsize,
               const char *UNUSED file, int UNUSED line)
{
        *ptr = NULL;
        CLEAR(*alloc);
}

void _buf_exit(void **ptr, struct Alloc *alloc, int UNUSED elsize,
               const char *UNUSED file, int UNUSED line)
{
        free(*ptr);
        *ptr = NULL;
        CLEAR(*alloc);
}

void _buf_reserve(void **ptr, struct Alloc *alloc, int nelems, int elsize,
                  int clear, const char *UNUSED file, int UNUSED line)
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
                *ptr = p;
                alloc->cap = cnt;
        }
}
