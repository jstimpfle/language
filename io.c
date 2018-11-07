#include "defs.h"
#include "api.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void read_whole_file(File file)
{
        FILE *f;
        size_t nread;
        const int chunksize = 4096;
        String fpath = fileInfo[file].filepath;

        f = fopen(string_buffer(fpath), "rb");
        if (f == NULL)
                FATAL("Failed to open file %s\n", string_buffer(fpath));

        BUF_INIT(&fileInfo[file].buf, &fileInfo[file].bufAlloc);
        fileInfo[file].size = 0;
        while (!feof(f) && !ferror(f)) {
                BUF_RESERVE(&fileInfo[file].buf,
                            &fileInfo[file].bufAlloc,
                            fileInfo[file].size + chunksize);
                nread = fread(fileInfo[file].buf + fileInfo[file].size,
                              1, chunksize, f);
                fileInfo[file].size += (int) nread; // XXX: check for overflow
        }
        if (ferror(f))
                FATAL("I/O error while reading from %s\n", string_buffer(fpath));
        fclose(f);
        BUF_RESERVE(&fileInfo[file].buf,
                    &fileInfo[file].bufAlloc,
                    fileInfo[file].size + 1); // XXX: check for overflow
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

void outs(const char *s)
{
        fputs(s, stdout);
        //XXX remove this later
        fflush(stdout);
}

void outf(const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        outfv(fmt, ap);
        va_end(ap);
}

void outfv(const char *fmt, va_list ap)
{
        vfprintf(stdout, fmt, ap);
        //XXX remove this later
        fflush(stdout);
}

void _msg_begin(const char *srcfilename, int srcline,
                const char *loglevel)
{
        fprintf(stdout, "%s:%d:\t", srcfilename, srcline);
        fprintf(stdout, "%s: ", loglevel);
}

void _msg_printf(const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        vfprintf(stdout, fmt, ap);
        va_end(ap);
}

void _msg_printfv(const char *fmt, va_list ap)
{
        vfprintf(stdout, fmt, ap);
}

void _msg_end(void)
{
        fflush(stdout);
}

void _vmsg(const char *srcfilename, int srcline,
           const char *loglevel, const char *fmt, va_list ap)
{
        _msg_begin(srcfilename, srcline, loglevel);
        _msg_printfv(fmt, ap);
        _msg_end();
}

void _msg(const char *srcfilename, int srcline,
          const char *loglevel, const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        _vmsg(srcfilename, srcline, loglevel, fmt, ap);
        va_end(ap);
}

void _abort(void)
{
        abort();
}

void NORETURN _fatal(const char *srcfilename, int srcline,
                     const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        _vmsg(srcfilename, srcline, "FATAL", fmt, ap);
        va_end(ap);
        _abort();
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
