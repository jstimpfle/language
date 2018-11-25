#include "defs.h"
#include "api.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

NORETURN
void exit_program(int exitcode)
{
        exit(exitcode);
}

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

void clear_mem(void *ptr, int size)
{
        char *p = ptr;
        while (size--)
                *p++ = 0;
}

void copy_mem(void *dst, const void *src, int size)
{
        char *d = dst;
        const char *s = src;
        while (size--)
                *d++ = *s++;
}

int compare_mem(const void *m1, const void *m2, int size)
{
        const char *a = m1;
        const char *b = m2;
        while (size && *a == *b)
                size--, a++, b++;
        if (!size)
                return 0;
        else if (*a > *b)
                return 1;
        else
                return -1;
}

int cstr_length(const char *s)
{
        return (int) strlen(s);
}

int cstr_compare(const char *s1, const char *s2)
{
        return strcmp(s1, s2);
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
        //fflush(stdout);
}

void outfv(const char *fmt, va_list ap)
{
        vfprintf(stdout, fmt, ap);
        //XXX remove this later
        //fflush(stdout);
}

void outf(const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        outfv(fmt, ap);
        va_end(ap);
}

void _abort(void)
{
        abort();
}

void NORETURN _abort_on_failed_assertion(const char * assertion,
                 const char * file, unsigned int line, const char * function)
{
        _fatal(file, line, "In %s(): failed assertion %s\n",
               function, assertion);
}
