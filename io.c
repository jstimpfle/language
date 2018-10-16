#include "io.h"
#include <malloc.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int read_char_from_file(FileImpl file)
{
        return fgetc((FILE *) file);
}

FileImpl open_file(const char *filepath)
{
        return (FileImpl) fopen(filepath, "rb");
}

void close_file(FileImpl file)
{
        fclose((FILE *) file);
}

void mem_fill(void *ptr, int val, int size)
{
        memset(ptr, val, size);
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

void NORETURN fatal(const char *msg, ...)
{
        va_list ap;
        va_start(ap, msg);
        fprintf(stderr, "FATAL: ");
        vfprintf(stderr, msg, ap);
        fprintf(stderr, "\n");
        va_end(ap);
        abort();
}
