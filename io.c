#include "io.h"
#include <stdio.h>

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

#include <string.h>
void mem_fill(void *ptr, int val, int size)
{
        memset(ptr, val, size);
}

#include <malloc.h>
void *mem_realloc(void *ptr, int size)
{
        return realloc(ptr, size);
}

#include <stdlib.h>
void sort_array(void *ptr, int nelems, int elemsize,
                int (*cmp)(const void*, const void*))
{
        qsort(ptr, nelems, elemsize, cmp);
}
