#include "defs.h"

typedef void *FileImpl;

int read_char_from_file(FileImpl file);
FileImpl open_file(const char *filepath);
void close_file(FileImpl file);

void mem_fill(void *ptr, int val, int size);

void sort_array(void *ptr, int nelems, int elemsize,
                int (*compare)(const void*, const void*));

void *mem_realloc(void *ptr, int size);


void NORETURN fatal(const char *msg, ...);
