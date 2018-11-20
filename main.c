#include "defs.h"
#include "api.h"

void free_buffers(void)
{
        for (File i = 0; i < fileCnt; i++)
                BUF_EXIT(&fileInfo[i].buf, &fileInfo[i].bufAlloc);
        for (int i = 0; i < NUM_BUFFERS; i++)
                BUF_EXIT(globalBufferInfo[i].ptr, &globalBufferAlloc[i]);
}

#include <stdlib.h>
#include <stdio.h>
int main(int argc, const char **argv)
{
        const char *fileToParse;

        fileToParse = "test2.txt";
        for (int i = 1; i < argc; i++) {
                if (cstr_compare(argv[i], "-debug") == 0)
                        doDebug = 1;
                else
                        fileToParse = argv[i];
        }

        initialize_pseudo_constant_data();
        {
        File x = fileCnt++;
        RESIZE_GLOBAL_BUFFER(fileInfo, fileCnt);
        fileInfo[x].filepath = intern_cstring(fileToParse);
        read_whole_file(x);
        }

        MSG(lvl_info, "Parsing file %s\n", fileToParse);
        parse_global_scope();

        MSG(lvl_info, "Resolving symbol references...\n");
        resolve_symbol_references();

        MSG(lvl_info, "Resolving type references...\n");
        resolve_type_references();

        MSG(lvl_info, "Checking types...\n");
        check_types();

        MSG(lvl_info, "Pretty printing input...\n\n");
        prettyprint();

        MSG(lvl_info, "Compiling to IR...\n");
        compile_to_IR();

        MSG(lvl_info, "Test IR pretty printer...\n");
        irprint();

        MSG(lvl_info, "A little codegen test...\n");
        codegen_x64();

        MSG(lvl_info, "Write elf object file\n");
        write_elf64_object("out.o");

        MSG(lvl_info, "Freeing allocated buffers...\n");
        free_buffers();

        MSG(lvl_info, "Exiting successfully.\n");
        return 0;
}
