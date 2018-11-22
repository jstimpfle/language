#include "defs.h"
#include "api.h"

void free_buffers(void)
{
        for (File i = 0; i < fileCnt; i++)
                BUF_EXIT(&fileInfo[i].buf, &fileInfo[i].bufAlloc);
        for (int i = 0; i < NUM_BUFFERS; i++)
                /* does not work because type of **globalBufferInfo[i].ptr is
                 * not known. */
                //BUF_EXIT(globalBufferInfo[i].ptr, &globalBufferAlloc[i]);
                /* This is the workaround. How to fix this ugly code? */
                _buf_exit(globalBufferInfo[i].ptr, &globalBufferAlloc[i],
                          globalBufferInfo[i].elemsize, __FILE__, __LINE__);
}

#include <stdlib.h>
#include <stdio.h>
int main(int argc, const char **argv)
{
        const char *fileToParse;

        fileToParse = "tests/test2.txt";
        for (int i = 1; i < argc; i++) {
                if (cstr_equal(argv[i], "-debug"))
                        doDebug = 1;
                else if (cstr_equal(argv[i], "-prettyprint-ast"))
                        doPrettyPrintAst = 1;
                else if (cstr_equal(argv[i], "-dump-ir"))
                        doDumpIr = 1;
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

        MSG(lvl_info, "Parse file %s\n", fileToParse);
        parse_global_scope();

        MSG(lvl_info, "Resolve symbol references...\n");
        resolve_symbol_references();

        MSG(lvl_info, "Resolve type references...\n");
        resolve_type_references();

        MSG(lvl_info, "Check types...\n");
        check_types();

        if (doPrettyPrintAst) {
                MSG(lvl_info, "Pretty print input...\n\n");
                prettyprint();
        }

        MSG(lvl_info, "Compile to IR...\n");
        compile_to_IR();

        if (doDumpIr) {
                MSG(lvl_info, "Test IR pretty printer...\n");
                irprint();
        }

        MSG(lvl_info, "A little codegen test...\n");
        codegen_x64();

        MSG(lvl_info, "Write elf object file...\n");
        write_elf64_object("out.o");

        MSG(lvl_info, "Free allocated buffers...\n");
        free_buffers();

        MSG(lvl_info, "Exit successfully.\n");
        return 0;
}
