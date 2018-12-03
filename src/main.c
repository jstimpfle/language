#include "defs.h"
#include "api.h"

void cleanup(void)
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

INTERNAL
void print_usage(const char *progname)
{
        outf("\n");
        outf("Usage:\n");
        outf("  %s -help\n", progname);
        outf("  %s [-debug] [-dump-ir] [-prettyprint-ast] <program.txt>\n",
             progname);
        outf("\n");
}


int main(int argc, const char **argv)
{
        const char *progname = argv[0];

        for (int i = 1; i < argc; i++) {
                if (argv[i][0] == '-') {
                        if (cstr_equal(argv[i], "-help"))
                                wantHelp = 1;
                        else if (cstr_equal(argv[i], "-debug"))
                                doDebug = 1;
                        else if (cstr_equal(argv[i], "-prettyprint-ast"))
                                doPrettyPrintAst = 1;
                        else if (cstr_equal(argv[i], "-dump-ir"))
                                doDumpIr = 1;
                        else {
                                MSG(lvl_error, "Invalid command-line argument \"%s\"\n", argv[i]);
                                goto bad;
                        }
                }
                else {
                        fileToParse = argv[i];
                }
        }
        if (fileToParse == 0) {
                MSG(lvl_error, "No file to compile given on command line\n");
                goto bad;
        }

        if (0) {  /* i wonder which compilers will trip on this */
bad:
                print_usage(progname);
                MSG(lvl_error, "bad invocation! Terminating.\n");
                return 1;
        }

        if (wantHelp) {
                print_usage(progname);
                return 0;
        }

        initialize_pseudo_constant_data();
        {
        File x = fileCnt++;
        RESIZE_GLOBAL_BUFFER(fileInfo, fileCnt);
        fileInfo[x].filepath = intern_cstring(fileToParse);
        read_whole_file(x);
        }

        DEBUG("Parse file %s\n", fileToParse);
        parse_global_scope();

        DEBUG("Resolve symbol references...\n");
        resolve_symbol_references();

        DEBUG("Resolve type references...\n");
        resolve_type_references();

        /* XXX: this must currently come *after* resolve phase. TODO move the
         * relevant code that fixes the indices out of resolve.c. */
        if (doPrettyPrintAst) {
                DEBUG("Pretty print input...\n\n");
                prettyprint();
        }

        DEBUG("Check types...\n");
        check_types();

        DEBUG("Compile to IR...\n");
        compile_to_IR();

        if (doDumpIr) {
                DEBUG("Test IR pretty printer...\n");
                irprint();
        }

        DEBUG("A little codegen test...\n");
        codegen_x64();

        DEBUG("Write elf object file...\n");
        write_elf64_object("out.o");

        DEBUG("Success. Cleanup and terminate program.\n");
        cleanup();

        return 0;
}
