#include "defs.h"
#include "api.h"

struct SimpleCmdlineOption {
        const char *optionName;
        int *flagStorage;
};

struct CmdlineOptionWithArgstring {
        const char *optionName;
        String *argStorage;
};

INTERNAL struct SimpleCmdlineOption simpleCmdlineOption[] = {
        { "-debug", &doDebug },
        { "-prettyprint-ast", &doPrettyPrintAst },
        { "-dump-ir", &doDumpIr },
        { "-write-elf-file", &doWriteElfFile },
        { "-write-pecoff-file", &doWritePecoffFile },
};

INTERNAL struct CmdlineOptionWithArgstring cmdlineOptionWithArgstring[] = {
        { "-elf-file", &ELF_ObjectFilepath },
        { "-pecoff-file", &PECOFF_ObjectFilepath },
};

INTERNAL void print_usage(const char *progname)
{
        outf("\n");
        outf("Usage:\n");
        outf("  %s -help\n", progname);
        outf("  %s OPTIONS... SOURCEFILES...\n", progname);
        outf("\n");
        outf("SOURCEFILES should have .bl extension\n");
        outf("\n");
        outf("Valid OPTIONS are:\n");
        outf("  -debug               output more debug info\n");
        outf("  -prettyprint-ast     output AST after syntax parsing\n");
        outf("  -dump-ir             output IR code after flattening phase\n");
        outf("  -write-elf-file      write an ELF object file containing the x64 code\n");
        outf("  -write-pecoff-file   write a PE/COFF object file containing the x64 code\n");
        outf("  -elf-file FILEPATH   override default ELF file path (default:\n"
             "                       last source file but with .o extension\n");
        outf("  -pecoff-file FILEPATH\n"
             "                       override default PE/COFF file path (default:\n"
             "                       last source file but with .obj extension\n");
}

INTERNAL String replace_file_extension(String x, const char *newExtension)
{
        ASSERT(newExtension[0] == '.');
        const char *s = string_buffer(x);
        int len = string_length(x);
        while (len > 0 && s[len] != '.')
                len--;
        if (len == 0)
                len = string_length(x);
        // XXX
        int lengthOfExtension = cstr_length(newExtension);
        char buf[256];
        int totalLength = len + lengthOfExtension;
        if (totalLength > LENGTH(buf))
                FATAL("Filename %s is too long\n", s);
        copy_mem(buf, s, len);
        copy_mem(buf + len, newExtension, lengthOfExtension);
        return intern_string(buf, totalLength);
}

INTERNAL int parse_cmdline(int argc, const char **argv)
{
        int badCmdline = 0;

        for (int i = 0; i < LENGTH(cmdlineOptionWithArgstring); i++)
                *cmdlineOptionWithArgstring[i].argStorage = (String) -1;

        for (int i = 1; i < argc; i++) {
                if (argv[i][0] != '-') {
                        File x = fileCnt++;
                        RESIZE_GLOBAL_BUFFER(fileInfo, fileCnt);
                        fileInfo[x].filepath = intern_cstring(argv[i]);
                        continue;
                }

                for (int j = 0; j < LENGTH(simpleCmdlineOption); j++) {
                        const char *optionName = simpleCmdlineOption[j].optionName;
                        int *flagStorage = simpleCmdlineOption[j].flagStorage;
                        if (cstr_equal(argv[i], optionName)) {
                                *flagStorage = 1;
                                goto nextArg;
                        }
                }

                for (int j = 0; j < LENGTH(cmdlineOptionWithArgstring); j++) {
                        const char *optionName = cmdlineOptionWithArgstring[j].optionName;
                        String *argStorage = cmdlineOptionWithArgstring[j].argStorage;
                        if (cstr_equal(argv[i], optionName)) {
                                i++;
                                if (i == argc)
                                        FATAL("Option %s requires an argument\n", optionName);
                                *argStorage = intern_cstring(argv[i]);
                                goto nextArg;
                        }
                }

                MSG(lvl_error,
                        "Invalid command-line argument: \"%s\"\n", argv[i]);
                badCmdline = 1;

        nextArg:
                continue;
        }

        if (fileCnt == 0) {
                MSG(lvl_error, "No files to compile given on command line\n");
                badCmdline = 1;
        }

        if (badCmdline)
                return 0;

        if (doWriteElfFile && ELF_ObjectFilepath == (String)-1) {
                String filepath = fileInfo[fileCnt - 1].filepath;
                ELF_ObjectFilepath = replace_file_extension(filepath, ".o");
        }

        if (doWritePecoffFile && PECOFF_ObjectFilepath == (String)-1) {
                String filepath = fileInfo[fileCnt - 1].filepath;
                PECOFF_ObjectFilepath = replace_file_extension(filepath, ".obj");
        }

        return 1;
}

int main(int argc, const char **argv)
{
        const char *progname = argv[0];

        if (!parse_cmdline(argc, argv)) {
                print_usage(progname);
                MSG(lvl_error, "bad invocation! Terminating.\n");
                return 1;
        }

        if (doPrintHelpString) {
                print_usage(progname);
                return 0;
        }

        for (File x = 0; x < fileCnt; x++) {
                read_whole_file(x);
        }

        setup_program();

        for (File file = 0; file < fileCnt; file++) {
                DEBUG("Parse file %s\n", string_buffer(fileInfo[file].filepath));
                parse_file(file);
        }

        DEBUG("Fix up parsed data...\n");
        fixup_parsed_data();

        DEBUG("Ensure there are no symbol collisions...\n");
        ensure_there_are_no_symbol_collisions();

        if (doPrettyPrintAst) {
                DEBUG("Pretty print input...\n\n");
                prettyprint();
        }

        DEBUG("Resolve symbol references...\n");
        resolve_symbol_references();

        DEBUG("Resolve type references...\n");
        resolve_type_references();

        DEBUG("Expand macros...\n");
        expand_macros();

        DEBUG("Infer constants and types...\n");
        infer_constants_and_types();

        DEBUG("Print interface file to stdout...\n");
        generate_interface_file();

        DEBUG("Compile to IR...\n");
        compile_to_IR();

        if (doDumpIr) {
                DEBUG("Test IR pretty printer...\n");
                irprint();
        }

        DEBUG("Generate x64 code...\n");
        codegen_x64();

        if (doWriteElfFile) {
                DEBUG("Write elf object file %s...\n",
                        string_buffer(ELF_ObjectFilepath));
                write_elf_file(string_buffer(ELF_ObjectFilepath));
        }

        if (doWritePecoffFile) {
                DEBUG("Write PE64 object file %s...\n",
                        string_buffer(PECOFF_ObjectFilepath));
                write_pecoff_file(string_buffer(PECOFF_ObjectFilepath));
        }

        DEBUG("Success. Cleanup and terminate program.\n");
        teardown_program();

        return 0;
}
