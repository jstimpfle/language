/* Generate interface file */

#include "defs.h"
#include "api.h"

void generate_data_iface(Symbol sym)
{
        outf("extern %s ", SS(sym));
        print_type(symbolInfo[sym].tData.tp);
        outs(";\n");
}

void generate_proc_iface(Symbol sym)
{
        Type tp = symbolInfo[sym].tProc.tp;
        Proc p = symbolInfo[sym].tProc.optionalproc;
        ASSERT(p != (Proc) -1);
        outf("extern %s ", SS(sym));
        print_type(tp);
        outs(";\n");
}

void generate_interface_file(void)
{
        for (Export x = 0; x < exportCnt; x++) {
                Symref ref = exportInfo[x].ref;
                Symbol sym = symrefToSym[ref];
                ASSERT(sym != (Symbol) -1);
                switch (symbolInfo[sym].symbolKind) {
                case SYMBOL_TYPE:
                        // TODO: struct and other type definitions
                        break;
                case SYMBOL_DATA:
                        generate_data_iface(sym);
                        break;
                case SYMBOL_ARRAY:
                        break;
                case SYMBOL_PROC:
                        generate_proc_iface(sym);
                        break;
                case SYMBOL_MACRO:
                        break;
                case SYMBOL_MACROPARAM:
                        break;
                default:
                        UNHANDLED_CASE();
                }
        }
}
