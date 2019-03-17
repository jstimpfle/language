#include <defs.h>
#include <api.h>

/* TODO: The approach with begin_symbol() / end_symbol() is too complicated. We
 * should rather emit the symDefs for procs mostly independently of the machine
 * code generation for the procs, filling in only the size of the proc code
 * after the proc code is generated. */
void begin_symbol(Symbol sym)
{
        Symdef sd = symdefCnt++;
        RESIZE_GLOBAL_BUFFER(symdefInfo, symdefCnt);
        symdefInfo[sd].symbol = sym;
        symdefInfo[sd].sectionKind = SECTION_CODE;
        symdefInfo[sd].offset = codeSectionCnt;  //XXX Alignment?
        symdefInfo[sd].size = 0; // set later
}

void end_symbol(void)
{
        Symdef sd = symdefCnt - 1;
        symdefInfo[sd].size = codeSectionCnt - symdefInfo[sd].offset;
}

Symdef emit_symdef(Symbol sym, int sectionKind, int offset, int size)
{
        ASSERT(symbolInfo[sym].symbolKind == SYMBOL_DATA);
        Symdef symdef = symdefCnt++;
        RESIZE_GLOBAL_BUFFER(symdefInfo, symdefCnt);
        symdefInfo[symdef].symbol = sym;
        symdefInfo[symdef].sectionKind = sectionKind;
        symdefInfo[symdef].offset = offset;
        symdefInfo[symdef].size = size;
        return symdef;
}

void emit_relocation(Symbol symbol, int codepos)
{
        emit_relative_relocation(symbol, 0, codepos);
}

void emit_relative_relocation(Symbol symbol, int addend, int codepos)
{
        int x = relocCnt++;
        RESIZE_GLOBAL_BUFFER(relocInfo, relocCnt);
        relocInfo[x].relocKind = RELOC_SYMBOL_RELATIVE;
        relocInfo[x].tSymbol = symbol;
        relocInfo[x].addend = addend;
        relocInfo[x].codepos = codepos;
}

void emit_section_relative_relocation(int sectionKind, int addend, int codepos)
{
        int x = relocCnt++;
        RESIZE_GLOBAL_BUFFER(relocInfo, relocCnt);
        relocInfo[x].relocKind = RELOC_SECTION_RELATIVE;
        relocInfo[x].tSectionKind = sectionKind;
        relocInfo[x].addend = addend;
        relocInfo[x].codepos = codepos;
}

void emit_bytes(int sectionKind, const void *buf, int size)
{
        if (sectionKind == SECTION_CODE) {
                int pos = codeSectionCnt;
                codeSectionCnt += size;
                RESIZE_GLOBAL_BUFFER(codeSection, codeSectionCnt);
                copy_mem(codeSection + pos, buf, size);
        }
        else if (sectionKind == SECTION_DATA) {
                int pos = dataSectionCnt;
                dataSectionCnt += size;
                RESIZE_GLOBAL_BUFFER(dataSection, dataSectionCnt);
                copy_mem(dataSection + pos, buf, size);
        }
        else if (sectionKind == SECTION_RODATA) {
                int pos = rodataSectionCnt;
                rodataSectionCnt += size;
                RESIZE_GLOBAL_BUFFER(rodataSection, rodataSectionCnt);
                copy_mem(rodataSection + pos, buf, size);
        }
        else {
                UNHANDLED_CASE();
        }
}
