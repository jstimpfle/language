#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif

/*
 * Relocation stuff
 */

typedef int Symdef;  // symbol definition position in code
typedef int Reloc; // relocation in code

enum {
        SECTION_CODE,
        SECTION_DATA,
        SECTION_RODATA,
        SECTION_ZERODATA,
        NUM_SECTIONS,
};

enum {
        RELOC_SYMBOL_RELATIVE,
        RELOC_SECTION_RELATIVE,
};

struct SymdefInfo {
        Symbol symbol;
        int sectionKind; // SECTION_
        int offset;
        int size;
};

struct GotoInfo {
        int codepos;  // place in the code where bytes should be edited
        IrStmt tgtstmt;  // target IR statement of the goto
};

struct RelocInfo {
        int relocKind;
        union {
                Symbol tSymbol;  /* if -1, the relocation is relative to the section
                           (kind field) */
                int tSectionKind; // SECTION_
        };
        int addend;  // offset relative to position of symbol
        int codepos;  // place in the code where bytes should be edited
};

/**
 * \data{symDefInfo}, \data{symDefCnt}: Symbol definitions.
 *
 * \data{gotoInfo}, \data{gotoCnt}: relocations for absolute jumps.
 *
 * \data{relocInfo}, \data{relocCnt}: relocations for symbol references.
 *
 * \data{irstmtToCodepos}: map ir statement to the position in the generated
 * machine code where that ir statement is implemented.
 *
 * \data{irprocToCodepos}: map ir proc to the position in the generated machine
 * code where that ir proc is implemented.
 */

DATA int symdefCnt;
DATA int gotoCnt;
DATA int relocCnt;
DATA struct SymdefInfo *symdefInfo;
DATA struct GotoInfo *gotoInfo;
DATA struct RelocInfo *relocInfo;
DATA int *irstmtToCodepos;
DATA int *irprocToCodepos;

/**
 * \data{rodataSection}, \data{rodataSectionCnt}: This is the read-only data
 * section (.rodata in ELF).
 *
 * \data{zerodataSectionCnt}: this is the size of zero-initialized data in
 * bytes (.bss in ELF). There's no further data associated with it, except from
 * the symbols that point into that virtual region (symDefInfo)
 *
 * \data{codeSection}, \data{codeSectionCnt}: This is the program code data
 * section (.text in ELF).
 */

DATA int rodataSectionCnt;
DATA int zerodataSectionCnt;
DATA int dataSectionCnt;
DATA int codeSectionCnt;
DATA unsigned char *rodataSection;
DATA unsigned char *dataSection;
DATA unsigned char *codeSection;
