#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif

/*
 * Relocation stuff
 */

typedef int SymDef;  // symbol definition position in code
typedef int Reloc; // relocation in code

enum {
        SECTION_CODE,
        SECTION_DATA,
        SECTION_RODATA,
        SECTION_ZERODATA,
        NUM_SECTIONS,
};

struct SymDefInfo {
        Symbol symbol;
        int sectionKind; // SECTION_
        int offset;
        int size;
};

struct GotoInfo {
        int offset;  // place in the code where bytes should be edited
        IrStmt tgtstmt;  // target IR statement of the goto
};

struct RelocInfo {
        Symbol symbol;  /* if -1, the relocation is relative to the section
                           (kind field) */
        int sectionKind; // SECTION_
        int addend;  // offset relative to position of symbol
        int offset;  // place in the code where bytes should be edited
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

DATA int symDefCnt;
DATA int gotoCnt;
DATA int relocCnt;
DATA struct SymDefInfo *symDefInfo;
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
