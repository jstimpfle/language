#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif


/*
 * Machine code generation
 */

typedef int SymDef;  // symbol definition position in code
typedef int Reloc; // relocation in code

enum {
        SECTION_DATA,
        SECTION_CODE,
};

struct SymDefInfo {
        Symbol symbol;
        int kind; // SECTION_
        int offset;
        int size;
};

struct GotoInfo {
        int offset;  // place in the code where bytes should be edited
        IrStmt tgtstmt;  // target IR statement of the goto
};

struct RelocInfo {
        Symbol symbol;
        int addend;  // offset relative to position of symbol
        int kind; // SECTION_
        int offset;  // place in the code where bytes should be edited
};

DATA int zerodataSectionCnt;  /* This is the size of zero-initialized data in
                                 bytes.  It's what goes into e.g. the BSS
                                 section in an ELF file.  There's no further
                                 data associated with it, except from the
                                 symbols that point into that virtual region
                                 (symDefInfo) */
DATA int dataSectionCnt;
DATA int codeSectionCnt;
DATA int symDefCnt;
DATA int gotoCnt;
DATA int relocCnt;
DATA unsigned char *dataSection;
DATA unsigned char *codeSection;
DATA struct SymDefInfo *symDefInfo;
DATA struct GotoInfo *gotoInfo;
DATA struct RelocInfo *relocInfo;
DATA int *irstmtToCodepos;  /* map ir statement to the position in the generated
                               machine code where that ir statement is
                               implemented */
DATA int *irprocToCodepos;  /* map ir proc to the position in the generated
                               machine code where that ir proc is implemented */
