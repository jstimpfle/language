#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif


/*
 * \typedef{File}: A file known to the compiler. Probably its contents will
 * be processed.
 *
 * \typedef{String}: An interned string. We use string interning for most of the
 * string handling because of three reasons: 1) Ease of memory management, 2)
 * Saving working memory, 3) (Probably?) more Efficient equality checks. On the
 * other hand, the disadvantage is that we cannot easily drop a string that was
 * interned.
 */

typedef int File;
typedef int String;
typedef int Type;

/**
 * \enum{ConstStrKind}: Constant strings that get interned at startup as an
 * optimization
 */

enum ConstStrKind {
        CONSTSTR_IF,
        CONSTSTR_ELSE,
        CONSTSTR_WHILE,
        CONSTSTR_FOR,
        CONSTSTR_RETURN,
        CONSTSTR_PROC,
        CONSTSTR_DATA,
        CONSTSTR_ENTITY,
        CONSTSTR_ARRAY,
        NUM_CONSTSTRS,
};

/**
 * \struct{Alloc}: Runtime information to support memory allocation.
 */

struct Alloc {
        int cap;
};

/**
 * \struct{FileInfo}
 */

struct FileInfo {
        String filepath;
        int size;
        unsigned char *buf;
        struct Alloc bufAlloc;
};

DATA struct FileInfo *fileInfo;
DATA int fileCnt;

/**
 * \struct{StringInfo}, \struct{StringBucketInfo}: Helper structures for string
 * interning.
 */

struct StringInfo {
        int pos;  // offset in character buffer
        String next;  // next string in chain
};

struct StringBucketInfo {
        String firstString;
};

DATA int strbufCnt;
DATA int stringCnt;
DATA int strBucketCnt;
DATA char *strbuf;
DATA struct StringInfo *stringInfo;
DATA struct StringBucketInfo *strBucketInfo;

/**
 * \struct{StringToBeInterned} Static information used at program initialization
 * time when constant strings get interned.
 */

struct StringToBeInterned {
        int constant;  // CONSTSTR_
        const char *string;
};


/**
 * Global Buffers
 */

enum {
        /* String */
        BUFFER_stringInfo,
        BUFFER_strBucketInfo,
        /* parsing */
        BUFFER_lexbuf,
        BUFFER_strbuf,
        BUFFER_fileInfo,
        BUFFER_tokenInfo,
        BUFFER_symrefToToken,
        BUFFER_typeInfo,
        BUFFER_symbolInfo,
        BUFFER_dataInfo,
        BUFFER_arrayInfo,
        BUFFER_scopeInfo,
        BUFFER_procInfo,
        BUFFER_procToType,
        BUFFER_paramInfo,
        BUFFER_symrefInfo,
        BUFFER_symrefToSym,
        BUFFER_exprInfo,
        BUFFER_exprType,
        BUFFER_stmtInfo,
        BUFFER_childStmtInfo,
        BUFFER_callArgInfo,
        /* AST -> IR */
        BUFFER_procToIrProc,
        BUFFER_dataToIrReg,
        BUFFER_exprToIrReg,
        /* IR */
        BUFFER_irSymbolInfo,
        BUFFER_irRegInfo,
        BUFFER_irStmtInfo,
        BUFFER_irCallArgInfo,
        BUFFER_irCallResultInfo,
        BUFFER_irReturnResultInfo,
        BUFFER_irProcInfo,
        BUFFER_irLabelInfo,
        BUFFER_irOrigin,
        /* Codegen */
        BUFFER_rodataSection,
        BUFFER_dataSection,
        BUFFER_codeSection,
        BUFFER_symDefInfo,
        BUFFER_gotoInfo,
        BUFFER_relocInfo,
        BUFFER_irstmtToCodepos,
        BUFFER_irprocToCodepos,
        /* X64 Asm */
        BUFFER_x64StackLocInfo,
        /* */
        NUM_BUFFERS,
};

struct GlobalBufferInfo {
        void **ptr;
        int elemsize;
};

extern const char lvl_debug[];
extern const char lvl_info[];
extern const char lvl_warn[];
extern const char lvl_error[];
extern const char lvl_fatal[];

extern const struct GlobalBufferInfo globalBufferInfo[NUM_BUFFERS];
DATA struct Alloc globalBufferAlloc[NUM_BUFFERS];

extern const struct StringToBeInterned stringsToBeInterned[NUM_CONSTSTRS];
DATA String constStr[NUM_CONSTSTRS];  // has initializer
