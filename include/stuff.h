#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif



/**
 * PE 64
 *
 * TODO: find better place for these declarations
 * */
int pe64strCnt;
char *pe64strtab;


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

/**
 * \enum{ConstStrKind}: Constant strings that get interned at startup as an
 * optimization
 */

enum ConstStrKind {
        CONSTSTR_IF,
        CONSTSTR_ELSE,
        CONSTSTR_WHILE,
        CONSTSTR_FOR,
        CONSTSTR_DO,
        CONSTSTR_RETURN,
        CONSTSTR_BREAK,
        CONSTSTR_PROC,
        CONSTSTR_MACRO,
        CONSTSTR_STRUCT,
        CONSTSTR_DATA,
        CONSTSTR_ENTITY,
        CONSTSTR_ARRAY,
        CONSTSTR_EXPORT,
        CONSTSTR_IN,
        CONSTSTR_FROM,
        CONSTSTR_TO,
        CONSTSTR_DOWNTO,
        CONSTSTR_SIZEOF,
        CONSTSTR_IGNORE,
        CONSTSTR_EXTERN,
        CONSTSTR_STRINGIFY,
        CONSTSTR_CONSTANT,
        CONSTSTR_ENUM,
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

/* Here we are using an old technique for avoiding code duplication, called
 * "X-macros". See locations of usage to understand better. */

/*
   TODO: a better version of this table would be:
       (bufferName, indexType, valueType)
   grouped by topic / file name.
*/
#define GLOBAL_BUFFERS_X_MACRO  \
        MAKE_GLOBAL_BUFFER(  callArgCnt,         callArgInfo          ),  \
        MAKE_GLOBAL_BUFFER(  childStmtCnt,       childStmtInfo        ),  \
        MAKE_GLOBAL_BUFFER(  codeSectionCnt,     codeSection          ),  \
        MAKE_GLOBAL_BUFFER(  constantCnt,        constantInfo         ),  \
        MAKE_GLOBAL_BUFFER(  constantCnt,        constantValue        ),  \
        MAKE_GLOBAL_BUFFER(  dataCnt,            dataInfo             ),  \
        MAKE_GLOBAL_BUFFER(  dataCnt,            dataToIrReg          ),  \
        MAKE_GLOBAL_BUFFER(  dataSectionCnt,     dataSection          ),  \
        MAKE_GLOBAL_BUFFER(  exportCnt,          exportInfo           ),  \
        MAKE_GLOBAL_BUFFER(  exprCnt,            exprInfo             ),  \
        MAKE_GLOBAL_BUFFER(  exprCnt,            exprToIrReg          ),  \
        MAKE_GLOBAL_BUFFER(  exprCnt,            exprType             ),  \
        MAKE_GLOBAL_BUFFER(  fileCnt,            fileInfo             ),  \
        MAKE_GLOBAL_BUFFER(  gotoCnt,            gotoInfo             ),  \
        MAKE_GLOBAL_BUFFER(  irCallArgCnt,       irCallArgInfo        ),  \
        MAKE_GLOBAL_BUFFER(  irCallResultCnt,    irCallResultInfo     ),  \
        MAKE_GLOBAL_BUFFER(  irLabelCnt,         irLabelInfo          ),  \
        MAKE_GLOBAL_BUFFER(  irOriginCnt,        irOrigin             ),  \
        MAKE_GLOBAL_BUFFER(  irProcCnt,          irProcInfo           ),  \
        MAKE_GLOBAL_BUFFER(  irProcCnt,          irprocToCodepos      ),  \
        MAKE_GLOBAL_BUFFER(  irRegCnt,           irRegInfo            ),  \
        MAKE_GLOBAL_BUFFER(  irReturnvalCnt,     irReturnvalInfo      ),  \
        MAKE_GLOBAL_BUFFER(  irStmtCnt,          irStmtInfo           ),  \
        MAKE_GLOBAL_BUFFER(  irStmtCnt,          irstmtToCodepos      ),  \
        MAKE_GLOBAL_BUFFER(  irSymbolCnt,        irSymbolInfo         ),  \
        MAKE_GLOBAL_BUFFER(  lexbufCnt,          lexbuf               ),  \
        MAKE_GLOBAL_BUFFER(  macroCnt,           macroInfo            ),  \
        MAKE_GLOBAL_BUFFER(  macroParamCnt,      macroParamInfo       ),  \
        MAKE_GLOBAL_BUFFER(  macroBoundArgCnt,   macroBoundArg        ),  \
        MAKE_GLOBAL_BUFFER(  paramCnt,           paramInfo            ),  \
        MAKE_GLOBAL_BUFFER(  procCnt,            procInfo             ),  \
        MAKE_GLOBAL_BUFFER(  procCnt,            procToIrProc         ),  \
        MAKE_GLOBAL_BUFFER(  procCnt,            procToType           ),  \
        MAKE_GLOBAL_BUFFER(  procCnt,            firstDataOfProc      ),  \
        MAKE_GLOBAL_BUFFER(  procCnt,            firstExprOfProc      ),  \
        MAKE_GLOBAL_BUFFER(  relocCnt,           relocInfo            ),  \
        MAKE_GLOBAL_BUFFER(  rodataSectionCnt,   rodataSection        ),  \
        MAKE_GLOBAL_BUFFER(  scopeCnt,           scopeInfo            ),  \
        MAKE_GLOBAL_BUFFER(  stmtCnt,            stmtInfo             ),  \
        MAKE_GLOBAL_BUFFER(  strBucketCnt,       strBucketInfo        ),  \
        MAKE_GLOBAL_BUFFER(  strbufCnt,          strbuf               ),  \
        MAKE_GLOBAL_BUFFER(  stringCnt,          stringInfo           ),  \
        MAKE_GLOBAL_BUFFER(  structmemberCnt,    structmemberInfo     ),  \
        MAKE_GLOBAL_BUFFER(  symbolCnt,          isSymbolExported     ),  \
        MAKE_GLOBAL_BUFFER(  symbolCnt,          symbolInfo           ),  \
        MAKE_GLOBAL_BUFFER(  symDefCnt,          symDefInfo           ),  \
        MAKE_GLOBAL_BUFFER(  symrefCnt,          symrefInfo           ),  \
        MAKE_GLOBAL_BUFFER(  symrefCnt,          symrefToSym          ),  \
        MAKE_GLOBAL_BUFFER(  symrefCnt,          symrefToToken        ),  \
        MAKE_GLOBAL_BUFFER(  tokenCnt ,          tokenInfo            ),  \
        MAKE_GLOBAL_BUFFER(  typeCnt,            typeInfo             ),  \
        MAKE_GLOBAL_BUFFER(  typeCnt,            firstProctypeParam   ),  \
        MAKE_GLOBAL_BUFFER(  x64StackLocCnt,     x64StackLocInfo      ),  \
        MAKE_GLOBAL_BUFFER(  pe64strCnt,         pe64strtab           ),


enum {
#define MAKE_GLOBAL_BUFFER(cnt, ptr) BUFFER_##ptr
        GLOBAL_BUFFERS_X_MACRO
#undef MAKE_GLOBAL_BUFFER
        /* */
        NUM_BUFFERS,
};

struct GlobalBufferInfo {
        void **ptr;
        int elemsize;
        /* for debugging purposes */
        const char *__bufferName;
        int *__bufferCnt;
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
