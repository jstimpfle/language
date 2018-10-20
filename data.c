#include "defs.h"
#include "api.h"

const char *tokenKindString[] = {
#define MAKE(x) [x] = #x
        MAKE( TOKTYPE_WORD ),
        MAKE( TOKTYPE_LEFTPAREN ),
        MAKE( TOKTYPE_RIGHTPAREN ),
        MAKE( TOKTYPE_LEFTBRACE ),
        MAKE( TOKTYPE_RIGHTBRACE ),
        MAKE( TOKTYPE_LEFTBRACKET ),
        MAKE( TOKTYPE_RIGHTBRACKET ),
        MAKE( TOKTYPE_MINUS ),
        MAKE( TOKTYPE_PLUS ),
        MAKE( TOKTYPE_ASTERISK ),
        MAKE( TOKTYPE_SLASH ),
        MAKE( TOKTYPE_DOUBLEMINUS ),
        MAKE( TOKTYPE_DOUBLEPLUS ),
        MAKE( TOKTYPE_COMMA ),
        MAKE( TOKTYPE_SEMICOLON ),
        MAKE( TOKTYPE_COLON ),
        MAKE( TOKTYPE_AMPERSAND ),
        MAKE( TOKTYPE_PIPE ),
        MAKE( TOKTYPE_CARET ),
        MAKE( TOKTYPE_BANG ),
        MAKE( TOKTYPE_ASSIGNEQUALS ),
        MAKE( TOKTYPE_DOUBLEEQUALS ),
#undef MAKE
};

const struct ToktypeToPrefixUnop toktypeToPrefixUnop[] = {
        { TOKTYPE_BANG,        UNOP_NOT },
        { TOKTYPE_AMPERSAND,   UNOP_ADDRESSOF },
        { TOKTYPE_ASTERISK,    UNOP_DEREF },
        { TOKTYPE_MINUS,       UNOP_NEGATIVE },
        { TOKTYPE_PLUS,        UNOP_POSITIVE },
        { TOKTYPE_DOUBLEMINUS, UNOP_PREDECREMENT },
        { TOKTYPE_DOUBLEPLUS,  UNOP_PREINCREMENT },
};

const struct ToktypeToPostfixUnop toktypeToPostfixUnop[] = {
        { TOKTYPE_DOUBLEMINUS, UNOP_POSTDECREMENT },
        { TOKTYPE_DOUBLEPLUS,  UNOP_POSTINCREMENT },
};

const struct ToktypeToBinop toktypeToBinop[] = {
        { TOKTYPE_ASSIGNEQUALS, BINOP_ASSIGN  },
        { TOKTYPE_DOUBLEEQUALS, BINOP_EQUALS  },
        { TOKTYPE_MINUS,        BINOP_MINUS   },
        { TOKTYPE_PLUS,         BINOP_PLUS    },
        { TOKTYPE_ASTERISK,     BINOP_MUL     },
        { TOKTYPE_SLASH,        BINOP_DIV     },
        { TOKTYPE_AMPERSAND,    BINOP_BITAND  },
        { TOKTYPE_PIPE,         BINOP_BITOR   },
        { TOKTYPE_CARET,        BINOP_BITXOR  },
};

const struct UnopInfo unopInfo[NUM_UNOPS] = {
#define MAKE(x, y, z) [x] = { y, z }
        MAKE( UNOP_NOT,           1, "!"  ),
        MAKE( UNOP_ADDRESSOF,     1, "&"  ),
        MAKE( UNOP_DEREF,         1, "*"  ),
        MAKE( UNOP_NEGATIVE,      1, "-"  ),
        MAKE( UNOP_POSITIVE,      1, "+"  ),
        MAKE( UNOP_PREDECREMENT,  1, "--" ),
        MAKE( UNOP_PREINCREMENT,  1, "++" ),
        MAKE( UNOP_POSTDECREMENT, 0, "--" ),
        MAKE( UNOP_POSTINCREMENT, 0, "++" ),
#undef MAKE
};

const struct BinopInfo binopInfo[NUM_BINOPS] = {
#define MAKE(x, y, z) [x] = { y, z }
        MAKE( BINOP_ASSIGN,  "=",  1 ),
        MAKE( BINOP_EQUALS,  "==", 1 ),
        MAKE( BINOP_MINUS,   "-",  1 ),
        MAKE( BINOP_PLUS,    "+",  1 ),
        MAKE( BINOP_MUL,     "*",  1 ),
        MAKE( BINOP_DIV,     "/",  1 ),
        MAKE( BINOP_BITAND,  "&",  1 ),
        MAKE( BINOP_BITOR,   "|",  1 ),
        MAKE( BINOP_BITXOR,  "^",  1 ),
#undef MAKE
};

const struct StringToBeInterned stringToBeInterned[] = {
#define MAKE(x, y) [x] = { x, y }
        MAKE( CONSTSTR_IF,     "if"     ),
        MAKE( CONSTSTR_WHILE,  "while"  ),
        MAKE( CONSTSTR_FOR,    "for"    ),
        MAKE( CONSTSTR_PROC,   "proc"   ),
        MAKE( CONSTSTR_DATA,   "data"   ),
        MAKE( CONSTSTR_ENTITY, "entity" ),
        MAKE( CONSTSTR_TABLE,  "table"  ),
        MAKE( CONSTSTR_COLUMN, "column" ),
#endif
};

const int toktypeToPrefixUnopCnt = LENGTH(toktypeToPrefixUnop);
const int toktypeToPostfixUnopCnt = LENGTH(toktypeToPostfixUnop);
const int toktypeToBinopCnt = LENGTH(toktypeToBinop);


String constStr[NUM_CONSTSTRS];  // has initializer


File currentFile;
int currentOffset;
int haveSavedChar;
int savedChar;

int haveSavedToken;
Token savedToken;

Scope globalScope;
Scope currentScope;
Scope scopeStack[16];
int scopeStackCnt;

int lexbufCnt;
int strbufCnt;
int stringCnt;
int strBucketCnt;
int fileCnt;
int tokenCnt;
int typeCnt;
int symbolCnt;
int entityCnt;
int tableCnt;
int columnCnt;
int dataCnt;
int scopeCnt;
int procCnt;
int procParamCnt;
int unopExprCnt;
int binopExprCnt;
int callExprCnt;
int exprCnt;
int stmtCnt;
int childStmtCnt;

char *lexbuf;
char *strbuf;
struct StringInfo *stringInfo;
struct StringBucketInfo *strBucketInfo;
struct FileInfo *fileInfo;
struct TokenInfo *tokenInfo;
struct TypeInfo *typeInfo;
struct SymbolInfo *symbolInfo;
struct EntityInfo *entityInfo;
struct TableInfo *tableInfo;
struct ColumnInfo *columnInfo;
struct DataInfo *dataInfo;
struct ScopeInfo *scopeInfo;
struct ProcInfo *procInfo;
struct ProcParamInfo *procParamInfo;
struct UnopExprInfo *unopExprInfo;
struct BinopExprInfo *binopExprInfo;
struct CallExprInfo *callExprInfo;
struct ExprInfo *exprInfo;
struct StmtInfo *stmtInfo;
struct ChildStmtInfo *childStmtInfo;

struct Alloc lexbufAlloc;
struct Alloc strbufAlloc;
struct Alloc stringInfoAlloc;
struct Alloc strBucketInfoAlloc;
struct Alloc fileInfoAlloc;
struct Alloc tokenInfoAlloc;
struct Alloc typeInfoAlloc;
struct Alloc symbolInfoAlloc;
struct Alloc entityInfoAlloc;
struct Alloc tableInfoAlloc;
struct Alloc columnInfoAlloc;
struct Alloc dataInfoAlloc;
struct Alloc scopeInfoAlloc;
struct Alloc procInfoAlloc;
struct Alloc procParamInfoAlloc;
struct Alloc unopExprInfoAlloc;
struct Alloc binopExprInfoAlloc;
struct Alloc callExprInfoAlloc;
struct Alloc exprInfoAlloc;
struct Alloc stmtInfoAlloc;
struct Alloc childStmtInfoAlloc;

