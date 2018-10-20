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
        { TOKTYPE_ASSIGNEQUALS, BINOP_ASSIGN, 0 },
        { TOKTYPE_DOUBLEEQUALS, BINOP_EQUALS, 1 },
        { TOKTYPE_MINUS,        BINOP_MINUS,  2 },
        { TOKTYPE_PLUS,         BINOP_PLUS,   2 },
        { TOKTYPE_ASTERISK,     BINOP_MUL,    3 },
        { TOKTYPE_SLASH,        BINOP_DIV,    3 },
        { TOKTYPE_AMPERSAND,    BINOP_BITAND, 3 },
        { TOKTYPE_PIPE,         BINOP_BITOR,  3 },
        { TOKTYPE_CARET,        BINOP_BITXOR, 3 },
};

const struct UnopInfo unopInfo[NUM_UNOPS] = {
#define MAKE(x, y, z) [x] = { y, z }
        MAKE(UNOP_NOT,           1, "!"),
        MAKE(UNOP_ADDRESSOF,     1, "&"),
        MAKE(UNOP_DEREF,         1, "*"),
        MAKE(UNOP_NEGATIVE,      1, "-"),
        MAKE(UNOP_POSITIVE,      1, "+"),
        MAKE(UNOP_PREDECREMENT,  1, "--"),
        MAKE(UNOP_PREINCREMENT,  1, "++"),
        MAKE(UNOP_POSTDECREMENT, 0, "--"),
        MAKE(UNOP_POSTINCREMENT, 0, "++"),
#undef MAKE
};

const struct BinopInfo binopInfo[NUM_BINOPS] = {
#define MAKE(x, y) [x] = { y }
        MAKE(BINOP_ASSIGN, "="),
        MAKE(BINOP_EQUALS, "=="),
        MAKE(BINOP_MINUS, "-"),
        MAKE(BINOP_PLUS, "+"),
        MAKE(BINOP_MUL, "*"),
        MAKE(BINOP_DIV, "/"),
        MAKE(BINOP_BITAND, "&"),
        MAKE(BINOP_BITOR, "|"),
        MAKE(BINOP_BITXOR, "^"),
#undef MAKE
};

const int toktypeToPrefixUnopCnt = LENGTH(toktypeToPrefixUnop);
const int toktypeToPostfixUnopCnt = LENGTH(toktypeToPostfixUnop);
const int toktypeToBinopCnt = LENGTH(toktypeToBinop);


String constStr[NUM_CONSTSTRS];  // has initializer


File current_file;
int current_offset;
int have_saved_char;
int saved_char;

int have_saved_token;
Token saved_token;

Scope global_scope;
Scope current_scope;
Scope scope_stack[16];
int scope_stack_count;

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
int procArgCnt;
int unopExprCnt;
int binopExprCnt;
int callExprCnt;
int exprCnt;
int compoundStmtCnt;
int exprStmtCnt;
int ifStmtCnt;
int forStmtCnt;
int whileStmtCnt;
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
struct ProcArgInfo *procArgInfo;
struct UnopExprInfo *unopExprInfo;
struct BinopExprInfo *binopExprInfo;
struct CallExprInfo *callExprInfo;
struct ExprInfo *exprInfo;
struct CompoundStmtInfo *compoundStmtInfo;
struct ExprStmtInfo *exprStmtInfo;
struct IfStmtInfo *ifStmtInfo;
struct ForStmtInfo *forStmtInfo;
struct WhileStmtInfo *whileStmtInfo;
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
struct Alloc procArgInfoAlloc;
struct Alloc unopExprInfoAlloc;
struct Alloc binopExprInfoAlloc;
struct Alloc callExprInfoAlloc;
struct Alloc exprInfoAlloc;
struct Alloc compoundStmtInfoAlloc;
struct Alloc exprStmtInfoAlloc;
struct Alloc ifStmtInfoAlloc;
struct Alloc forStmtInfoAlloc;
struct Alloc whileStmtInfoAlloc;
struct Alloc stmtInfoAlloc;
struct Alloc childStmtInfoAlloc;
