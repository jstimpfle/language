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

const char *tokenKindString[];
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

