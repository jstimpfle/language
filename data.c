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

String constStr[NUM_CONSTSTRS];  // has initializer

File current_file;
int current_offset;
int have_saved_char;
int saved_char;

int have_saved_token;
Token saved_token;

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
int procCnt;
int unopExprCnt;
int binopExprCnt;
int callExprCnt;
int exprCnt;

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
struct ProcInfo *procInfo;
struct UnopExprInfo *unopExprInfo;
struct BinopExprInfo *binopExprInfo;
struct CallExprInfo *callExprInfo;
struct ExprInfo *exprInfo;

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
struct Alloc procInfoAlloc;
struct Alloc unopExprInfoAlloc;
struct Alloc binopExprInfoAlloc;
struct Alloc callExprInfoAlloc;
struct Alloc exprInfoAlloc;
