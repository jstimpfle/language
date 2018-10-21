#ifdef DATA_IMPL
#define DATA
#else
#define DATA extern
#endif

typedef int File;
typedef int String;
typedef int Token;
typedef int Type;
typedef int Symbol;
typedef int Symref;
typedef int Typeref;
typedef int Scope;
typedef int Entity;
typedef int Table;
typedef int Column;
typedef int Data;
typedef int Proc;
typedef int ProcParam;
typedef int Expr;
typedef int CompoundStmt;
typedef int ExprStmt;
typedef int IfStmt;
typedef int ForStmt;
typedef int WhileStmt;
typedef int Stmt;

enum {
        TOKTYPE_WORD,
        TOKTYPE_INTEGER,
        TOKTYPE_LEFTPAREN,
        TOKTYPE_RIGHTPAREN,
        TOKTYPE_LEFTBRACE,
        TOKTYPE_RIGHTBRACE,
        TOKTYPE_LEFTBRACKET,
        TOKTYPE_RIGHTBRACKET,
        TOKTYPE_MINUS,
        TOKTYPE_PLUS,
        TOKTYPE_ASTERISK,
        TOKTYPE_SLASH,
        TOKTYPE_DOUBLEMINUS,
        TOKTYPE_DOUBLEPLUS,
        TOKTYPE_COMMA,
        TOKTYPE_SEMICOLON,
        TOKTYPE_COLON,
        TOKTYPE_AMPERSAND,
        TOKTYPE_PIPE,
        TOKTYPE_CARET,
        TOKTYPE_BANG,
        TOKTYPE_ASSIGNEQUALS,
        TOKTYPE_DOUBLEEQUALS,
};

enum {
        CONSTSTR_IF,
        CONSTSTR_WHILE,
        CONSTSTR_FOR,
        CONSTSTR_PROC,
        CONSTSTR_DATA,
        CONSTSTR_ENTITY,
        CONSTSTR_TABLE,
        CONSTSTR_COLUMN,
        NUM_CONSTSTRS,
};

enum {
        STMT_IF,
        STMT_FOR,
        STMT_WHILE,
        STMT_DATA,
        STMT_EXPR,
        STMT_COMPOUND,
};

enum {
        UNOP_NOT,
        UNOP_ADDRESSOF,
        UNOP_DEREF,
        UNOP_NEGATIVE,
        UNOP_POSITIVE,
        UNOP_PREDECREMENT,
        UNOP_PREINCREMENT,
        UNOP_POSTDECREMENT,
        UNOP_POSTINCREMENT,
        NUM_UNOPS,
};

enum {
        BINOP_ASSIGN,
        BINOP_EQUALS,
        BINOP_MINUS,
        BINOP_PLUS,
        BINOP_MUL,
        BINOP_DIV,
        BINOP_BITAND,
        BINOP_BITOR,
        BINOP_BITXOR,
        NUM_BINOPS,
};

enum {
        EXPR_LITERAL,
        EXPR_SYMREF,
        EXPR_UNOP,
        EXPR_BINOP,
        EXPR_CALL,
};

enum {
        SCOPE_GLOBAL,
        SCOPE_PROC,
};

enum {
        SYMBOL_DATA,
        SYMBOL_PROC,
        SYMBOL_LOCALDATA,
};

struct StringToBeInterned {
        int constant;  // CONSTSTR_
        const char *string;
};

struct ToktypeToPrefixUnop {
        int ttype;
        int optype;
};

struct ToktypeToPostfixUnop {
        int ttype;
        int optype;
};

struct ToktypeToBinop {
        int ttype;
        int optype;
};

struct UnopInfo {
        int isprefix;
        char *str;
};

struct BinopInfo {
        int prec;
        char *str;
};

struct Alloc {
        int cap;
};

struct FileInfo {
        unsigned char *buf;
        struct Alloc bufAlloc;
        String filepath;
        int size;
};

struct StringInfo {
        int pos;  // offset in character buffer
        String next;  // next string in chain
};

struct StringBucketInfo {
        int firstString;
};

struct WordTokenInfo {
        String string;
};

struct IntegerTokenInfo {
        long long value;
};

struct StringTokenInfo {
        String value;
};

struct TokenInfo {
        File file;
        int offset;
        int kind;
        union {
                struct WordTokenInfo word;
                struct IntegerTokenInfo integer;
                struct StringTokenInfo string;
        };
};

struct TypeInfo {
        Symbol sym;
        // TODO
};

struct EntityInfo {
        Type tp;
        Symbol sym;
};

struct TableInfo {
        Type tp;
        Symbol sym;
};

struct SymbolInfo {
        int kind;  // SYMBOL_
        String name;
        Scope scope;
};

struct ColumnInfo {
        Table table;
        Type tp;
        Symbol sym;
};

struct DataInfo {
        Scope scope;
        Type tp;
        Symbol sym;
};

struct ScopeInfo {
        int numSymbols;
        Scope parentScope;
        Symbol firstSymbol; // speed-up
        int kind;
        union {
                struct {
                        Proc proc;
                } tProc;
        };
};

struct ProcInfo {
        Type tp;
        int nparams;
        Symbol sym;
        Scope scope;
        Stmt body;
        ProcParam firstParam;  // speed-up
};

struct ProcParamInfo {
        Proc proc;
        int argIdx;
        Type tp;
        String sym;
};

struct SymrefInfo {
        String name;
        Scope refScope;
        Token tok;
        Symbol sym;  // only valid after symbol resolution
};

struct LiteralExprInfo {
        Token tok;
};

struct CallExprInfo {
        Expr callee;
        Expr firstArgIdx;  // speed-up
        int nargs;
};

struct CallArgInfo {
        Expr callExpr;
        Expr argExpr;
        int rank;
};

struct UnopExprInfo {
        int kind;
        Expr expr;
};

struct BinopExprInfo {
        int kind;
        Expr expr1;
        Expr expr2;
};

struct ExprInfo {
        int kind;
        union {
                Symref tSymref;
                struct LiteralExprInfo tLiteral;
                struct CallExprInfo tCall;
                struct UnopExprInfo tUnop;
                struct BinopExprInfo tBinop;
        };
};

struct DataStmtInfo {
        Data data;
};

struct CompoundStmtInfo {
        int numStatements;
        int firstChildStmtIdx;
};

struct ExprStmtInfo {
        Expr expr;
};

struct IfStmtInfo {
        Expr condExpr;
        Stmt childStmt;
};

struct ForStmtInfo {
        Stmt initStmt;
        Expr condExpr;
        Stmt stepStmt;
        Stmt childStmt;
};

struct WhileStmtInfo {
        Expr condExpr;
        Stmt childStmt;
};

struct StmtInfo {
        int kind;
        union {
                struct DataStmtInfo tData;
                struct CompoundStmtInfo tCompound;
                struct ExprStmtInfo tExpr;
                struct IfStmtInfo tIf;
                struct WhileStmtInfo tWhile;
                struct ForStmtInfo tFor;
        };
};

struct ChildStmtInfo {
        Stmt parent;
        Stmt child;
        int rank;
};

extern const char *tokenKindString[];
extern const struct ToktypeToPrefixUnop toktypeToPrefixUnop[];
extern const struct ToktypeToPostfixUnop toktypeToPostfixUnop[];
extern const struct ToktypeToBinop toktypeToBinop[];
extern const struct UnopInfo unopInfo[NUM_UNOPS];
extern const struct BinopInfo binopInfo[NUM_BINOPS];
extern const struct StringToBeInterned stringToBeInterned[NUM_CONSTSTRS];
extern const int toktypeToPrefixUnopCnt;
extern const int toktypeToPostfixUnopCnt;
extern const int toktypeToBinopCnt;
DATA String constStr[NUM_CONSTSTRS];  // has initializer


/**/

DATA File currentFile;
DATA int currentOffset;
DATA int haveSavedChar;
DATA int savedChar;

DATA int haveSavedToken;
DATA Token savedToken;

DATA Scope globalScope;
DATA Scope currentScope;
DATA Scope scopeStack[16];
DATA int scopeStackCnt;

DATA int lexbufCnt;
DATA int strbufCnt;
DATA int stringCnt;
DATA int strBucketCnt;
DATA int fileCnt;
DATA int tokenCnt;
DATA int typeCnt;
DATA int symbolCnt;
DATA int entityCnt;
DATA int tableCnt;
DATA int columnCnt;
DATA int dataCnt;
DATA int scopeCnt;
DATA int procCnt;
DATA int procParamCnt;
DATA int symrefCnt;
DATA int exprCnt;
DATA int stmtCnt;
DATA int childStmtCnt;
DATA int callArgCnt;

DATA char *lexbuf;
DATA char *strbuf;
DATA struct StringInfo *stringInfo;
DATA struct StringBucketInfo *strBucketInfo;
DATA struct FileInfo *fileInfo;
DATA struct TokenInfo *tokenInfo;
DATA struct TypeInfo *typeInfo;
DATA struct SymbolInfo *symbolInfo;
DATA struct EntityInfo *entityInfo;
DATA struct TableInfo *tableInfo;
DATA struct ColumnInfo *columnInfo;
DATA struct DataInfo *dataInfo;
DATA struct ScopeInfo *scopeInfo;
DATA struct ProcInfo *procInfo;
DATA struct ProcParamInfo *procParamInfo;
DATA struct SymrefInfo *symrefInfo;
DATA struct ExprInfo *exprInfo;
DATA struct StmtInfo *stmtInfo;
DATA struct ChildStmtInfo *childStmtInfo;
DATA struct CallArgInfo *callArgInfo;

DATA struct Alloc lexbufAlloc;
DATA struct Alloc strbufAlloc;
DATA struct Alloc stringInfoAlloc;
DATA struct Alloc strBucketInfoAlloc;
DATA struct Alloc fileInfoAlloc;
DATA struct Alloc tokenInfoAlloc;
DATA struct Alloc typeInfoAlloc;
DATA struct Alloc symbolInfoAlloc;
DATA struct Alloc entityInfoAlloc;
DATA struct Alloc tableInfoAlloc;
DATA struct Alloc columnInfoAlloc;
DATA struct Alloc dataInfoAlloc;
DATA struct Alloc scopeInfoAlloc;
DATA struct Alloc procInfoAlloc;
DATA struct Alloc procParamInfoAlloc;
DATA struct Alloc symrefInfoAlloc;
DATA struct Alloc exprInfoAlloc;
DATA struct Alloc stmtInfoAlloc;
DATA struct Alloc childStmtInfoAlloc;
DATA struct Alloc callArgInfoAlloc;

static inline const char *string_buffer(String s)
{
        return &strbuf[stringInfo[s].pos];
}

static inline int string_length(String s)
{
        return stringInfo[s+1].pos - stringInfo[s].pos - 1;
}

static inline const char *SS(Symbol sym)
{
        return string_buffer(symbolInfo[sym].name);
}

static inline const char *TS(Token tok)
{
        return string_buffer(tokenInfo[tok].word.string);
}

static inline const char *TKS(Token tok)
{
        return tokenKindString[tokenInfo[tok].kind];
}

String lookup_string_with_hash(const void *buf, int len, unsigned hsh);
String intern_string(const void *buf, int len);
String intern_cstring(const char *str);


void read_whole_file(File file);

void mem_fill(void *ptr, int val, int size);
void mem_copy(void *dst, const void *src, int size);
int mem_compare(const void *m1, const void *m2, int size);
int cstr_length(const char *s);

void *mem_realloc(void *ptr, int size);

void sort_array(void *ptr, int nelems, int elemsize,
                int (*compare)(const void*, const void*));

void NORETURN _fatal(const char *filename, int line, const char *msg, ...);

#define FATAL(msg, ...) _fatal(__FILE__, __LINE__, msg, ##__VA_ARGS__)

void msg(const char *fmt, ...);

void _buf_init(void **ptr, struct Alloc *alloc, int elsize,
               const char *UNUSED file, int UNUSED line);

void _buf_exit(void **ptr, struct Alloc *alloc, int elsize,
               const char *UNUSED file, int UNUSED line);

void _buf_reserve(void **ptr, struct Alloc *alloc, int nelems, int elsize,
                  int clear, const char *UNUSED file, int UNUSED line);

#define BUF_INIT(buf, alloc) \
        _buf_init((void**)&(buf), &(alloc), sizeof *(buf), __FILE__, __LINE__);

#define BUF_EXIT(buf, alloc) \
        _buf_exit((void**)&(buf), &(alloc), sizeof *(buf), __FILE__, __LINE__);

#define BUF_RESERVE(buf, alloc, cnt) \
        _buf_reserve((void**)&(buf), &(alloc), (cnt), sizeof *(buf), 0, \
                     __FILE__, __LINE__);

#define BUF_RESERVE_Z(buf, alloc, cnt) \
        _buf_reserve((void**)&(buf), &(alloc), (cnt), sizeof *(buf), 1, \
                     __FILE__, __LINE__);

#define BUF_APPEND(buf, alloc, cnt, el) \
        do { \
                void *ptr = (buf); \
                int _appendpos = (cnt)++; \
                _buf_reserve(&ptr, &(alloc), _appendpos+1, sizeof *(buf), 0, \
                             __FILE__, __LINE__); \
                (buf)[_appendpos] = el; \
        } while (0)
