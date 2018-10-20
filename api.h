typedef int File;
typedef int String;
typedef int Token;
typedef int Type;
typedef int Symbol;
typedef int Symref;
typedef int Scope;
typedef int Entity;
typedef int Table;
typedef int Column;
typedef int Data;
typedef int Proc;
typedef int ProcParam;
typedef int SymrefExpr;
typedef int CallExpr;
typedef int UnopExpr;
typedef int BinopExpr;
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
        String name;
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
        int kind;
        union {
                struct {
                        Proc proc;
                        Scope parentScope;
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

struct SymrefExprInfo {
        String name;
        Token tok;
};

struct LiteralExprInfo {
        Token tok;
};

struct CallExprInfo {
        Expr callee;
        Expr firstArgIndex;  // speed-up
        Expr lastArgIndex;  // speed-up
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
                struct SymrefExprInfo tSymref;
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
extern String constStr[NUM_CONSTSTRS];  // has initializer


/**/

extern File currentFile;
extern int currentOffset;
extern int haveSavedChar;
extern int savedChar;

extern int haveSavedToken;
extern Token savedToken;

extern Scope globalScope;
extern Scope currentScope;
extern Scope scopeStack[16];
extern int scopeStackCnt;

extern int lexbufCnt;
extern int strbufCnt;
extern int stringCnt;
extern int strBucketCnt;
extern int fileCnt;
extern int tokenCnt;
extern int typeCnt;
extern int symbolCnt;
extern int entityCnt;
extern int tableCnt;
extern int columnCnt;
extern int dataCnt;
extern int scopeCnt;
extern int procCnt;
extern int procParamCnt;
extern int unopExprCnt;
extern int binopExprCnt;
extern int callExprCnt;
extern int exprCnt;
extern int stmtCnt;
extern int childStmtCnt;
extern int callArgCnt;

extern char *lexbuf;
extern char *strbuf;
extern struct StringInfo *stringInfo;
extern struct StringBucketInfo *strBucketInfo;
extern struct FileInfo *fileInfo;
extern struct TokenInfo *tokenInfo;
extern struct TypeInfo *typeInfo;
extern struct SymbolInfo *symbolInfo;
extern struct EntityInfo *entityInfo;
extern struct TableInfo *tableInfo;
extern struct ColumnInfo *columnInfo;
extern struct DataInfo *dataInfo;
extern struct ScopeInfo *scopeInfo;
extern struct ProcInfo *procInfo;
extern struct ProcParamInfo *procParamInfo;
extern struct UnopExprInfo *unopExprInfo;
extern struct BinopExprInfo *binopExprInfo;
extern struct CallExprInfo *callExprInfo;
extern struct ExprInfo *exprInfo;
extern struct StmtInfo *stmtInfo;
extern struct ChildStmtInfo *childStmtInfo;
extern struct CallArgInfo *callArgInfo;

extern struct Alloc lexbufAlloc;
extern struct Alloc strbufAlloc;
extern struct Alloc stringInfoAlloc;
extern struct Alloc strBucketInfoAlloc;
extern struct Alloc fileInfoAlloc;
extern struct Alloc tokenInfoAlloc;
extern struct Alloc typeInfoAlloc;
extern struct Alloc symbolInfoAlloc;
extern struct Alloc entityInfoAlloc;
extern struct Alloc tableInfoAlloc;
extern struct Alloc columnInfoAlloc;
extern struct Alloc dataInfoAlloc;
extern struct Alloc scopeInfoAlloc;
extern struct Alloc procInfoAlloc;
extern struct Alloc procParamInfoAlloc;
extern struct Alloc unopExprInfoAlloc;
extern struct Alloc binopExprInfoAlloc;
extern struct Alloc callExprInfoAlloc;
extern struct Alloc exprInfoAlloc;
extern struct Alloc stmtInfoAlloc;
extern struct Alloc childStmtInfoAlloc;
extern struct Alloc callArgInfoAlloc;

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
