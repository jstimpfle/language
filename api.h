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
typedef int ProcArg;
typedef int SymrefExpr;
typedef int CallExpr;
typedef int UnopExpr;
typedef int BinopExpr;
typedef int Expr;

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
        STATEMENT_IF,
        STATEMENT_FOR,
        STATEMENT_WHILE,
        STATEMENT_DATA,
        STATEMENT_EXPR,
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
};

enum {
        OP_ASSIGN,
        OP_EQUALS,
        OP_MINUS,
        OP_PLUS,
        OP_MUL,
        OP_DIV,
        OP_BITAND,
        OP_BITOR,
        OP_BITXOR,
};

enum {
        EXPR_UNOP,
        EXPR_BINOP,
        EXPR_CALL,
};

struct Alloc {
        int cap;
};

struct FileInfo {
        String filepath;
        unsigned char *buf;
        struct Alloc bufAlloc;
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

struct ProcInfo {
        Type tp;
        int nargs;
        Symbol sym;
};

struct ProcArgInfo {
        Proc proc;
        int argIdx;
        Type tp;
};

struct SymrefExprInfo {
        Symbol sym;
        Token tok;
};

struct LiteralExprInfo {
        Token tok;
};

struct CallExprInfo {
        Proc proc;
        Expr firstArgIndex;  // speed-up
        Expr lastArgIndex;  // speed-up
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
                struct SymrefExprInfo symref;
                struct CallExprInfo call;
                struct UnopExprInfo unop;
                struct BinopExprInfo binop;
        };
};

extern const char *tokenKindString[];
extern String constStr[NUM_CONSTSTRS];  // has initializer

extern File current_file;
extern int current_offset;
extern int have_saved_char;
extern int saved_char;

extern int have_saved_token;
extern Token saved_token;

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
extern int procCnt;
extern int unopExprCnt;
extern int binopExprCnt;
extern int callExprCnt;
extern int exprCnt;

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
extern struct ProcInfo *procInfo;
extern struct UnopExprInfo *unopExprInfo;
extern struct BinopExprInfo *binopExprInfo;
extern struct CallExprInfo *callExprInfo;
extern struct ExprInfo *exprInfo;

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
extern struct Alloc procInfoAlloc;
extern struct Alloc unopExprInfoAlloc;
extern struct Alloc binopExprInfoAlloc;
extern struct Alloc callExprInfoAlloc;
extern struct Alloc exprInfoAlloc;

static inline const char *string_buffer(String s)
{
        return &strbuf[stringInfo[s].pos];
}

static inline int string_length(String s)
{
        return stringInfo[s+1].pos - stringInfo[s].pos - 1;
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
