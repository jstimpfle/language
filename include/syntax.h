#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif


/**
 * \typedef{Symbol}: A symbol is a name for a syntactical entity (such as a
 * type, a data item or a proc) that is visible within a particular \ref{Scope}.
 * See also \ref{SymbolKind}.
 *
 * \typedef{Symref}: A symref is a reference (by name) to a Symbol. It's the
 * compilers job to match up references with symbols. This is usually delayed,
 * such that broken reference will be detected only later.
 *
 * \typedef{Scope}: A scope is an abstract container for \ref{Symbol}
 * definitions and \ref{Symref} references. In the parsing phase a tree (or
 * DAG?) of scopes is built up. Symbol lookup is done starting in a particular
 * scope and looking for the symbol recursively in the parent scopes until a
 * match is found. There are different kinds of scopes; see \ref{ScopeKind}.
 */

typedef int Symbol;
typedef int Symref;
typedef int Scope;


/**
 * \enum{ScopeKind}: Scope kinds. For now, there are global and block scopes.
 * The latter exist inside procedures.
 *
 * \enum{SymbolKind}: Symbol kinds. Symbols are names for artifacts that are
 * builtin or defined by the programmer. These artifacts can be types, data, or
 * procedures. A symbol is a name (string) together with a scope (definition
 * namespace), and an artifact kind and index. The kind and index identify the
 * artifact that the symbol names.
 */

enum ScopeKind {
        SCOPE_GLOBAL,
        SCOPE_PROC,
};

enum SymbolKind {
        SYMBOL_TYPE,
        SYMBOL_DATA,
        SYMBOL_ARRAY,
        SYMBOL_PROC,
};


/**
 * A few external symbols that we're currently using in the code generator.
 */

enum {
        EXTSYM_add64,
        EXTSYM_sub64,
        EXTSYM_mul64,
        EXTSYM_div64,
        EXTSYM_gt64,
        EXTSYM_lt64,
        EXTSYM_ge64,
        EXTSYM_le64,
        EXTSYM_eq64,
        EXTSYM_ne64,
        EXTSYM_print64,
        EXTSYM_prints,
        NUM_EXTSYMS,
};

extern const char *const extsymname[NUM_EXTSYMS];
DATA Symbol extsym[NUM_EXTSYMS];


/**
 * \typedef{Data}: The result of parsing a data declaration. See also
 * \ref{DataInfo}.
 *
 * \typedef{Array}: The result of parsing an array definition. See also
 * \ref{ArrayInfo}.
 *
 * \typedef{Proc}: The result of parsing a proc definition. See also
 * \ref{ProcInfo}.
 *
 * \typedef{Param}: The result of parsing a procedure parameter as part of a
 * proc definition. See also \ref{ParamInfo}
 *
 * \typedef{Expr}: The result of parsing an expression, as part of a statement.
 * See also \ref{ExprInfo}.
 *
 * \typedef{Stmt}: The result of parsing a statement, which can be any of the
 * \enum{StmtKind} kinds of statements. See also \ref{StmtInfo}.
 */

typedef int Data;
typedef int Array;
typedef int Proc;
typedef int Param;
typedef int Expr;
typedef int Stmt;


/**
 * \enum{LiteralKind}: Literal value expression kinds (e.g. integer literal or
 * string literal)
 *
 * \enum{ExprKind}: Expression kinds. Expressions are (typically?) contained in
 * statements.
 *
 * \enum{StmtKind}: Statement kinds. Procedure bodies are made up of statements.
 * One kind of statement is the compound statement, which contains an arbitrary
 * number of other child statements in curly braces.
 */

enum LiteralKind {
        LITERAL_INTEGER,
        LITERAL_STRING,
};

enum ExprKind {
        EXPR_LITERAL,
        EXPR_SYMREF,
        EXPR_UNOP,
        EXPR_BINOP,
        EXPR_MEMBER,
        EXPR_SUBSCRIPT,
        EXPR_CALL,
};

enum StmtKind {
        STMT_IF,
        STMT_IFELSE,
        STMT_FOR,
        STMT_WHILE,
        STMT_RETURN,
        STMT_EXPR,
        STMT_COMPOUND,
        STMT_DATA,
        STMT_ARRAY,
};


/**
 * \struct{SymbolInfo}: Represents a name given to one of the various entity
 * that the programmer can define, such as data items, array items, or types.
 * This struct contains the name and scope of the symbol as well as a reference
 * to the named item (by means of a kind tag and union containing an index for
 * each possible kind).
 *
 * \struct{ScopeInfo}: Represents a scope. TODO
 * 
 * \struct{SymrefInfo}: Represents a reference to a symbol that is (hopefully)
 * defined somewhere else in the code. The reference itself can occur at various
 * places in the grammar, such as as expressions or type definitions.
 *
 * \struct{DataInfo}: Result from parsing a `data` declaration. Contains the
 * name, scope, and type of the data item.
 *
 * \struct{ArrayInfo}: Result from parsing an `array` declaration. Contains the
 * name and scope of the data item as well as the type of indices needed and
 * the type of elements contained.
 *
 * \struct{ProcInfo}: Result from parsing a `proc` declaration.
 */

struct DatasymbolInfo {
        Type tp;  // TODO: what kind of type?
        Data optionaldata;
};

struct ProcsymbolInfo {
        Type tp;  // TYPE_PROC, must match with type of optionalproc (if exists)
        Proc optionalproc;  // may be -1 if external linkage
};

struct SymbolInfo {
        String name;
        Scope scope;
        int kind;  // SYMBOL_
        union {
                Type tType;
                struct DatasymbolInfo tData;
                Array tArray;
                struct ProcsymbolInfo tProc;
        };
};

struct ScopeInfo {
        Scope parentScope;
        Symbol firstSymbol; // speed-up
        int numSymbols;
        int kind;
        union {
                struct {
                        Proc proc;
                } tProc;
        };
};

struct SymrefInfo {
        String name;
        Scope refScope;
};

DATA int symbolCnt;
DATA int scopeCnt;
DATA int symrefCnt;

DATA struct SymbolInfo *symbolInfo;
DATA struct ScopeInfo *scopeInfo;
DATA struct SymrefInfo *symrefInfo;
DATA Token *symrefToToken;
DATA Symbol *symrefToSym;

/**
 */

struct DataInfo {
        Scope scope;
        Type tp;
        Symbol sym;  // back-link
};

struct ArrayInfo {
        Scope scope;
        Type tp;
        Symbol sym;  // back-link
};

struct ProcInfo {
        Symbol sym;
        Scope scope;
        int nparams;
        Stmt body;
};


/**
 * Expressions
 */

struct SymrefExprInfo {
        Symref ref;
};

struct LiteralExprInfo {
        int kind;  // LITERAL_
        union {
                Token tok; // integer (TODO: real integer)
                String tString;
        };
};

struct CallExprInfo {
        Expr callee;
        int firstArgIdx;  // speed-up
        int nargs;
};

struct CallArgInfo {
        Expr callExpr;
        Expr argExpr;
        int rank;
};

struct UnopExprInfo {
        int kind;
        Token tok;
        Expr expr;
};

struct BinopExprInfo {
        int kind;
        Token tok;
        Expr expr1;
        Expr expr2;
};

struct MemberExprInfo {
        Expr expr;
        String name;
};

struct SubscriptExprInfo {
        Expr expr1;
        Expr expr2;
};

struct ExprInfo {
        Proc proc;
        int kind;
        union {
                struct SymrefExprInfo tSymref;
                struct LiteralExprInfo tLiteral;
                struct CallExprInfo tCall;
                struct UnopExprInfo tUnop;
                struct BinopExprInfo tBinop;
                struct MemberExprInfo tMember;
                struct SubscriptExprInfo tSubscript;
        };
};

/**
 * Statements
 */

struct CompoundStmtInfo {
        int numStatements;
        int firstChildStmtIdx;
};

struct ExprStmtInfo {
        Expr expr;
};

struct IfStmtInfo {
        Expr condExpr;
        Stmt ifbody;
};

struct IfelseStmtInfo {
        Expr condExpr;
        Stmt ifbody;
        Stmt elsebody;
};

struct ForStmtInfo {
        Stmt initStmt;
        Expr condExpr;
        Stmt stepStmt;
        Stmt forbody;
};

struct WhileStmtInfo {
        Expr condExpr;
        Stmt whilebody;
};

struct ReturnStmtInfo {
        Expr expr;
};

struct StmtInfo {
        int kind;
        union {
                struct CompoundStmtInfo tCompound;
                struct ExprStmtInfo tExpr;
                struct IfStmtInfo tIf;
                struct IfelseStmtInfo tIfelse;
                struct WhileStmtInfo tWhile;
                struct ForStmtInfo tFor;
                struct ReturnStmtInfo tReturn;
                Data tData;
                Array tArray;
        };
};

struct ChildStmtInfo {
        Stmt parent;
        Stmt child;
        int rank;
};

DATA File currentFile;
DATA int currentOffset;

DATA int lexbufCnt;
DATA char *lexbuf;
DATA int tokenCnt;
DATA struct TokenInfo *tokenInfo;

DATA Scope globalScope;
DATA Scope currentScope;
DATA Scope scopeStack[16];
DATA int scopeStackCnt;
DATA Proc currentProc;

DATA int dataCnt;
DATA int arrayCnt;
DATA int procCnt;
DATA int exprCnt;
DATA int callArgCnt;
DATA int stmtCnt;
DATA int childStmtCnt;

DATA struct DataInfo *dataInfo;
DATA struct ArrayInfo *arrayInfo;
DATA struct ProcInfo *procInfo;
DATA Type *procToType;

DATA struct ExprInfo *exprInfo;
DATA struct CallArgInfo *callArgInfo;
DATA struct StmtInfo *stmtInfo;
DATA struct ChildStmtInfo *childStmtInfo;
