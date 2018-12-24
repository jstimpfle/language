#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif


/**
 * \typedef{Symbol}: A symbol is a name for a syntactical entity (such as a
 * type, a data item or a proc) that is visible within a particular \ref{Scope}.
 * See also \ref{SymbolKind}.
 *
 * \typedef{Symref}: A symref is a reference (by name) to a Symbol. It's the
 * compiler's job to match up references with symbols. This is usually delayed,
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
        SCOPE_MACRO,  /* TODO: XXX: I haven't really thought about this. Does it
                         make sense to have a SCOPE_MACRO? */
};

enum SymbolKind {
        SYMBOL_TYPE,
        SYMBOL_DATA,
        SYMBOL_ARRAY,
        SYMBOL_PROC,
        SYMBOL_MACRO,
        SYMBOL_MACROPARAM,
};


/**
 * A few external symbols that we're currently using in the code generator.
 */

enum {
        EXTSYM_print64,
        EXTSYM_prints,
        NUM_EXTSYMS,
};

extern const char *const extsymname[NUM_EXTSYMS];
DATA Symbol extsymToSymbol[NUM_EXTSYMS];

/**
 * \typedef{Expr}: Expression (part of a statement). Expressions have a type and
 * a (runtime) value. Each expressions is of one of the \enum{ExprKind} kinds.
 * See also \ref{ExprInfo}.
 *
 * \typedef{Stmt}: Statement, which can be any of the \enum{StmtKind} kinds of
 * statements. See also \ref{StmtInfo}.
 */

typedef int Expr;
typedef int Stmt;

/**
 * Top-level syntactic elements:
 *
 * \typedef{Data}: Data declaration. See also \ref{DataInfo}.
 *
 * \typedef{Array}: Array definition. See also \ref{ArrayInfo}.
 *
 * \typedef{Proc}: Procedure definition. See also \ref{ProcInfo}.
 *
 * \typedef{Macro}: Simple expression replacement
 * \typedef{MacroParam}: Formal macro parameter
 *
 * \typedef{Export}: Statement that some definition should be "exported"
 */

typedef int Data;
typedef int Array;
typedef int Proc;
typedef int Macro;
typedef int MacroParam;
typedef int Export;


/**
 * \struct{SymbolInfo}: Contains the name and scope of a symbol as well as a
 * reference to the named item (by means of a kind tag and union containing an
 * index for each possible kind).
 *
 * \struct{ScopeInfo}: Represents a scope. TODO
 *
 * \struct{SymrefInfo}: Represents a reference to a symbol that is (hopefully)
 * defined somewhere else in the code. The reference itself can occur at various
 * places in the grammar, such as as expressions or type definitions.
 *
 * \struct{DataInfo}: Contains the name, scope, and type of a data declaration.
 *
 * \struct{ArrayInfo}: Contains the name and scope of a data declaration as well
 * as the type of indices needed and the type of elements contained.
 *
 * \struct{ProcInfo}: Result from parsing a `proc` declaration.
 */

struct DatasymbolInfo {
        Type tp;  // TODO: what kind of type?
        Data optionaldata;
};

struct ProcsymbolInfo {
        Type tp;  /* Type of kind TYPE_PROC. must be compatible with type of
                     optionalproc (if exists) */
        Proc optionalproc;  // may be -1 if external linkage
};

struct SymbolInfo {
        String name;
        Scope scope;
        int symbolKind;  // SYMBOL_
        union {
                Type tType;
                struct DatasymbolInfo tData;
                Array tArray;
                struct ProcsymbolInfo tProc;
                Macro tMacro;
                /* There is no case for SYMBOL_MACROPARAM. Arguments for formal
                 * macro parameters are expressions that can be looked up from a
                 * "current macro substitutions context". The important thing is
                 * that there is not static resolution! */
        };
};

struct ScopeInfo {
        Scope parentScope;
        Symbol firstSymbol; // speed-up
        int numSymbols;
        int scopeKind;
        union {
                Proc tProc;
                Macro tMacro;
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
DATA unsigned char *isSymbolExported;

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
        NUM_EXPR_KINDS,
};

enum StmtKind {
        STMT_IF,
        STMT_IFELSE,
        STMT_FOR,
        STMT_WHILE,
        STMT_RANGE,
        STMT_RETURN,
        STMT_EXPR,
        STMT_COMPOUND,
        STMT_DATA,
        STMT_ARRAY,
        STMT_MACRO,
        NUM_STMT_KINDS,
};


/**
 * Expressions
 */

struct SymrefExprInfo {
        Symref ref;
};

struct LiteralExprInfo {
        int literalKind;  // LITERAL_
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
};

struct UnopExprInfo {
        int unopKind;
        Token tok;
        Expr expr;
};

struct BinopExprInfo {
        int binopKind;
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
        int exprKind;
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

struct RangeStmtInfo {
        Data variable;
        Expr startExpr;
        Expr stopExpr;
        int directionIsDown;
        Stmt rangebody;
};

struct ReturnStmtInfo {
        Expr expr;
};

struct StmtInfo {
        int stmtKind;
        union {
                struct CompoundStmtInfo tCompound;
                struct ExprStmtInfo tExpr;
                struct IfStmtInfo tIf;
                struct IfelseStmtInfo tIfelse;
                struct WhileStmtInfo tWhile;
                struct ForStmtInfo tFor;
                struct RangeStmtInfo tRange;
                struct ReturnStmtInfo tReturn;
                Data tData;
                Array tArray;
                Macro tMacro;
        };
};

struct ChildStmtInfo {
        Stmt parent;
        Stmt child;
};

/**
 * Top-level entities
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

struct MacroParam {
        Macro macro;
        Token token;  // TOKEN_WORD token holding the parameter name
};

struct MacroInfo {
        Symbol symbol;
        Scope scope;
        Expr expr;
};

struct ExportInfo {
        Symref ref;
};

/*
 * Data
 */

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

DATA int exprCnt;
DATA int callArgCnt;
DATA int stmtCnt;
DATA int childStmtCnt;

DATA int dataCnt;
DATA int arrayCnt;
DATA int procCnt;
DATA int macroCnt;
DATA int macroParamCnt;
DATA int exportCnt;

DATA struct ExprInfo *exprInfo;
DATA struct CallArgInfo *callArgInfo;
DATA struct StmtInfo *stmtInfo;
DATA struct ChildStmtInfo *childStmtInfo;

DATA struct DataInfo *dataInfo;
DATA struct ArrayInfo *arrayInfo;
DATA struct ProcInfo *procInfo;
DATA Type *procToType;
DATA Data *firstDataOfProc;
DATA Expr *firstExprOfProc;
DATA struct MacroInfo *macroInfo;
DATA struct MacroParam *macroParam;
DATA struct ExportInfo *exportInfo;
