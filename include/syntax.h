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
        SYMBOL_PROC,
        SYMBOL_MACRO,
        SYMBOL_CONSTANT,
        SYMBOL_MACROPARAM,
        NUM_SYMBOL_KINDS,
};


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
 * Top-level Program elements:
 *
 * \typedef{Data}: Data value. See also \ref{DataInfo}.
 *
 * \typedef{Proc}: Procedure. See also \ref{ProcInfo}.
 *
 * \typedef{Macro}: Simple expression replacement
 * \typedef{MacroParam}: Formal macro parameter
 *
 * \typedef{Constant}: Compile-time constant value (defined using an Expr)
 *
 * \typedef{Export}: Statement that some definition should be "exported" (TODO:
 * not a program element. Move to another place)
 */

typedef int Data;
typedef int Proc;
typedef int Macro;
typedef int MacroParam;
typedef int Constant;
typedef int Export;


/*
 * \enum{MacroKind}: Macro kinds. Currently we have function macros and value
 * macros.

 * \enum{ConstantKind}: Constant kinds. Currently we have integer and expression
 * constants.  Expression constants are declared by "constant" directives in the
 * source file. Integer constants are defined by "enum" directives.
 *
 * \enum{ValueKind}: Value Kinds. These values are used for the representation
 * of constants.  CONSTANT_INTEGER constants have a value of kind VALUE_INTEGER.
 * CONSTANT_EXPRESSION constants have a value of a kind that depends on the type
 * of the expression (typechecking needed).
 */

enum MacroKind {
        MACRO_VALUE,
        MACRO_FUNCTION,
        NUM_MACRO_KINDS,
};

enum ConstantKind {
        CONSTANT_INTEGER,
        CONSTANT_EXPRESSION,
        NUM_CONSTANT_KINDS,
};

enum ValueKind {
        VALUE_INTEGER,
        VALUE_STRING,
        NUM_VALUE_KINDS
};

const char *const macroKindString[NUM_MACRO_KINDS];
const char *const constantKindString[NUM_CONSTANT_KINDS];
const char *const valueKindString[NUM_VALUE_KINDS];


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
                struct ProcsymbolInfo tProc;
                Macro tMacro;
                MacroParam tMacroParam;
                Constant tConstant;
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
        NUM_LITERAL_KINDS,
};

enum ExprKind {
        EXPR_LITERAL,
        EXPR_SYMREF,
        EXPR_UNOP,
        EXPR_BINOP,
        EXPR_MEMBER,
        EXPR_SUBSCRIPT,
        EXPR_CALL,
        EXPR_COMPOUND,
        EXPR_SIZEOF,
        EXPR_LENGTHOF,
        EXPR_STRINGIFY,
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
        STMT_IGNORE,
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
        Token tok; // XXX: currently also used for LITERAL_INTEGER
        union {
                String tString;
        };
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

struct CallExprInfo {
        Expr callee;
        int firstArgIdx;  // speed-up
        int nargs;
};

struct CompoundExprInfo {
        Token initialToken;  // first token in expression. As of writing this, that's a left-brace.
        int firstCompoundExprLink;
        int numChilds;
};

struct SizeofExprInfo {
        Token tok;
        Expr expr;
};

struct LengthofExprInfo {
        Token tok;
        Expr expr;
};

struct StringifyExprInfo {
        Token tok;
        Expr expr;
};

struct ExprInfo {
        Proc proc;
        int exprKind;
        union {
                struct SymrefExprInfo tSymref;
                struct LiteralExprInfo tLiteral;
                struct UnopExprInfo tUnop;
                struct BinopExprInfo tBinop;
                struct MemberExprInfo tMember;
                struct SubscriptExprInfo tSubscript;
                struct CallExprInfo tCall;
                struct CompoundExprInfo tCompound;
                struct SizeofExprInfo tSizeof;
                struct LengthofExprInfo tLengthof;
                struct StringifyExprInfo tStringify;
        };
};

struct CompoundExprLink {
        Expr parentExpr;
        Expr childExpr;
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

struct DataStmtInfo {
        Data data;
        Expr optionalInitializerExpr;
};

struct ArrayStmtInfo {
        Data data;
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
                struct DataStmtInfo tData;
                struct ArrayStmtInfo tArray;
                Macro tMacro;
                Stmt tIgnore;
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

struct ProcInfo {
        Symbol sym;
        Scope scope; // same scope as symbol's scope. Is this too redundant?
        Stmt body;
};

struct MacroParamInfo {  // parameter for MACRO_FUNCTION macro
        Macro macro;
        String name;
};

struct FunctionMacroInfo {
        // speed-up indices. Note that the params of a macro are contiguous
        // in memory due to the way they are parsed.
        MacroParam firstMacroParam;
        int nparams;
};

struct MacroInfo {
        Symbol symbol;
        Scope scope; // same scope as symbol's scope. Is this too redundant?
        Expr expr;
        int macroKind;
        union {
                struct FunctionMacroInfo tFunction;
        };
};

struct ConstantInfo {
        int constantKind;
        Symbol symbol;
        Scope scope; // same scope as symbol's scope. Is this too redundant?
        Expr tExpr;
};

/* type and value of a constant. Calculated after typechecking phase */
struct ConstantValue {
        int valueKind;
        union {
                long long tInteger;  // for now, long long
                String tString;
        };
};

struct ExportInfo {
        Symref ref;
};

/*
 * \typedef{Directive}: Top-level syntactic element. Each file is grammatically
 * a sequence of directives.
 */

typedef int Directive;

enum {
        BUILTINDIRECTIVE_EXTERN,
        BUILTINDIRECTIVE_DATA,
        BUILTINDIRECTIVE_ARRAY,
        BUILTINDIRECTIVE_STRUCT,
        BUILTINDIRECTIVE_PROC,
        BUILTINDIRECTIVE_MACRO,
        BUILTINDIRECTIVE_ENUM,
        BUILTINDIRECTIVE_CONSTANT,
        BUILTINDIRECTIVE_EXPORT,
        NUM_BUILTINDIRECTIVE_KINDS,
};

struct DirectiveKindInfo {
        String keyword;
        void (*parser)(Directive directive);
};

/* initializer for DirectiveKindInfo */
struct BuiltinDirectiveKindInfo {
        int constStrKind;
        void (*parser)(Directive directive);
};

struct ExternDirectiveInfo {
        Symbol symbol;
};

struct DataDirectiveInfo {
        Data data;
        Expr optionalInitializerExpr;
};

struct ArrayDirectiveInfo {
        Data data;
        Expr lengthExpr;
};

struct StructDirectiveInfo {
        Type tp;
};

struct DirectiveInfo {
        int directiveKind;
        union {
                struct ExternDirectiveInfo tExtern;
                struct DataDirectiveInfo tData;
                struct ArrayDirectiveInfo tArray;
                struct StructDirectiveInfo tStruct;
                Proc tProc;
                Macro tMacro;
                Constant tConstant;  // Constant of kind CONSTANT_EXPRESSION
                Export tExport;
                /* hook for future extensions */
                long long tGeneric;
        };
};

/*
 * Data
 */

extern const char *const symbolKindString[NUM_SYMBOL_KINDS];
extern const char *const stmtKindString[NUM_STMT_KINDS];
extern const char *const exprKindString[NUM_EXPR_KINDS];
extern const char *const literalKindString[NUM_LITERAL_KINDS];

/* initializer for directiveKindInfo */
extern const struct BuiltinDirectiveKindInfo builtinDirectiveKindInfo[];
extern const int builtinDirectiveKindCnt;

/* constant after initialization time. But not constants technically */
DATA struct DirectiveKindInfo *directiveKindInfo;
DATA int directiveKindCnt;

DATA File currentFile;
DATA int currentOffset;

DATA Scope globalScope;
DATA Scope currentScope;
DATA Scope scopeStack[16];
DATA int scopeStackCnt;
DATA Proc currentProc;

DATA int exprCnt;
DATA int callArgCnt;
DATA int compoundExprLinkCnt;
DATA int stmtCnt;
DATA int childStmtCnt;

DATA int dataCnt;
DATA int procCnt;
DATA int macroCnt;
DATA int constantCnt;
DATA int macroParamCnt;
DATA int exportCnt;
DATA int directiveCnt;

DATA struct ExprInfo *exprInfo;
DATA struct CallArgInfo *callArgInfo;
DATA struct CompoundExprLink *compoundExprLink;
DATA struct StmtInfo *stmtInfo;
DATA struct ChildStmtInfo *childStmtInfo;

DATA struct DataInfo *dataInfo;
DATA struct ProcInfo *procInfo;
DATA Type *procToType;
DATA Data *firstDataOfProc;
DATA Expr *firstExprOfProc;
DATA struct MacroInfo *macroInfo;
DATA struct MacroParamInfo *macroParamInfo;
DATA struct ConstantInfo *constantInfo;
DATA struct ConstantValue *constantValue;
DATA struct ExportInfo *exportInfo;
DATA struct DirectiveInfo *directiveInfo;
