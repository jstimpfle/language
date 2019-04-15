#include "defs.h"
#define DATA_IMPL
#include "api.h"

const char lvl_debug[] = "DEBUG";
const char lvl_info[] = "INFO";
const char lvl_warn[] = "WARN";
const char lvl_error[] = "ERROR";
const char lvl_fatal[] = "FATAL";
const char lvl_internalerror[] = "INTERNALERROR";

const char *const tokenKindString[NUM_TOKEN_KINDS] = {
        [TOKEN_WORD]           = "TOKEN_WORD",
        [TOKEN_INTEGER]        = "TOKEN_INTEGER",
        [TOKEN_FLOAT]          = "TOKEN_FLOAT",
        [TOKEN_STRING]         = "TOKEN_STRING",
        [TOKEN_CHARACTER]      = "TOKEN_CHARACTER",
        [TOKEN_LEFTPAREN]      = "TOKEN_LEFTPAREN",
        [TOKEN_RIGHTPAREN]     = "TOKEN_RIGHTPAREN",
        [TOKEN_LEFTBRACE]      = "TOKEN_LEFTBRACE",
        [TOKEN_RIGHTBRACE]     = "TOKEN_RIGHTBRACE",
        [TOKEN_LEFTBRACKET]    = "TOKEN_LEFTBRACKET",
        [TOKEN_RIGHTBRACKET]   = "TOKEN_RIGHTBRACKET",
        [TOKEN_DOT]            = "TOKEN_DOT",
        [TOKEN_MINUS]          = "TOKEN_MINUS",
        [TOKEN_PLUS]           = "TOKEN_PLUS",
        [TOKEN_ASTERISK]       = "TOKEN_ASTERISK",
        [TOKEN_SLASH]          = "TOKEN_SLASH",
        [TOKEN_DOUBLEMINUS]    = "TOKEN_DOUBLEMINUS",
        [TOKEN_DOUBLEPLUS]     = "TOKEN_DOUBLEPLUS",
        [TOKEN_COMMA]          = "TOKEN_COMMA",
        [TOKEN_SEMICOLON]      = "TOKEN_SEMICOLON",
        [TOKEN_COLON]          = "TOKEN_COLON",
        [TOKEN_AMPERSAND]      = "TOKEN_AMPERSAND",
        [TOKEN_PIPE]           = "TOKEN_PIPE",
        [TOKEN_CARET]          = "TOKEN_CARET",
        [TOKEN_TILDE]          = "TOKEN_TILDE",
        [TOKEN_BANG]           = "TOKEN_BANG",
        [TOKEN_HASH]           = "TOKEN_HASH",
        [TOKEN_DOLLAR]         = "TOKEN_DOLLAR",
        [TOKEN_BACKSLASH]      = "TOKEN_BACKSLASH",
        [TOKEN_QUESTIONMARK]   = "TOKEN_QUESTIONMARK",
        [TOKEN_ATSIGN]         = "TOKEN_ATSIGN",
        [TOKEN_ASSIGNEQUALS]   = "TOKEN_ASSIGNEQUALS",
        [TOKEN_RIGHTARROW]     = "TOKEN_RIGHTARROW",
        [TOKEN_GT]             = "TOKEN_GT",
        [TOKEN_LT]             = "TOKEN_LT",
        [TOKEN_GE]             = "TOKEN_GE",
        [TOKEN_LE]             = "TOKEN_LE",
        [TOKEN_EQ]             = "TOKEN_EQ",
        [TOKEN_NE]             = "TOKEN_NE",
};

const char *const literalKindString[NUM_LITERAL_KINDS] = {
        [LITERAL_INTEGER]   = "LITERAL_INTEGER",
        [LITERAL_FLOAT]     = "LITERAL_FLOAT",
        [LITERAL_STRING]    = "LITERAL_STRING",
        [LITERAL_CHARACTER] = "LITERAL_CHARACTER",
};

const char *const unopKindString[NUM_UNOP_KINDS] = {
        [UNOP_BITWISENOT]     = "UNOP_BITWISENOT",
        [UNOP_NOT]            = "UNOP_NOT",
        [UNOP_ADDRESSOF]      = "UNOP_ADDRESSOF",
        [UNOP_DEREF]          = "UNOP_DEREF",
        [UNOP_NEGATIVE]       = "UNOP_NEGATIVE",
        [UNOP_POSITIVE]       = "UNOP_POSITIVE",
        [UNOP_PREDECREMENT]   = "UNOP_PREDECREMENT",
        [UNOP_PREINCREMENT]   = "UNOP_PREINCREMENT",
        [UNOP_POSTDECREMENT]  = "UNOP_POSTDECREMENT",
        [UNOP_POSTINCREMENT]  = "UNOP_POSTINCREMENT",
};

const char *const binopKindString[NUM_BINOP_KINDS] = {
        [BINOP_ASSIGN]   = "BINOP_ASSIGN",
        [BINOP_GT]       = "BINOP_GT",
        [BINOP_LT]       = "BINOP_LT",
        [BINOP_GE]       = "BINOP_GE",
        [BINOP_LE]       = "BINOP_LE",
        [BINOP_EQ]       = "BINOP_EQ",
        [BINOP_NE]       = "BINOP_NE",
        [BINOP_MINUS]    = "BINOP_MINUS",
        [BINOP_PLUS]     = "BINOP_PLUS",
        [BINOP_MUL]      = "BINOP_MUL",
        [BINOP_DIV]      = "BINOP_DIV",
        [BINOP_BITAND]   = "BINOP_BITAND",
        [BINOP_BITOR]    = "BINOP_BITOR",
        [BINOP_BITXOR]   = "BINOP_BITXOR",
};

const char *const compilervalueKindString[NUM_COMPILERVALUE_KINDS] = {
        [COMPILERVALUE_FILE]     = "COMPILERVALUE_FILE",
        [COMPILERVALUE_LINE]     = "COMPILERVALUE_LINE",
        [COMPILERVALUE_PROCNAME] = "COMPILERVALUE_PROCNAME",
};

const char *const compilercallKindString[NUM_COMPILERCALL_KINDS] = {
        [COMPILERCALL_STRINGIFY] = "COMPILERVALUE_STRINGIFY",
        [COMPILERCALL_LENGTHOF] = "COMPILERVALUE_LENGTHOF",
        [COMPILERCALL_SIZEOF] = "COMPILERVALUE_SIZEOF",
};

const char *const exprKindString[NUM_EXPR_KINDS] = {
        [EXPR_LITERAL]       = "EXPR_LITERAL",
        [EXPR_SYMREF]        = "EXPR_SYMREF",
        [EXPR_UNOP]          = "EXPR_UNOP",
        [EXPR_BINOP]         = "EXPR_BINOP",
        [EXPR_MEMBER]        = "EXPR_MEMBER",
        [EXPR_SUBSCRIPT]     = "EXPR_SUBSCRIPT",
        [EXPR_CALL]          = "EXPR_CALL",
        [EXPR_COMPOUND]      = "EXPR_COMPOUND",
        [EXPR_COMPILERVALUE] = "EXPR_COMPILERVALUE",
        [EXPR_COMPILERCALL]  = "EXPR_COMPILERCALL",
};

const char *const stmtKindString[NUM_STMT_KINDS] = {
        [STMT_IF]        = "STMT_IF",
        [STMT_IFELSE]    = "STMT_IFELSE",
        [STMT_FOR]       = "STMT_FOR",
        [STMT_WHILE]     = "STMT_WHILE",
        [STMT_RANGE]     = "STMT_RANGE",
        [STMT_RETURN]    = "STMT_RETURN",
        [STMT_EXPR]      = "STMT_EXPR",
        [STMT_COMPOUND]  = "STMT_COMPOUND",
        [STMT_DATA]      = "STMT_DATA",
        [STMT_MACRO]     = "STMT_MACRO",
        [STMT_IGNORE]    = "STMT_IGNORE",
};

const char *const typeKindString[NUM_TYPE_KINDS] = {
        [TYPE_BASE]       = "TYPE_BASE",
        [TYPE_STRUCT]     = "TYPE_STRUCT",
        [TYPE_ARRAY]      = "TYPE_ARRAY",
        [TYPE_POINTER]    = "TYPE_POINTER",
        [TYPE_PROC]       = "TYPE_PROC",
        [TYPE_REFERENCE]  = "TYPE_REFERENCE",
};

const char *const symbolKindString[NUM_SYMBOL_KINDS] = {
        [SYMBOL_TYPE]        = "SYMBOL_TYPE",
        [SYMBOL_DATA]        = "SYMBOL_DATA",
        [SYMBOL_PROC]        = "SYMBOL_PROC",
        [SYMBOL_MACRO]       = "SYMBOL_MACRO",
        [SYMBOL_MACROPARAM]  = "SYMBOL_MACROPARAM",
};

const char *const macroKindString[NUM_MACRO_KINDS] = {
        [MACRO_VALUE]     = "MACRO_VALUE",
        [MACRO_FUNCTION]  = "MACRO_FUNCTION",
};

const char *const constantKindString[NUM_CONSTANT_KINDS] = {
        [CONSTANT_INTEGER]     = "CONSTANT_INTEGER",
        [CONSTANT_EXPRESSION]  = "CONSTANT_EXPRESSION",
};

const char *const valueKindString[NUM_VALUE_KINDS] = {
        [VALUE_INTEGER]  = "VALUE_INTEGER",
        [VALUE_STRING]   = "VALUE_STRING",
};

const char* const irStmtString[NUM_IRSTMT_KINDS] = {
        [IRSTMT_LOADCONSTANT]    = "IRSTMT_LOADCONSTANT",
        [IRSTMT_LOADSYMBOLADDR]  = "IRSTMT_LOADSYMBOLADDR",
        [IRSTMT_LOADREGADDR]     = "IRSTMT_LOADREGADDR",
        [IRSTMT_LOAD]            = "IRSTMT_LOAD",
        [IRSTMT_STORE]           = "IRSTMT_STORE",
        [IRSTMT_REGREG]          = "IRSTMT_REGREG",
        [IRSTMT_OP1]             = "IRSTMT_OP1",
        [IRSTMT_OP2]             = "IRSTMT_OP2",
        [IRSTMT_CMP]             = "IRSTMT_CMP",
        [IRSTMT_CALL]            = "IRSTMT_CALL",
        [IRSTMT_CONDGOTO]        = "IRSTMT_CONDGOTO",
        [IRSTMT_GOTO]            = "IRSTMT_GOTO",
        [IRSTMT_RETURN]          = "IRSTMT_RETURN",
};

const char *const unopString[NUM_UNOP_KINDS] = {
        [UNOP_BITWISENOT]     = "~",
        [UNOP_NOT]            = "!",
        [UNOP_ADDRESSOF]      = "&",
        [UNOP_DEREF]          = "^",
        [UNOP_NEGATIVE]       = "-",
        [UNOP_POSITIVE]       = "+",
        [UNOP_PREDECREMENT]   = "--",
        [UNOP_PREINCREMENT]   = "++",
        [UNOP_POSTDECREMENT]  = "--",
        [UNOP_POSTINCREMENT]  = "++",
};

const char *const binopString[NUM_BINOP_KINDS] = {
        [BINOP_ASSIGN]   = "=",
        [BINOP_GT]       = ">",
        [BINOP_LT]       = "<",
        [BINOP_GE]       = ">=",
        [BINOP_LE]       = "<=",
        [BINOP_EQ]       = "==",
        [BINOP_NE]       = "!=",
        [BINOP_MINUS]    = "-",
        [BINOP_PLUS]     = "+",
        [BINOP_MUL]      = "*",
        [BINOP_DIV]      = "/",
        [BINOP_BITAND]   = "&",
        [BINOP_BITOR]    = "|",
        [BINOP_BITXOR]   = "^",
};

const char *const irOp1String[NUM_IROP1_KINDS] = {
        [IROP1_INC] = "INC",
        [IROP1_DEC] = "DEC",
        [IROP1_NEG] = "NEG",
        [IROP1_BITWISENOT] = "BITWISENOT",
};

const char *const irOp2String[NUM_IROP2_KINDS] = {
        [IROP2_ADD]     = "ADD",
        [IROP2_SUB]     = "SUB",
        [IROP2_MUL]     = "MUL",
        [IROP2_DIV]     = "DIV",
        [IROP2_BITAND]  = "AND",
        [IROP2_BITOR]   = "OR",
        [IROP2_BITXOR]  = "XOR",
};

const char *const irCmpString[NUM_IRCMP_KINDS] = {
        [IRCMP_LT] = "cmp<",
        [IRCMP_GT] = "cmp>",
        [IRCMP_LE] = "cmp<=",
        [IRCMP_GE] = "cmp>=",
        [IRCMP_EQ] = "cmp==",
        [IRCMP_NE] = "cmp!=",
};

const int unopIsPrefix[NUM_UNOP_KINDS] = {
        [UNOP_BITWISENOT]     = 1,
        [UNOP_NOT]            = 1,
        [UNOP_ADDRESSOF]      = 1,
        [UNOP_DEREF]          = 1,
        [UNOP_NEGATIVE]       = 1,
        [UNOP_POSITIVE]       = 1,
        [UNOP_PREDECREMENT]   = 1,
        [UNOP_PREINCREMENT]   = 1,
        [UNOP_POSTDECREMENT]  = 0,
        [UNOP_POSTINCREMENT]  = 0,
};

const int binopPrec[NUM_BINOP_KINDS] = {
        [BINOP_ASSIGN]  = 1,
        [BINOP_GT]      = 2,
        [BINOP_LT]      = 2,
        [BINOP_GE]      = 2,
        [BINOP_LE]      = 2,
        [BINOP_EQ]      = 2,
        [BINOP_NE]      = 2,
        [BINOP_MINUS]   = 3,
        [BINOP_PLUS]    = 3,
        [BINOP_MUL]     = 4,
        [BINOP_DIV]     = 4,
        [BINOP_BITAND]  = 4,
        [BINOP_BITOR]   = 4,
        [BINOP_BITXOR]  = 4,
};

const struct ToktypeToPrefixUnop toktypeToPrefixUnop[] = {
        { TOKEN_TILDE,       UNOP_BITWISENOT },
        { TOKEN_BANG,        UNOP_NOT },
        { TOKEN_AMPERSAND,   UNOP_ADDRESSOF },
        { TOKEN_MINUS,       UNOP_NEGATIVE },
        { TOKEN_PLUS,        UNOP_POSITIVE },
        { TOKEN_DOUBLEMINUS, UNOP_PREDECREMENT },
        { TOKEN_DOUBLEPLUS,  UNOP_PREINCREMENT },
};

const struct ToktypeToPostfixUnop toktypeToPostfixUnop[] = {
        { TOKEN_CARET,       UNOP_DEREF },
        { TOKEN_DOUBLEMINUS, UNOP_POSTDECREMENT },
        { TOKEN_DOUBLEPLUS,  UNOP_POSTINCREMENT },
};

const struct ToktypeToBinop toktypeToBinop[] = {
        { TOKEN_ASSIGNEQUALS, BINOP_ASSIGN  },
        { TOKEN_GT,           BINOP_GT      },
        { TOKEN_LT,           BINOP_LT      },
        { TOKEN_GE,           BINOP_GE      },
        { TOKEN_LE,           BINOP_LE      },
        { TOKEN_EQ,           BINOP_EQ      },
        { TOKEN_NE,           BINOP_NE      },
        { TOKEN_MINUS,        BINOP_MINUS   },
        { TOKEN_PLUS,         BINOP_PLUS    },
        { TOKEN_ASTERISK,     BINOP_MUL     },
        { TOKEN_SLASH,        BINOP_DIV     },
        { TOKEN_AMPERSAND,    BINOP_BITAND  },
        { TOKEN_PIPE,         BINOP_BITOR   },
        /* caret is our dereference operator now.
         * { TOKEN_CARET,        BINOP_BITXOR  },
         */
};

const char *const constStrToCstring[NUM_CONSTSTR_KINDS] = {
        [CONSTSTR_IF]         = "if",
        [CONSTSTR_ELSE]       = "else",
        [CONSTSTR_WHILE]      = "while",
        [CONSTSTR_FOR]        = "for",
        [CONSTSTR_DO]         = "do",
        [CONSTSTR_RETURN]     = "return",
        [CONSTSTR_BREAK]      = "break",
        [CONSTSTR_PROC]       = "proc",
        [CONSTSTR_MACRO]      = "macro",
        [CONSTSTR_STRUCT]     = "struct",
        [CONSTSTR_DATA]       = "data",
        [CONSTSTR_EXPORT]     = "export",
        [CONSTSTR_IN]         = "in",
        [CONSTSTR_FROM]       = "from",
        [CONSTSTR_TO]         = "to",
        [CONSTSTR_DOWNTO]     = "downto",
        [CONSTSTR_SIZEOF]     = "sizeof",
        [CONSTSTR_LENGTHOF]   = "lengthof",
        [CONSTSTR_IGNORE]     = "ignore",
        [CONSTSTR_EXTERN]     = "extern",
        [CONSTSTR_STRINGIFY]  = "stringify",
        [CONSTSTR_CONSTANT]   = "constant",
        [CONSTSTR_ENUM]       = "enum",
        [CONSTSTR_TYPEALIAS]  = "typealias",
        [CONSTSTR_FILE]       = "file",
        [CONSTSTR_LINE]       = "line",
        [CONSTSTR_PROCNAME]   = "procname",
};

const struct BasetypeToBeInitialized basetypesToBeInitialized[] = {
        { "void",            -42,  &builtinType[BUILTINTYPE_VOID] },
        { "{compound-type}", -42,  &builtinType[BUILTINTYPE_COMPOUND] },
        { "int",               8,  &builtinType[BUILTINTYPE_INT] },
        { "float",             4,  &builtinType[BUILTINTYPE_FLOAT] },
        { "char",              1,  &builtinType[BUILTINTYPE_CHAR] },
};

const struct Lex1 lex1[] = {
        { '(', TOKEN_LEFTPAREN },
        { ')', TOKEN_RIGHTPAREN },
        { '{', TOKEN_LEFTBRACE },
        { '}', TOKEN_RIGHTBRACE },
        { '[', TOKEN_LEFTBRACKET },
        { ']', TOKEN_RIGHTBRACKET },
        { '.', TOKEN_DOT },
        { '*', TOKEN_ASTERISK },
        { '/', TOKEN_SLASH },
        { ',', TOKEN_COMMA },
        { ';', TOKEN_SEMICOLON },
        { ':', TOKEN_COLON },
        { '&', TOKEN_AMPERSAND },
        { '|', TOKEN_PIPE },
        { '^', TOKEN_CARET },
        { '~', TOKEN_TILDE },
        { '#', TOKEN_HASH },
        { '$', TOKEN_DOLLAR },
        { '\\', TOKEN_BACKSLASH },
        { '?', TOKEN_QUESTIONMARK },
        { '@', TOKEN_ATSIGN },
};

const struct Lex2 lex2[] = {
        { '+', '+', TOKEN_PLUS, TOKEN_DOUBLEPLUS },
        { '-', '-', TOKEN_MINUS, TOKEN_DOUBLEMINUS },
        { '!', '=', TOKEN_BANG, TOKEN_NE },
        { '>', '=', TOKEN_GT, TOKEN_GE },
        { '<', '=', TOKEN_LT, TOKEN_LE, },
};

const struct Lex3 lex3[] = {
        { '=', '=', '>', TOKEN_ASSIGNEQUALS, TOKEN_EQ, TOKEN_RIGHTARROW },
};

const int toktypeToPrefixUnopCnt = LENGTH(toktypeToPrefixUnop);
const int toktypeToPostfixUnopCnt = LENGTH(toktypeToPostfixUnop);
const int toktypeToBinopCnt = LENGTH(toktypeToBinop);
const int basetypesToBeInitializedCnt = LENGTH(basetypesToBeInitialized);
const int lex1Cnt = LENGTH(lex1);
const int lex2Cnt = LENGTH(lex2);
const int lex3Cnt = LENGTH(lex3);

const struct GlobalBufferInfo globalBufferInfo[NUM_BUFFERS] = {
#define MAKE_GLOBAL_BUFFER(cnt, b) [BUFFER_##b] = { (void **) &b, sizeof *b, #b, &cnt }
        GLOBAL_BUFFERS_X_MACRO
#undef MAKE_GLOBAL_BUFFER
};
