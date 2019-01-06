#include "defs.h"
#define DATA_IMPL
#include "api.h"

const char lvl_debug[] = "DEBUG";
const char lvl_info[] = "INFO";
const char lvl_warn[] = "WARN";
const char lvl_error[] = "ERROR";
const char lvl_fatal[] = "FATAL";

const char *const tokenKindString[NUM_TOKEN_KINDS] = {
#define MAKE(x) [x] = #x
        MAKE( TOKEN_WORD ),
        MAKE( TOKEN_INTEGER ),
        MAKE( TOKEN_STRING ),
        MAKE( TOKEN_LEFTPAREN ),
        MAKE( TOKEN_RIGHTPAREN ),
        MAKE( TOKEN_LEFTBRACE ),
        MAKE( TOKEN_RIGHTBRACE ),
        MAKE( TOKEN_LEFTBRACKET ),
        MAKE( TOKEN_RIGHTBRACKET ),
        MAKE( TOKEN_DOT ),
        MAKE( TOKEN_MINUS ),
        MAKE( TOKEN_PLUS ),
        MAKE( TOKEN_ASTERISK ),
        MAKE( TOKEN_SLASH ),
        MAKE( TOKEN_DOUBLEMINUS ),
        MAKE( TOKEN_DOUBLEPLUS ),
        MAKE( TOKEN_COMMA ),
        MAKE( TOKEN_SEMICOLON ),
        MAKE( TOKEN_COLON ),
        MAKE( TOKEN_AMPERSAND ),
        MAKE( TOKEN_PIPE ),
        MAKE( TOKEN_CARET ),
        MAKE( TOKEN_TILDE ),
        MAKE( TOKEN_BANG ),
        MAKE( TOKEN_HASH ),
        MAKE( TOKEN_DOLLAR ),
        MAKE( TOKEN_BACKSLASH ),
        MAKE( TOKEN_QUESTIONMARK ),
        MAKE( TOKEN_ATSIGN ),
        MAKE( TOKEN_ASSIGNEQUALS ),
        MAKE( TOKEN_RIGHTARROW ),
        MAKE( TOKEN_GT ),
        MAKE( TOKEN_LT ),
        MAKE( TOKEN_GE ),
        MAKE( TOKEN_LE ),
        MAKE( TOKEN_EQ ),
        MAKE( TOKEN_NE ),
#undef MAKE
};

const char *const literalKindString[NUM_LITERAL_KINDS] = {
#define MAKE(x) [x] = #x
        MAKE( LITERAL_INTEGER ),
        MAKE( LITERAL_STRING ),
#undef MAKE
};

const char *const exprKindString[NUM_EXPR_KINDS] = {
#define MAKE(x) [x] = #x
        MAKE( EXPR_LITERAL ),
        MAKE( EXPR_SYMREF ),
        MAKE( EXPR_UNOP ),
        MAKE( EXPR_BINOP ),
        MAKE( EXPR_MEMBER ),
        MAKE( EXPR_SUBSCRIPT ),
        MAKE( EXPR_CALL ),
        MAKE( EXPR_SIZEOF ),
        MAKE( EXPR_STRINGIFY ),
#undef MAKE
};

const char *const stmtKindString[NUM_STMT_KINDS] = {
#define MAKE(x) [x] = #x
        MAKE( STMT_IF ),
        MAKE( STMT_IFELSE ),
        MAKE( STMT_FOR ),
        MAKE( STMT_WHILE ),
        MAKE( STMT_RANGE ),
        MAKE( STMT_RETURN ),
        MAKE( STMT_EXPR ),
        MAKE( STMT_COMPOUND ),
        MAKE( STMT_DATA ),
        MAKE( STMT_ARRAY ),
        MAKE( STMT_MACRO ),
        MAKE( STMT_IGNORE ),
#undef MAKE
};

const char *const typeKindString[NUM_TYPE_KINDS] = {
#define MAKE(x) [x] = #x
        MAKE( TYPE_BASE ),
        MAKE( TYPE_STRUCT ),
        MAKE( TYPE_ENTITY ),
        MAKE( TYPE_ARRAY ),
        MAKE( TYPE_POINTER ),
        MAKE( TYPE_PROC ),
        MAKE( TYPE_REFERENCE ),
#undef MAKE
};

const char *const symbolKindString[NUM_SYMBOL_KINDS] = {
#define MAKE(x) [x] = #x
        MAKE( SYMBOL_TYPE ),
        MAKE( SYMBOL_DATA ),
        MAKE( SYMBOL_PROC ),
        MAKE( SYMBOL_MACRO ),
        MAKE( SYMBOL_MACROPARAM ),
#undef MAKE
};

const char *const macroKindString[NUM_MACRO_KINDS] = {
#define MAKE(x) [x] = #x
        MAKE( MACRO_VALUE ),
        MAKE( MACRO_FUNCTION ),
#undef MAKE
};

const char *const constantKindString[NUM_CONSTANT_KINDS] = {
#define MAKE(x) [x] = #x
        MAKE( CONSTANT_INTEGER ),
        MAKE( CONSTANT_STRING ),
#undef MAKE
};

const char* const irStmtString[NUM_IRSTMT_KINDS] = {
#define MAKE(x) [x] = #x
        MAKE( IRSTMT_LOADCONSTANT ),
        MAKE( IRSTMT_LOADSYMBOLADDR ),
        MAKE( IRSTMT_LOADREGADDR ),
        MAKE( IRSTMT_LOAD ),
        MAKE( IRSTMT_STORE ),
        MAKE( IRSTMT_REGREG ),
        MAKE( IRSTMT_OP1 ),
        MAKE( IRSTMT_OP2 ),
        MAKE( IRSTMT_CMP ),
        MAKE( IRSTMT_CALL ),
        MAKE( IRSTMT_CONDGOTO ),
        MAKE( IRSTMT_GOTO ),
        MAKE( IRSTMT_RETURN ),
#undef MAKE
};

const char *const irOp1String[NUM_IROP1_KINDS] = {
#define MAKE(x, y) [x] = y
        MAKE( IROP1_INC,    "INC" ),
        MAKE( IROP1_DEC,    "DEC" ),
        MAKE( IROP1_NEG,    "NEG" ),
        MAKE( IROP1_BITNEG, "BITNEG" ),
#undef MAKE
};

const char *const irOp2String[NUM_IROP2_KINDS] = {
#define MAKE(x, y) [x] = y
        MAKE( IROP2_ADD, "ADD" ),
        MAKE( IROP2_SUB, "SUB" ),
        MAKE( IROP2_MUL, "MUL" ),
        MAKE( IROP2_DIV, "DIV" ),
#undef MAKE
};

const char *const irCmpString[NUM_IRCMP_KINDS] = {
#define MAKE(x, y) [x] = y
        MAKE( IRCMP_LT, "cmp<" ),
        MAKE( IRCMP_GT, "cmp>" ),
        MAKE( IRCMP_LE, "cmp<=" ),
        MAKE( IRCMP_GE, "cmp>=" ),
        MAKE( IRCMP_EQ, "cmp==" ),
        MAKE( IRCMP_NE, "cmp!=" ),
#undef MAKE
};

const struct ToktypeToPrefixUnop toktypeToPrefixUnop[] = {
        { TOKEN_TILDE,       UNOP_INVERTBITS },
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

const struct UnopInfo unopInfo[NUM_UNOPS] = {
#define MAKE(x, y, z) [x] = { y, z }
        MAKE( UNOP_INVERTBITS,    1, "~"  ),
        MAKE( UNOP_NOT,           1, "!"  ),
        MAKE( UNOP_ADDRESSOF,     1, "&"  ),
        MAKE( UNOP_DEREF,         1, "^"  ),
        MAKE( UNOP_NEGATIVE,      1, "-"  ),
        MAKE( UNOP_POSITIVE,      1, "+"  ),
        MAKE( UNOP_PREDECREMENT,  1, "--" ),
        MAKE( UNOP_PREINCREMENT,  1, "++" ),
        MAKE( UNOP_POSTDECREMENT, 0, "--" ),
        MAKE( UNOP_POSTINCREMENT, 0, "++" ),
#undef MAKE
};

const struct BinopInfo binopInfo[NUM_BINOPS] = {
#define MAKE(x, y, z) [x] = { y, z }
        MAKE( BINOP_ASSIGN, 1,  "="  ),
        MAKE( BINOP_GT, 2,  ">" ),
        MAKE( BINOP_LT, 2,  "<" ),
        MAKE( BINOP_GE, 2,  ">=" ),
        MAKE( BINOP_LE, 2,  "<=" ),
        MAKE( BINOP_EQ, 2,  "==" ),
        MAKE( BINOP_NE, 2,  "!=" ),
        MAKE( BINOP_MINUS,  3,  "-"  ),
        MAKE( BINOP_PLUS,   3,  "+"  ),
        MAKE( BINOP_MUL,    4,  "*"  ),
        MAKE( BINOP_DIV,    4,  "/"  ),
        MAKE( BINOP_BITAND, 4,  "&"  ),
        MAKE( BINOP_BITOR,  4,  "|"  ),
        MAKE( BINOP_BITXOR, 4,  "^"  ),
#undef MAKE
};

const struct StringToBeInterned stringsToBeInterned[NUM_CONSTSTRS] = {
#define MAKE(x, y) [x] = { x, y }
        MAKE( CONSTSTR_IF,     "if"     ),
        MAKE( CONSTSTR_ELSE,   "else"   ),
        MAKE( CONSTSTR_WHILE,  "while"  ),
        MAKE( CONSTSTR_FOR,    "for"    ),
        MAKE( CONSTSTR_DO,     "do"     ),
        MAKE( CONSTSTR_RETURN, "return" ),
        MAKE( CONSTSTR_BREAK,  "break"  ),
        MAKE( CONSTSTR_PROC,   "proc"   ),
        MAKE( CONSTSTR_MACRO,  "macro"  ),
        MAKE( CONSTSTR_STRUCT, "struct" ),
        MAKE( CONSTSTR_DATA,   "data"   ),
        MAKE( CONSTSTR_ENTITY, "entity" ),
        MAKE( CONSTSTR_ARRAY,  "array"  ),
        MAKE( CONSTSTR_EXPORT, "export" ),
        MAKE( CONSTSTR_IN,     "in"     ),
        MAKE( CONSTSTR_FROM,   "from"   ),
        MAKE( CONSTSTR_TO,     "to"     ),
        MAKE( CONSTSTR_DOWNTO, "downto" ),
        MAKE( CONSTSTR_SIZEOF, "sizeof" ),
        MAKE( CONSTSTR_IGNORE, "ignore" ),
        MAKE( CONSTSTR_EXTERN, "extern" ),
        MAKE( CONSTSTR_STRINGIFY, "stringify" ),
        MAKE( CONSTSTR_CONSTANT,  "constant" ),
#undef MAKE
};

const struct BasetypeToBeInitialized basetypesToBeInitialized[] = {
        { "void", -42,  &builtinType[BUILTINTYPE_VOID] },
        { "int",    8,  &builtinType[BUILTINTYPE_INT] },
        { "char",   1,  &builtinType[BUILTINTYPE_CHAR] },
};

const int toktypeToPrefixUnopCnt = LENGTH(toktypeToPrefixUnop);
const int toktypeToPostfixUnopCnt = LENGTH(toktypeToPostfixUnop);
const int toktypeToBinopCnt = LENGTH(toktypeToBinop);
const int basetypesToBeInitializedCnt = LENGTH(basetypesToBeInitialized);

const struct GlobalBufferInfo globalBufferInfo[NUM_BUFFERS] = {
#define MAKE_GLOBAL_BUFFER(cnt, b) [BUFFER_##b] = { (void **) &b, sizeof *b, #b, &cnt }
        GLOBAL_BUFFERS_X_MACRO
#undef MAKE_GLOBAL_BUFFER
};
