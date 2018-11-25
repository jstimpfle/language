#include "defs.h"
#define DATA_IMPL
#include "api.h"

const char lvl_debug[] = "DEBUG";
const char lvl_info[] = "INFO";
const char lvl_warn[] = "WARN";
const char lvl_error[] = "ERROR";
const char lvl_fatal[] = "FATAL";

const char *const tokenKindString[] = {
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
        MAKE( TOKEN_ASSIGNEQUALS ),
        MAKE( TOKEN_GT ),
        MAKE( TOKEN_LT ),
        MAKE( TOKEN_GE ),
        MAKE( TOKEN_LE ),
        MAKE( TOKEN_EQ ),
        MAKE( TOKEN_NE ),
#undef MAKE
};

const char *const exprKindString[] = {
#define MAKE(x) [x] = #x
        MAKE( EXPR_LITERAL ),
        MAKE( EXPR_SYMREF ),
        MAKE( EXPR_UNOP ),
        MAKE( EXPR_BINOP ),
        MAKE( EXPR_MEMBER ),
        MAKE( EXPR_SUBSCRIPT ),
        MAKE( EXPR_CALL ),
#undef MAKE
};

const char *const typeKindString[] = {
#define MAKE(x) [x] = #x
        MAKE( TYPE_BASE ),
        MAKE( TYPE_ENTITY ),
        MAKE( TYPE_ARRAY ),
        MAKE( TYPE_POINTER ),
        MAKE( TYPE_PROC ),
        MAKE( TYPE_REFERENCE ),
#undef MAKE
};


const struct ToktypeToPrefixUnop toktypeToPrefixUnop[] = {
        { TOKEN_TILDE,       UNOP_INVERTBITS },
        { TOKEN_BANG,        UNOP_NOT },
        { TOKEN_AMPERSAND,   UNOP_ADDRESSOF },
        { TOKEN_ASTERISK,    UNOP_DEREF },
        { TOKEN_MINUS,       UNOP_NEGATIVE },
        { TOKEN_PLUS,        UNOP_POSITIVE },
        { TOKEN_DOUBLEMINUS, UNOP_PREDECREMENT },
        { TOKEN_DOUBLEPLUS,  UNOP_PREINCREMENT },
};

const struct ToktypeToPostfixUnop toktypeToPostfixUnop[] = {
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
        { TOKEN_CARET,        BINOP_BITXOR  },
};

const struct UnopInfo unopInfo[NUM_UNOPS] = {
#define MAKE(x, y, z) [x] = { y, z }
        MAKE( UNOP_INVERTBITS,    1, "~"  ),
        MAKE( UNOP_NOT,           1, "!"  ),
        MAKE( UNOP_ADDRESSOF,     1, "&"  ),
        MAKE( UNOP_DEREF,         1, "*"  ),
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
        MAKE( CONSTSTR_RETURN, "return" ),
        MAKE( CONSTSTR_PROC,   "proc"   ),
        MAKE( CONSTSTR_DATA,   "data"   ),
        MAKE( CONSTSTR_ENTITY, "entity" ),
        MAKE( CONSTSTR_ARRAY,  "array"  ),
#undef MAKE
};

const struct BasetypeToBeInitialized basetypesToBeInitialized[] = {
        { "void", -42 },
        { "int", 4 },
        { "char", 1 },
};

const int toktypeToPrefixUnopCnt = LENGTH(toktypeToPrefixUnop);
const int toktypeToPostfixUnopCnt = LENGTH(toktypeToPostfixUnop);
const int toktypeToBinopCnt = LENGTH(toktypeToBinop);
const int basetypesToBeInitializedCnt = LENGTH(basetypesToBeInitialized);

const struct GlobalBufferInfo globalBufferInfo[NUM_BUFFERS] = {
#define MAKE(b) [BUFFER_##b] = { (void **) &b, sizeof *b }
        /* str.h */
        MAKE( stringInfo ),
        MAKE( strBucketInfo ),
        /* parsing */
        MAKE( lexbuf ),
        MAKE( strbuf ),
        MAKE( fileInfo ),
        MAKE( tokenInfo ),
        MAKE( symrefToToken ),
        MAKE( typeInfo ),
        MAKE( symbolInfo ),
        MAKE( dataInfo ),
        MAKE( arrayInfo ),
        MAKE( scopeInfo ),
        MAKE( procInfo ),
        MAKE( procToType ),
        MAKE( paramInfo ),
        MAKE( symrefInfo ),
        MAKE( symrefToSym ),
        MAKE( exprInfo ),
        MAKE( exprType ),
        MAKE( stmtInfo ),
        MAKE( childStmtInfo ),
        MAKE( callArgInfo ),
        /* AST -> IR */
        MAKE( procToIrProc ),
        MAKE( dataToIrReg ),
        MAKE( exprToIrReg ),
        /* IR */
        MAKE( irSymbolInfo ),
        MAKE( irRegInfo ),
        MAKE( irStmtInfo ),
        MAKE( irCallArgInfo ),
        MAKE( irCallResultInfo ),
        MAKE( irReturnResultInfo ),
        MAKE( irProcInfo ),
        MAKE( irLabelInfo ),
        MAKE( irOrigin ),
        /* Codegen */
        MAKE( rodataSection ),
        MAKE( dataSection ),
        MAKE( codeSection ),
        MAKE( symDefInfo ),
        MAKE( gotoInfo ),
        MAKE( relocInfo ),
        MAKE( irstmtToCodepos ),
        MAKE( irprocToCodepos ),
        /* X64 Asm */
        MAKE( x64StackLocInfo ),
        /* */
#undef MAKE
};
