#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif


/**
 * \enum{UnopKind}: Unary operators. Currently all correspond to single tokens,
 * like ~, !, &, ++, and others.
 *
 * \enum{BinopKind}: Binary operators. Currently all correspond to single
 * tokens, like =, ==, -, +, *, /, and others.
 *
 */

enum UnopKind {
        UNOP_BITWISENOT,
        UNOP_NOT,
        UNOP_ADDRESSOF,
        UNOP_DEREF,
        UNOP_NEGATIVE,
        UNOP_POSITIVE,
        UNOP_PREDECREMENT,
        UNOP_PREINCREMENT,
        UNOP_POSTDECREMENT,
        UNOP_POSTINCREMENT,
        NUM_UNOP_KINDS,
};

enum BinopKind {
        BINOP_ASSIGN,
        BINOP_GT,
        BINOP_LT,
        BINOP_LE,
        BINOP_GE,
        BINOP_EQ,
        BINOP_NE,
        BINOP_MINUS,
        BINOP_PLUS,
        BINOP_MUL,
        BINOP_DIV,
        BINOP_BITAND,
        BINOP_BITOR,
        BINOP_BITXOR,
        NUM_BINOP_KINDS,
};

/*
 * \struct{ToktypeToPrefixUnop}, \data{toktypeToPrefixUnop},
 * \data{toktypeToPrefixUnopCnt}: Map (some) token types to prefix operators.
 *
 * \struct{ToktypeToPostfixUnop}, \data{toktypeToPostfixUnop},
 * \data{toktypeToPostfixUnopCnt}: Map (some) token types to postfix operators.
 *
 * \struct{ToktypeToInfixUnop}, \data{toktypeToInfixUnop},
 * \data{toktypeToInfixUnopCnt}: Map (some) token types to to infix operators.
 *
 * \struct{UnopInfo}, \data{unopInfo}: Map unary operators to additional
 * information: string representation and whether it is a prefix or postfix
 * operator.
 *
 * \struct{BinopInfo}, \data{binopInfo}: Map binary operators to additional
 * information: string representation and precedence (associativity),
 */

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

extern const char *const unopKindString[NUM_UNOP_KINDS];
extern const char *const binopKindString[NUM_BINOP_KINDS];
extern const char *const unopString[NUM_UNOP_KINDS];
extern const char *const binopString[NUM_BINOP_KINDS];
extern const int unopIsPrefix[NUM_UNOP_KINDS];
extern const int binopPrec[NUM_BINOP_KINDS];

extern const struct ToktypeToPrefixUnop toktypeToPrefixUnop[];
extern const struct ToktypeToPostfixUnop toktypeToPostfixUnop[];
extern const struct ToktypeToBinop toktypeToBinop[];

extern const int toktypeToPrefixUnopCnt;
extern const int toktypeToPostfixUnopCnt;
extern const int toktypeToBinopCnt;
