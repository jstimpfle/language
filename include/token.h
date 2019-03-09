#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif


/**
 * \typedef{Token}: A Token is a syntactical entity as seen by the compiler's
 * language parser. For example, an identifier such as `xyz`, an integer literal
 * such as `9`, a string literal such as `"a string"`, or an operator such as
 * `++`. See \ref{TokenKind}.
 */

typedef int Token;

/**
 * \enum{TokenKind}: Token kinds (lexical syntax)
 */

enum TokenKind {
        TOKEN_WORD,
        TOKEN_INTEGER,
        TOKEN_FLOAT,
        TOKEN_STRING,
        TOKEN_LEFTPAREN,
        TOKEN_RIGHTPAREN,
        TOKEN_LEFTBRACE,
        TOKEN_RIGHTBRACE,
        TOKEN_LEFTBRACKET,
        TOKEN_RIGHTBRACKET,
        TOKEN_DOT,
        TOKEN_MINUS,
        TOKEN_PLUS,
        TOKEN_ASTERISK,
        TOKEN_SLASH,
        TOKEN_DOUBLEMINUS,
        TOKEN_DOUBLEPLUS,
        TOKEN_COMMA,
        TOKEN_SEMICOLON,
        TOKEN_COLON,
        TOKEN_AMPERSAND,
        TOKEN_PIPE,
        TOKEN_CARET,
        TOKEN_TILDE,
        TOKEN_BANG,
        TOKEN_HASH,
        TOKEN_DOLLAR,
        TOKEN_BACKSLASH,
        TOKEN_QUESTIONMARK,
        TOKEN_ATSIGN,
        TOKEN_ASSIGNEQUALS,
        TOKEN_RIGHTARROW,
        TOKEN_GT,
        TOKEN_LT,
        TOKEN_GE,
        TOKEN_LE,
        TOKEN_EQ,
        TOKEN_NE,
        NUM_TOKEN_KINDS,
};

struct Lex1 {
        char ch;
        char kind;
};

struct Lex2 {
        char ch1;
        char ch2;
        char kind1;
        char kind2;
};

struct Lex3 {
        char ch1;
        char ch2;
        char ch3;
        char kind1;
        char kind2;
        char kind3;
};

/**
 * \struct{TokenInfo}, \struct{WordTokenInfo}, \struct{IntegerTokenInfo},
 * \struct{StringTokenInfo}: TODO
 */

struct WordTokenInfo {
        String string;
};

struct IntegerTokenInfo {
        long long value;
};

struct FloatTokenInfo {
        float value;
};

struct StringTokenInfo {
        String value;
};

struct TokenInfo {
        File file;
        int offset;
        int tokenKind;
        union {
                struct WordTokenInfo tWord;
                struct IntegerTokenInfo tInteger;
                struct FloatTokenInfo tFloat;
                struct StringTokenInfo tString;
        };
};

/*
 * Constant data
 */

extern const char *const tokenKindString[NUM_TOKEN_KINDS];

extern const struct Lex1 lex1[];
extern const struct Lex2 lex2[];
extern const struct Lex3 lex3[];

extern const int lex1Cnt;
extern const int lex2Cnt;
extern const int lex3Cnt;

/*
 * Dynamic data
 */

DATA int lexbufCnt;
DATA int tokenCnt;

DATA char *lexbuf;
DATA struct TokenInfo *tokenInfo;
