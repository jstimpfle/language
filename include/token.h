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
        TOKEN_ASSIGNEQUALS,
        TOKEN_GT,
        TOKEN_LT,
        TOKEN_GE,
        TOKEN_LE,
        TOKEN_EQ,
        TOKEN_NE,
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

struct StringTokenInfo {
        String value;
};

struct TokenInfo {
        File file;
        int offset;
        int kind;
        union {
                struct WordTokenInfo tWord;
                struct IntegerTokenInfo tInteger;
                struct StringTokenInfo tString;
        };
};

/**
 */

extern const char *const tokenKindString[];
extern const char *const exprKindString[];
extern const int toktypeToPrefixUnopCnt;
extern const int toktypeToPostfixUnopCnt;
extern const int toktypeToBinopCnt;
