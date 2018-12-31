#include "defs.h"
#include "api.h"

INTERNAL const struct {
        char c;
        int k;
} t1[] = {
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

INTERNAL const struct {
        char c1;
        char c2;
        int k1;
        int k2;
} t2[] = {
        { '=', '=', TOKEN_ASSIGNEQUALS, TOKEN_EQ },
        { '+', '+', TOKEN_PLUS, TOKEN_DOUBLEPLUS },
        { '-', '-', TOKEN_MINUS, TOKEN_DOUBLEMINUS },
        { '!', '=', TOKEN_BANG, TOKEN_NE },
        { '>', '=', TOKEN_GT, TOKEN_GE },
        { '<', '=', TOKEN_LT, TOKEN_LE, },
};


INTERNAL
int look_char(void)
{
        if (currentOffset >= fileInfo[currentFile].size)
                return -1;
        int c = fileInfo[currentFile].buf[currentOffset];
        if (c < 32 && c != '\n')
                FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
                                     "Invalid byte %d\n", c);
        return c;
}

INTERNAL
void consume_char(void)
{
        ASSERT(currentOffset < fileInfo[currentFile].size);
        currentOffset++;
}

Token lex_token(void)
{
        int c;
looknonwhite:
        /* look for first non-white character */
        for (;;) {
                c = look_char();
                if (c == -1)
                        return -1;
                consume_char();
                if (c == '/' && look_char() == '*') {
                        consume_char();
                        goto lookcommentend;
                }
                else if (c > ' ')
                        goto tokenstart;
        }

lookcommentend:
        /* look for the end of a C-style comment */
        for (;;) {
                c = look_char();
                if (c == -1) {
                        FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
                "EOF encountered while looking for the end of a C comment\n");
                }
                consume_char();
                if (c == '*' && look_char() == '/') {
                        consume_char();
                        goto looknonwhite;
                }
        }

        Token x;
tokenstart:
        /* Start of token found. Variable c contains first character to lex */
        x = tokenCnt++;
        RESIZE_GLOBAL_BUFFER(tokenInfo, tokenCnt);
        tokenInfo[x].file = currentFile;
        tokenInfo[x].offset = currentOffset - 1;
        /* remaining fields of tokenInfo[x] are set depending on token type */
        if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_')
                goto wordtoken;
        else if ('0' <= c && c <= '9')
                goto numbertoken;
        else if (c == '"')
                goto stringlit;
        else
                goto baretoken;

wordtoken:
        lexbufCnt = 0;
        for (;;) {
                int idx = lexbufCnt++;
                RESIZE_GLOBAL_BUFFER(lexbuf, lexbufCnt);
                lexbuf[idx] = (char) c;
                c = look_char();
                if (!('a' <= c && c <= 'z') &&
                    !('A' <= c && c <= 'Z') &&
                    !('0' <= c && c <= '9') &&
                    !(c == '_'))
                        break;
                consume_char();
        }
        tokenInfo[x].tokenKind = TOKEN_WORD;
        tokenInfo[x].tWord.string = intern_string(lexbuf, lexbufCnt);
        return x;

        long long value;
numbertoken:
        value = c - '0';
        for (;;) {
                c = look_char();
                if (!('0' <= c && c <= '9'))
                        break;
                consume_char();
                value = 10 * value + c - '0';
        }
        tokenInfo[x].tokenKind = TOKEN_INTEGER;
        tokenInfo[x].tInteger.value = value;
        return x;

stringlit:
        lexbufCnt = 0;
        for (;;) {
                c = look_char();
                if (c == -1)
                        FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
        "EOF encountered while looking for end of string literal\n");
                consume_char();
                if (c == '"')
                        break;
                if (c == '\\') {
                        c = look_char();
                        if (c == -1)
                                FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
        "EOF encountered while looking for end of escape sequence\n");
                        consume_char();
                        if (c == 'n')
                                c = '\n';
                        else if (c == '\\') {
                                /* (c = '\\') */
                        }
                        else {
                                FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
        "escape sequence not supported: '\\<0x%x>'\n", c);
                        }
                }
                int idx = lexbufCnt++;
                RESIZE_GLOBAL_BUFFER(lexbuf, lexbufCnt);
                lexbuf[idx] = c;
        }
        tokenInfo[x].tokenKind = TOKEN_STRING;
        tokenInfo[x].tString.value = intern_string(lexbuf, lexbufCnt);
        return x;

        int tokenKind;
baretoken:
        tokenKind = -1;
        for (int i = 0; i < LENGTH(t1); i++) {
                if (c == t1[i].c) {
                        tokenKind = t1[i].k;
                        goto goodbaretoken;
                }
        }
        for (int i = 0; i < LENGTH(t2); i++) {
                if (c == t2[i].c1) {
                        if (look_char() == t2[i].c2) {
                                consume_char();
                                tokenKind = t2[i].k2;
                        }
                        else {
                                tokenKind = t2[i].k1;
                        }
                        goto goodbaretoken;
                }
        }
        FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
                             "Failed to lex token\n");
goodbaretoken:
        tokenInfo[x].tokenKind = tokenKind;
        return x;
}
