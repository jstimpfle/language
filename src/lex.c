#include "defs.h"
#include "api.h"

INTERNAL int haveSavedChar;
INTERNAL int haveSavedToken;
INTERNAL int savedChar;
INTERNAL Token savedToken;

INTERNAL
Token add_word_token(File file, int offset, String string)
{
        Token x = tokenCnt++;
        RESIZE_GLOBAL_BUFFER(tokenInfo, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = TOKEN_WORD;
        tokenInfo[x].tWord.string = string;
        return x;
}

INTERNAL
Token add_integer_token(File file, int offset, long long value)
{
        Token x = tokenCnt++;
        RESIZE_GLOBAL_BUFFER(tokenInfo, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = TOKEN_INTEGER;
        tokenInfo[x].tInteger.value = value;
        return x;
}

INTERNAL
Token add_string_token(File file, int offset, String str)
{
        Token x = tokenCnt++;
        RESIZE_GLOBAL_BUFFER(tokenInfo, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = TOKEN_STRING;
        tokenInfo[x].tString.value = str;
        return x;
}

INTERNAL
Token add_bare_token(File file, int offset, int kind)
{
        Token x = tokenCnt++;
        RESIZE_GLOBAL_BUFFER(tokenInfo, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = kind;
        return x;
}

INTERNAL
int look_char(void)
{
        if (haveSavedChar)
                return savedChar;
        if (currentOffset < fileInfo[currentFile].size) {
                int c = fileInfo[currentFile].buf[currentOffset];
                if (c < 32 && c != '\n')
                        FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
                                             "Invalid byte %d\n", c);
                haveSavedChar = 1;
                savedChar = c;
                return c;
        }
        return -1;
}

INTERNAL
int read_char(void)
{
        int c = look_char();
        if (c != -1) {
                currentOffset++;
                haveSavedChar = 0;
        }
        return c;
}

Token parse_next_token(void)
{
        int c;
        Token ans;

        if (haveSavedToken) {
                haveSavedToken = 0;
                return savedToken;
        }

        /* skip comments and whitespace */
        for (;;) {
                c = read_char();
                if (c == -1)
                        return -1;
                if (c == '/' && look_char() == '*') {
                        read_char();
                        for (;;) {
                                c = read_char();
                                if (c == -1) {
                                        FATAL_PARSE_ERROR_AT(
                                                currentFile, currentOffset,
                                                "EOF with unclosed comment\n");
                                }
                                if (c == '*' && look_char() == '/') {
                                        read_char();
                                        break;
                                }
                        }
                        continue;
                }
                if (c != ' ' && c != '\n')
                        break;
        }

        /* good to go. Variable c contains first character to lex */
        int off = currentOffset - 1;
        if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_') {
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
                        read_char();
                }
                ans = add_word_token(currentFile, off,
                                     intern_string(lexbuf, lexbufCnt));
        }
        else if ('0' <= c && c <= '9') {
                long long x = c - '0';
                for (;;) {
                        c = look_char();
                        if (!('0' <= c && c <= '9'))
                                break;
                        read_char();
                        x = 10 * x + c - '0';
                }
                ans = add_integer_token(currentFile, off, x);
        }
        else if (c == '"') {
                lexbufCnt = 0;
                for (;;) {
                        c = read_char();
                        if (c == '"')
                                break;
                        if (c == '\\') {
                                c = read_char();
                                if (c == 'n')
                                        c = '\n';
                                else if (c == '\\') {
                                        /* (c = '\\') */
                                }
                                else {
                                        FATAL_PARSE_ERROR_AT(
                                                currentFile, currentOffset,
                "escape sequence not supported: '\\<0x%x>'\n", c);
                                }
                        }
                        int idx = lexbufCnt++;
                        RESIZE_GLOBAL_BUFFER(lexbuf, lexbufCnt);
                        lexbuf[idx] = c;
                }
                ans = add_string_token(currentFile, off,
                                       intern_string(lexbuf, lexbufCnt));
        }
        else {
                static const struct { char c; int kind; } t1[] = {
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
                };

                static const struct {
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

                ans = -1;
                int tokenKind = -1;

                for (int i = 0; i < LENGTH(t1); i++) {
                        if (c == t1[i].c) {
                                tokenKind = t1[i].kind;
                                goto good;
                        }
                }

                for (int i = 0; i < LENGTH(t2); i++) {
                        if (c == t2[i].c1) {
                                c = look_char();
                                if (c == t2[i].c2) {
                                        read_char();
                                        tokenKind = t2[i].k2;
                                }
                                else {
                                        tokenKind = t2[i].k1;
                                }
                                goto good;
                        }
                }

                FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
                                     "Failed to lex token\n");
good:
                ans = add_bare_token(currentFile, off, tokenKind);
        }

        return ans;
}

Token look_next_token(void)
{
        if (haveSavedToken)
                return savedToken;
        else {
                savedToken = parse_next_token();
                haveSavedToken = 1;
                return savedToken;
        }
}

Token parse_token_kind(int tkind)
{
        Token tok = parse_next_token();
        if (tok == -1) {
                FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
                               "Unexpected end of file. Expected %s token\n",
                               tokenKindString[tkind]);
        }
        int k = tokenInfo[tok].kind;
        if (k != tkind) {
                FATAL_PARSE_ERROR_AT_TOK(tok, "Expected %s token\n",
                                      tokenKindString[tkind]);
        }
        return tok;
}

Token look_token_kind(int tkind)
{
        Token tok = look_next_token();
        if (tok == -1 || tokenInfo[tok].kind != tkind)
                return -1;
        return tok;
}
