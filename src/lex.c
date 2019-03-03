#include "defs.h"
#include "api.h"

INTERNAL
int look_char(void)
{
        if (currentOffset >= fileInfo[currentFile].size)
                return -1;
        int c = fileInfo[currentFile].buf[currentOffset];
        if (c < 32 && c != '\n')
                FATAL_LEX_ERROR("Invalid byte %d\n", c);
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
                        FATAL_LEX_ERROR("EOF encountered while looking for "
                                        "the end of a C comment\n");
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
        if (c == '0' && look_char() == 'x') {
                consume_char();
                value = 0;
                for (;;) {
                        c = look_char();
                        if ('0' <= c && c <= '9')
                                value = 16 * value + c - '0';
                        else if ('a' <= c && c <= 'f')
                                value = 16 * value + c - 'a';
                        else if ('A' <= c && c <= 'F')
                                value = 16 * value + c - 'A';
                        else
                                break;
                        consume_char();
                }
        }
        else {
                value = c - '0';
                for (;;) {
                        c = look_char();
                        if (!('0' <= c && c <= '9'))
                                break;
                        consume_char();
                        value = 10 * value + c - '0';
                }
        }
        tokenInfo[x].tokenKind = TOKEN_INTEGER;
        tokenInfo[x].tInteger.value = value;
        return x;

stringlit:
        lexbufCnt = 0;
        for (;;) {
                c = look_char();
                if (c == -1)
                        FATAL_LEX_ERROR("EOF encountered while looking for "
                                        "the end of the string literal\n");
                consume_char();
                if (c == '"')
                        break;
                if (c == '\\') {
                        c = look_char();
                        if (c == -1)
                                FATAL_LEX_ERROR(
                                        "EOF encountered while looking for "
                                        "the end of the escape sequence\n");
                        consume_char();
                        if (c == 'n')
                                c = '\n';
                        else if (c == '\\') {
                                /* (c = '\\') */
                        }
                        else {
                                FATAL_LEX_ERROR(
                                        "escape sequence not supported: "
                                        "'\\<0x%x>'\n", c);
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
        for (int i = 0; i < lex1Cnt; i++) {
                if (c == lex1[i].ch) {
                        tokenKind = lex1[i].kind;
                        goto goodbaretoken;
                }
        }
        for (int i = 0; i < lex2Cnt; i++) {
                if (c == lex2[i].ch1) {
                        if (look_char() == lex2[i].ch2) {
                                consume_char();
                                tokenKind = lex2[i].kind2;
                        }
                        else {
                                tokenKind = lex2[i].kind1;
                        }
                        goto goodbaretoken;
                }
        }
        for (int i = 0; i < lex3Cnt; i++) {
                if (c == lex3[i].ch1) {
                        if (look_char() == lex3[i].ch2) {
                                consume_char();
                                tokenKind = lex3[i].kind2;
                        }
                        else if (look_char() == lex3[i].ch3) {
                                consume_char();
                                tokenKind = lex3[i].kind3;
                        }
                        else {
                                tokenKind = lex3[i].kind1;
                        }
                        goto goodbaretoken;
                }
        }
        FATAL_LEX_ERROR("Failed to lex token\n");
goodbaretoken:
        tokenInfo[x].tokenKind = tokenKind;
        return x;
}
