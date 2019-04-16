#include "defs.h"
#include "api.h"

INTERNAL int look_char(void)
{
        if (currentOffset >= fileInfo[currentFile].size)
                return -1;
        int c = fileInfo[currentFile].buf[currentOffset];
        if (c < 32 && c != '\n')
                FATAL_LEX_ERROR("Invalid byte %d\n", c);
        return c;
}

INTERNAL int look_char_noeol(const char *context)
{
        int c = look_char();
        if (c == -1)
                FATAL_LEX_ERROR(
                        "EOF encountered while %s\n", context);
        return c;
}

INTERNAL void consume_char(void)
{
        ASSERT(currentOffset < fileInfo[currentFile].size);
        currentOffset++;
}

INTERNAL int char_as_hex(int c)
{
        if ('0' <= c && c <= '9')
                return c - '0';
        if ('A' <= c && c <= 'F')
                return 10 + c - 'A';
        if ('a' <= c && c <= 'f')
                return 10 + c - 'a';
        return -1;
}

INTERNAL int read_hex_char(void)
{
        int c = look_char_noeol("reading hex character");
        consume_char();
        int x = char_as_hex(c);
        if (x == -1)
                FATAL_LEX_ERROR("Invalid character 0x%.2x encountered "
                        "while reading hex escape sequence\n", c);
        return x;
}

INTERNAL int read_escape_sequence(void)
{
        int c = look_char_noeol("reading escape sequence");
        consume_char();
        if (c == 't')
                return 0x09;
        else if (c == 'n')
                return 0x0a;
        else if (c == 'r')
                return 0x0d;
        else if (c == '\\')
                return '\\';
        else if (c == 'x') {
                int x1 = read_hex_char();
                int x2 = read_hex_char();
                return x1 * 16 + x2;
        }
        FATAL_LEX_ERROR("escape sequence not supported: '\\<0x%x>'\n", c);
}

INTERNAL void consume_comment(void)
{
        for (;;) {
                int c = look_char();
                if (c == -1) {
                        FATAL_LEX_ERROR("EOF encountered while looking for "
                                        "the end of a C comment\n");
                }
                consume_char();
                if (c == '*' && look_char() == '/') {
                        consume_char();
                        break;
                }
        }
}

INTERNAL void lex_wordtoken(Token token)
{
        lexbufCnt = 0;
        for (;;) {
                int c = look_char();  /* assume valid leader character */
                consume_char();
                int idx = lexbufCnt++;
                RESIZE_GLOBAL_BUFFER(lexbuf, lexbufCnt);
                lexbuf[idx] = (char) c;
                c = look_char();
                if (!('a' <= c && c <= 'z') &&
                    !('A' <= c && c <= 'Z') &&
                    !('0' <= c && c <= '9') &&
                    !(c == '_'))
                        break;
        }
        tokenInfo[token].tokenKind = TOKEN_WORD;
        tokenInfo[token].tWord.string = intern_string(lexbuf, lexbufCnt);
}

INTERNAL void lex_stringtoken(Token token)
{
        consume_char();  /* assume '"' */
        lexbufCnt = 0;
        for (;;) {
                int c = look_char();
                if (c == -1)
                        FATAL_LEX_ERROR("EOF encountered while looking for "
                                        "the end of the string literal\n");
                consume_char();
                if (c == '"')
                        break;
                if (c == '\\')
                        c = read_escape_sequence();
                int idx = lexbufCnt++;
                RESIZE_GLOBAL_BUFFER(lexbuf, lexbufCnt);
                lexbuf[idx] = (char) c;
        }
        tokenInfo[token].tokenKind = TOKEN_STRING;
        tokenInfo[token].tString.value = intern_string(lexbuf, lexbufCnt);
}

INTERNAL void lex_numbertoken(Token token)
{
        long long value;
        int c = look_char();
        consume_char();
        if (c == '0' && look_char() == 'x') {
                value = 0;
                for (;;) {
                        consume_char();
                        c = look_char();
                        int x = char_as_hex(c);
                        if (x == -1)
                                break;
                        value = 16 * value + x;
                }
                tokenInfo[token].tokenKind = TOKEN_INTEGER;
                tokenInfo[token].tInteger.value = value;
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
                if (c == '.') {
                        consume_char();
                        //XXX
                        long long n = 1;
                        for (;; n *= 10) {
                                c = look_char();
                                if (!('0' <= c && c <= '9'))
                                        break;
                                consume_char();
                                value = 10 * value + c - '0';
                        }
                        tokenInfo[token].tokenKind = TOKEN_FLOAT;
                        tokenInfo[token].tFloat.value = (float) value / n;
                } else {
                        tokenInfo[token].tokenKind = TOKEN_INTEGER;
                        tokenInfo[token].tInteger.value = value;
                }
        }
}

INTERNAL UNUSEDFUNC void lex_charlit(Token token)
{
        int charvalue;
        int c = look_char_noeol("lexing character literal");
        consume_char();
        if (c == '\\')
                charvalue = read_escape_sequence();
        else
                charvalue = c;
        c = look_char_noeol("lexing character literal");
        if (c != '\'')
                FATAL_LEX_ERROR("Invalid character literal. "
                        "Expected closing '\n");
        consume_char();
        DEBUG("Value of escape sequence is %d\n", charvalue);
        tokenInfo[token].tokenKind = TOKEN_CHARACTER;
        tokenInfo[token].tCharacter.value = charvalue;
}

INTERNAL void lex_baretoken(Token token)
{
        int c = look_char();
        consume_char();
        int tokenKind = -1;
        for (int i = 0; i < lex1Cnt; i++) {
                if (c == lex1[i].ch) {
                        tokenKind = lex1[i].kind;
                        goto goodbaretoken;
                }
        }
        int c2 = look_char();
        for (int i = 0; i < lex2Cnt; i++) {
                if (c == lex2[i].ch1) {
                        if (c2 == lex2[i].ch2) {
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
                        if (c2 == lex3[i].ch2) {
                                consume_char();
                                tokenKind = lex3[i].kind2;
                        }
                        else if (c2 == lex3[i].ch3) {
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
        tokenInfo[token].tokenKind = tokenKind;
}

Token lex_token(void)
{
        int c;

again:
        for (;;) {
                c = look_char();
                if (c == -1)
                        return -1;
                if (c > ' ')
                        break;
                consume_char();
        }

        /* Currently we use special case code to distinguish comments from
         * slash. */
        if (c == '/') {
                consume_char();
                if (look_char() == '*') {
                        consume_char();
                        consume_comment();
                        goto again;
                }
                else {
                        Token token = tokenCnt++;
                        RESIZE_GLOBAL_BUFFER(tokenInfo, tokenCnt);
                        tokenInfo[token].file = currentFile;
                        tokenInfo[token].offset = currentOffset - 1;
                        tokenInfo[token].tokenKind = TOKEN_SLASH;
                        return token;
                }
        }

        /* Start of token found. Variable c contains first character to lex */
        Token token = tokenCnt++;
        RESIZE_GLOBAL_BUFFER(tokenInfo, tokenCnt);
        tokenInfo[token].file = currentFile;
        tokenInfo[token].offset = currentOffset - 1;
        /* remaining fields of tokenInfo[x] are set depending on token type */
        if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_')
                lex_wordtoken(token);
        else if ('0' <= c && c <= '9')
                lex_numbertoken(token);
        else if (c == '"')
                lex_stringtoken(token);
        /*
        else if (c == '\'')
                lex_chartoken(token);
                */
        else
                lex_baretoken(token);
        return token;
}
