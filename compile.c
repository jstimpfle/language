#include "defs.h"
#include "api.h"
#include "io.h"

#define SS(sym) (string_buffer(symbolInfo[sym].name))
#define TS(tok) (string_buffer(tokenInfo[tok].word.string))
#define TKS(tok) (tokenKindString[tokenInfo[tok].kind])

String add_string(const char *buf, int len)
{
        String s = stringCnt;
        int pos = strbufCnt;

        stringCnt += 1;
        strbufCnt += len + 1;

        // Allocate one more to support string_length()
        BUF_RESERVE(stringInfo, stringInfoAlloc, stringCnt + 1);
        stringInfo[s].pos = pos;
        stringInfo[s+1].pos = pos + len + 1;

        BUF_RESERVE(strbuf, strbufAlloc, strbufCnt);
        memcpy(&strbuf[pos], buf, len);
        strbuf[pos + len] = '\0';

        return s;
}

unsigned hash_string(const void *str, int len)
{
        int i;
        unsigned hsh = 5381;
        for (i = 0; i < len; i++)
                hsh = 33*hsh + ((const unsigned char *)str)[i];
        return hsh;
}

String lookup_string_with_hash(const void *buf, int len, unsigned hsh)
{
        unsigned bck;
        String s;

        bck = hsh & (strBucketCnt - 1);
        for (s = strBucketInfo[bck].firstString; s != -1;
             s = stringInfo[s].next) {
                if (string_length(s) == len &&
                    memcmp(string_buffer(s), buf, len) == 0)
                        return s;
        }
        return -1;
}

void insert_string_with_hash(String s, unsigned hsh)
{
        unsigned bck;
       
        bck = hsh & (strBucketCnt - 1);
        stringInfo[s].next = strBucketInfo[bck].firstString;
        strBucketInfo[bck].firstString = s;
}

String intern_string(const void *buf, int len)
{
        int i;
        unsigned hsh;
        String s;

        /* resize hash map if load factor exceeds 66% */
        if (2 * strBucketCnt <= 3 * stringCnt) {
                if (strBucketCnt == 0)
                        strBucketCnt = 256;
                while (2 * strBucketCnt < 3 * stringCnt)
                        strBucketCnt *= 2;

                BUF_RESERVE(strBucketInfo, strBucketInfoAlloc, strBucketCnt);
                for (i = 0; i < strBucketCnt; i++)
                        strBucketInfo[i].firstString = -1;

                for (s = 0; s < stringCnt; s++) {
                        const void *buf = string_buffer(s);
                        int len = string_length(s);
                        unsigned hsh = hash_string(buf, len);
                        insert_string_with_hash(s, hsh);
                }
        }
        // MUST BE POWER OF 2 !!!!
        assert((strBucketCnt & (strBucketCnt-1)) == 0);

        hsh = hash_string(buf, len);
        s = lookup_string_with_hash(buf, len, hsh);
        if (s == -1) {
                s = add_string(buf, len);
                insert_string_with_hash(s, hsh);
        }
        return s;
}

String intern_cstring(const char *str)
{
        return intern_string((const void *)str, strlen(str));
}

File add_file(String filepath)
{
        File x = fileCnt++;
        BUF_RESERVE(fileInfo, fileInfoAlloc, fileCnt);
        fileInfo[x].filepath = filepath;
        read_whole_file(x);
        return x;
}

Token add_word_token(File file, int offset, const char *string, int length)
{
        Token x = tokenCnt++;
        BUF_RESERVE(tokenInfo, tokenInfoAlloc, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = TOKTYPE_WORD;
        tokenInfo[x].word.string = intern_string(string, length);
        return x;
}

Token add_integer_token(File file, int offset, long long value)
{
        Token x = tokenCnt++;
        BUF_RESERVE(tokenInfo, tokenInfoAlloc, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = TOKTYPE_INTEGER;
        tokenInfo[x].integer.value = value;
        return x;
}

Token add_bare_token(File file, int offset, int kind)
{
        Token x = tokenCnt++;
        BUF_RESERVE(tokenInfo, tokenInfoAlloc, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = kind;
        return x;
}

Type add_type(Symbol sym)
{
        Type x = typeCnt++;
        BUF_RESERVE(typeInfo, typeInfoAlloc, typeCnt);
        typeInfo[x].sym = sym;
        return x;
}

Symbol add_symbol(String s)
{
        Symbol x = symbolCnt++;
        BUF_RESERVE(symbolInfo, symbolInfoAlloc, symbolCnt);
        symbolInfo[x].name = s;
        return x;
}

Entity add_entity(Type tp, Symbol sym)
{
        Entity x = entityCnt++;
        BUF_RESERVE(entityInfo, entityInfoAlloc, entityCnt);
        entityInfo[x].tp = tp;
        entityInfo[x].sym = sym;
        return x;
}

Table add_table(Type tp, Symbol sym)
{
        Table x = tableCnt++;
        BUF_RESERVE(tableInfo, tableInfoAlloc, tableCnt);
        tableInfo[x].tp = tp;
        tableInfo[x].sym = sym;
        return x;
}

Column add_column(Table table, Type tp, Symbol sym)
{
        Column x = columnCnt++;
        BUF_RESERVE(columnInfo, columnInfoAlloc, columnCnt);
        columnInfo[x].tp = tp;
        columnInfo[x].sym = sym;
        return x;
}

Data add_data(Scope scope, Type tp, Symbol sym)
{
        Data x = dataCnt++;
        BUF_RESERVE(dataInfo, dataInfoAlloc, dataCnt);
        dataInfo[x].scope = scope;
        dataInfo[x].tp = tp;
        dataInfo[x].sym = sym;
        return x;
}

Proc add_proc(Type tp, Symbol sym)
{
        Proc x = procCnt++;
        BUF_RESERVE(procInfo, procInfoAlloc, procCnt);
        procInfo[x].tp = tp;
        procInfo[x].sym = sym;
        return x;
}

void add_procarg(Proc proc, Type argtp, Symbol argsym)
{
}

Expr add_symref_expr(Token tok)
{
        return -1;
}

Expr add_literal_expr(Token tok)
{
        return -1;
}

Expr add_call_expr(Expr callee)
{
        return -1;
}

Expr add_unop_expr(int opkind, Expr expr)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_UNOP;
        exprInfo[x].unop.kind = opkind;
        exprInfo[x].unop.expr = expr;
        return x;
}

Expr add_binop_expr(int opkind, Expr expr1, Expr expr2)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_BINOP;
        exprInfo[x].binop.kind = opkind;
        exprInfo[x].binop.expr1 = expr1;
        exprInfo[x].binop.expr2 = expr1;
        return x;
}

void init_strings(void)
{
        static const struct {
                int constant;
                const char *string;
        } tbl[] = {
                { CONSTSTR_IF,     "if" },
                { CONSTSTR_WHILE,  "while" },
                { CONSTSTR_FOR,    "for" },
                { CONSTSTR_PROC,   "proc" },
                { CONSTSTR_DATA,   "data" },
                { CONSTSTR_ENTITY, "entity" },
                { CONSTSTR_TABLE,  "table" },
                { CONSTSTR_COLUMN, "column" },
        };

        for (size_t i = 0; i < LENGTH(tbl); i++) {
                int idx = tbl[i].constant;
                const char *str = tbl[i].string;
                constStr[idx] = intern_cstring(str);
        }
}

int char_is_alpha(int c)
{
        return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
}

int char_is_digit(int c)
{
        return '0' <= c && c <= '9';
}

int char_is_whitespace(int c)
{
        return c == ' ' || c == '\n';
}

int char_is_invalid(int c)
{
        return c < 32 && ! char_is_whitespace(c);
}

int token_is_word(Token tok, String string)
{
        return tokenInfo[tok].kind == TOKTYPE_WORD &&
                tokenInfo[tok].word.string == string;
}

int token_is_unary_prefix_operator(Token tok, int *out_optype) {
        static const struct {
                int ttype;
                int optype;
        } tbl[] = {
                { TOKTYPE_BANG,        UNOP_NOT },
                { TOKTYPE_AMPERSAND,   UNOP_ADDRESSOF },
                { TOKTYPE_ASTERISK,    UNOP_DEREF },
                { TOKTYPE_MINUS,       UNOP_NEGATIVE },
                { TOKTYPE_PLUS,        UNOP_POSITIVE },
                { TOKTYPE_DOUBLEMINUS, UNOP_PREDECREMENT },
                { TOKTYPE_DOUBLEPLUS,  UNOP_PREINCREMENT },
        };

        int ans = 0;
        int tp = tokenInfo[tok].kind;
        for (int i = 0; i < LENGTH(tbl); i++) {
                if (tp == tbl[i].ttype) {
                        ans = 1;
                        *out_optype = tbl[i].optype;
                        break;
                }
        }
        return ans;
}

int token_is_binary_infix_operator(Token tok, int *out_optp, int *out_prec)
{
        static const struct {
                int ttp;
                int optp;
                int prec;
        } tbl[] = {
                { TOKTYPE_ASSIGNEQUALS, OP_ASSIGN, 0 },
                { TOKTYPE_DOUBLEEQUALS, OP_EQUALS, 1 },
                { TOKTYPE_MINUS,        OP_MINUS,  2 },
                { TOKTYPE_PLUS,         OP_PLUS,   2 },
                { TOKTYPE_ASTERISK,     OP_MUL,    3 },
                { TOKTYPE_SLASH,        OP_DIV,    3 },
                { TOKTYPE_AMPERSAND,    OP_BITAND, 3 },
                { TOKTYPE_PIPE,         OP_BITOR,  3 },
                { TOKTYPE_CARET,        OP_BITXOR, 3 },
        };

        int ans = 0;
        int tp = tokenInfo[tok].kind;
        for (int i = 0; i < LENGTH(tbl); i++) {
                if (tp == tbl[i].ttp) {
                        ans = 1;
                        *out_prec = tbl[i].prec;
                        break;
                }
        }
        return ans;
}

int token_is_unary_postfix_operator(Token tok, int *out_optype) {
        static const struct {
                int ttype;
                int optype;
        } tbl[] = {
                { TOKTYPE_DOUBLEMINUS, UNOP_POSTDECREMENT },
                { TOKTYPE_DOUBLEPLUS,  UNOP_POSTINCREMENT },
        };

        int ans = 0;
        int tp = tokenInfo[tok].kind;
        for (int i = 0; i < LENGTH(tbl); i++) {
                if (tp == tbl[i].ttype) {
                        ans = 1;
                        *out_optype = tbl[i].optype;
                        break;
                }
        }
        return ans;
}

int read_char(void)
{
        int ans;
        if (have_saved_char) {
                have_saved_char = 0;
                ans = saved_char;
                current_offset++;
        }
        else if (current_offset < fileInfo[current_file].size) {
                int pos = current_offset++;
                ans = fileInfo[current_file].buf[pos];
        }
        else {
                ans = -1;
        }
        return ans;
}

int look_char(void)
{
        if (! have_saved_char) {
                have_saved_char = 1;
                if (current_offset < fileInfo[current_file].size)
                        saved_char = fileInfo[current_file].buf[current_offset];
                else
                        saved_char = -1;
        }
        return saved_char;
}

/* offset may be 1 past the end of file (i.e., equal to file size) */
int compute_lineno(File file, int offset)
{
        int i;
        int line = 1;
        int filesize = fileInfo[file].size;

        for (i = 0; i <= offset; i++)
                if (fileInfo[file].buf[i-1] == '\n')
                        line++;
        return line;
}

/* offset may be 1 past the end of file (i.e., equal to file size) */
int compute_colno(File file, int offset)
{
        int i;
        int column = 1;

        for (i = 1; i <= offset; i++)
                if (fileInfo[file].buf[i-1] == '\n')
                        column = 1;
                else
                        column ++;
        return column;
}

#ifndef NODEBUG
#define PARSE_ERROR_AT(file, offset, mesg, ...) \
        fatal(__FILE__ ", " __FUNCTION__ "(): " "At %s %d:%d: " mesg, \
              string_buffer(fileInfo[file].filepath), \
              compute_lineno(file, offset), \
              compute_colno(file, offset), \
              __VA_ARGS__)
#define PARSE_ERROR(tok, mesg, ...) \
        fatal(__FILE__ ", " __FUNCTION__ "(): " "At %s %d:%d: " \
              "ERROR parsing %s token. " mesg, \
              string_buffer(fileInfo[tokenInfo[tok].file].filepath), \
              compute_lineno(tokenInfo[tok].file, tokenInfo[tok].offset), \
              compute_colno(tokenInfo[tok].file, tokenInfo[tok].offset), \
              tokenKindString[tokenInfo[tok].kind], \
              __VA_ARGS__)
#define PARSE_LOG() \
        msg("AT " __FUNCTION__ "() at %d:%d\n", \
              compute_lineno(current_file, current_offset), \
              compute_colno(current_file, current_offset))
#else
#define PARSE_ERROR_AT(file, offset, mesg, ...) \
        fatal("At %s %d:%d: " mesg, \
              string_buffer(fileInfo[file].filepath), \
              compute_lineno(file, offset), \
              compute_colno(file, offset), \
              __VA_ARGS__)
#define PARSE_ERROR(tok, mesg, ...) \
        fatal("At %s %d:%d: " \
              "ERROR parsing %s token. " mesg, \
              string_buffer(fileInfo[tokenInfo[tok].file].filepath), \
              compute_lineno(tokenInfo[tok].file, tokenInfo[tok].offset), \
              compute_colno(tokenInfo[tok].file, tokenInfo[tok].offset), \
              tokenKindString[tokenInfo[tok].kind], \
              __VA_ARGS__)
#define PARSE_LOG()
#endif

Token parse_next_token(void)
{
        int c;
        int off;
        Token ans;

        if (have_saved_token) {
                have_saved_token = 0;
                return saved_token;
        }

        for (;;) {
                c = look_char();
                if (c == -1)
                        return -1;
                if (char_is_invalid(c))
                        PARSE_ERROR_AT(current_file, current_offset,
                                       "Invalid byte %d\n", c);
                if (! char_is_whitespace(c))
                        break;
                read_char();
        }

        off = current_offset;

        c = read_char();
        if (char_is_alpha(c)) {
                lexbufCnt = 0;
                for (;;) {
                        int idx = lexbufCnt++;
                        BUF_RESERVE(lexbuf, lexbufAlloc, lexbufCnt);
                        lexbuf[idx] = c;
                        c = look_char();
                        if (c == -1)
                                break;
                        if (! char_is_alpha(c))
                                break;
                        read_char();
                }
                ans = add_word_token(current_file, off, lexbuf, lexbufCnt);
        }
        else if (char_is_digit(c)) {
                long long x = c - '0';
                for (;;) {
                        if (! char_is_digit(look_char()))
                                break;
                        c = read_char();
                        x = 10 * x + c - '0';
                }
                ans = add_integer_token(current_file, off, x);
        }
        else if (c == '(') {
                ans = add_bare_token(current_file, off, TOKTYPE_LEFTPAREN);
        }
        else if (c == ')') {
                ans = add_bare_token(current_file, off, TOKTYPE_RIGHTPAREN);
        }
        else if (c == '{') {
                ans = add_bare_token(current_file, off, TOKTYPE_LEFTBRACE);
        }
        else if (c == '}') {
                ans = add_bare_token(current_file, off, TOKTYPE_RIGHTBRACE);
        }
        else if (c == '[') {
                ans = add_bare_token(current_file, off, TOKTYPE_LEFTBRACKET);
        }
        else if (c == ']') {
                ans = add_bare_token(current_file, off, TOKTYPE_RIGHTBRACKET);
        }
        else if (c == '-') {
                c = look_char();
                if (c == '-') {
                        read_char();
                        ans = add_bare_token(current_file, off,
                                             TOKTYPE_DOUBLEMINUS);
                }
                else {
                        ans = add_bare_token(current_file, off,
                                             TOKTYPE_MINUS);
                }
        }
        else if (c == '+') {
                c = look_char();
                if (c == '+') {
                        read_char();
                        ans = add_bare_token(current_file, off,
                                             TOKTYPE_DOUBLEPLUS);
                }
                else {
                        ans = add_bare_token(current_file, off, TOKTYPE_PLUS);
                }
        }
        else if (c == '*') {
                ans = add_bare_token(current_file, off, TOKTYPE_ASTERISK);
        }
        else if (c == '/') {
                ans = add_bare_token(current_file, off, TOKTYPE_SLASH);
        }
        else if (c == ',') {
                ans = add_bare_token(current_file, off, TOKTYPE_COMMA);
        }
        else if (c == ';') {
                ans = add_bare_token(current_file, off, TOKTYPE_SEMICOLON);
        }
        else if (c == ':') {
                ans = add_bare_token(current_file, off, TOKTYPE_COLON);
        }
        else if (c == '&') {
                ans = add_bare_token(current_file, off, TOKTYPE_AMPERSAND);
        }
        else if (c == '|') {
                ans = add_bare_token(current_file, off, TOKTYPE_PIPE);
        }
        else if (c == '^') {
                ans = add_bare_token(current_file, off, TOKTYPE_CARET);
        }
        else if (c == '!') {
                ans = add_bare_token(current_file, off, TOKTYPE_BANG);
        }
        else if (c == '=') {
                c = look_char();
                if (c == '=') {
                        read_char();
                        ans = add_bare_token(current_file, off,
                                             TOKTYPE_DOUBLEEQUALS);
                }
                else {
                        ans = add_bare_token(current_file, off,
                                             TOKTYPE_ASSIGNEQUALS);
                }
        }
        else {
                PARSE_ERROR_AT(current_file, current_offset,
                               "Failed to lex token\n");
        }

        return ans;
}

Token look_next_token(void)
{
        if (have_saved_token)
                return saved_token;
        else {
                saved_token = parse_next_token();
                have_saved_token = 1;
                return saved_token;
        }
}

Token parse_token_kind(int tkind)
{
        Token tok = parse_next_token();
        if (tok == -1) {
                PARSE_ERROR_AT(current_file, current_offset,
                               "Unexpected end of file. Expected %s token\n",
                               tokenKindString[tkind]);
        }
        int k = tokenInfo[tok].kind;
        if (k != tkind) {
                PARSE_ERROR(tok, "Expected %s token\n", tokenKindString[tkind]);
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

Symbol parse_symbol(void)
{
        PARSE_LOG();

        Token tok = parse_token_kind(TOKTYPE_WORD);
        return add_symbol(tokenInfo[tok].word.string);
}

Type parse_type(void)
{
        PARSE_LOG();

        Symbol sym;

        sym = parse_symbol();
        return add_type(sym);
}

Entity parse_entity(void)
{
        PARSE_LOG();

        Type tp;
        Symbol sym;

        tp = parse_type();
        sym = parse_symbol();
        parse_token_kind(TOKTYPE_SEMICOLON);

        return add_entity(tp, sym);
}

void parse_column(Table table)
{
        PARSE_LOG();

        Type tp;
        Symbol sym;

        tp = parse_type();
        sym = parse_symbol();
        parse_token_kind(TOKTYPE_SEMICOLON);

        add_column(table, tp, sym);
}

void parse_table(void)
{
        PARSE_LOG();

        Token tok;
        Type tp;
        Symbol sym;
        Table table;

        tp = parse_type();
        sym = parse_symbol();

        table = add_table(tp, sym);

        parse_token_kind(TOKTYPE_LEFTBRACE);
        for (;;) {
                tok = look_next_token();
                if (tok == -1)
                        break;
                if (token_is_word(tok, constStr[CONSTSTR_COLUMN])) {
                        parse_column(table);
                }
                else
                        break;
        }
        parse_token_kind(TOKTYPE_RIGHTBRACE);
}

Data parse_data(Scope scope)
{
        PARSE_LOG();

        Type tp;
        Symbol sym;

        tp = parse_type();
        sym = parse_symbol();
        parse_token_kind(TOKTYPE_SEMICOLON);

        return add_data(scope, tp, sym);
}

Expr parse_expr(int minprec)
{
        PARSE_LOG();

        Token tok;
        Expr expr;
        Expr subexpr;
        int opkind;
        int opprec;

        tok = look_next_token();

        if (token_is_unary_prefix_operator(tok, &opkind)) {
                parse_next_token();
                subexpr = parse_expr(42  /* TODO: unop precedence */);
                expr = add_unop_expr(opkind, subexpr);
        }
        else if (tokenInfo[tok].kind == TOKTYPE_WORD) {
                parse_next_token();
                expr = add_symref_expr(tok);
        }
        else if (tokenInfo[tok].kind == TOKTYPE_INTEGER) {
                parse_next_token();
                expr = add_literal_expr(tok);
        }
        else if (tokenInfo[tok].kind == TOKTYPE_LEFTPAREN) {
                parse_next_token();
                expr = parse_expr(0);
                parse_token_kind(TOKTYPE_RIGHTPAREN);
        }
        else {
                PARSE_ERROR(tok, "Expected expression\n");
        }

        for (;;) {
                tok = look_next_token();
                if (token_is_unary_postfix_operator(tok, &opkind)) {
                        parse_next_token();
                        expr = add_unop_expr(opkind, expr);
                }
                else if (tokenInfo[tok].kind == TOKTYPE_LEFTPAREN) {
                        /* function call */
                        parse_next_token();
                        while (look_token_kind(TOKTYPE_RIGHTPAREN) == -1) {
                                subexpr = parse_expr(0);
                                /* TODO: add call argument */
                                if (look_token_kind(TOKTYPE_COMMA) == -1)
                                        break;
                                parse_next_token();
                        }
                        parse_token_kind(TOKTYPE_RIGHTPAREN);
                }
                else {
                        break;
                }
        }

        for (;;) {
                tok = look_next_token();
                if (! token_is_binary_infix_operator(tok, &opkind, &opprec))
                        break;
                if (opprec < minprec)
                        break;
                parse_next_token();
                subexpr = parse_expr(opprec + 1);
                expr = add_binop_expr(opkind, expr, subexpr);
        }

        return expr;
}

void parse_simple_exprstmt(void)
{
        PARSE_LOG();

        parse_expr(0);
        parse_token_kind(TOKTYPE_SEMICOLON);
}

void parse_exprstmt(void);
void parse_stmt(void);

void parse_compound_exprstmt(void)
{
        PARSE_LOG();

        parse_token_kind(TOKTYPE_LEFTBRACE);
        while (look_token_kind(TOKTYPE_RIGHTBRACE) == -1) {
                parse_stmt();
        }
        parse_token_kind(TOKTYPE_RIGHTBRACE);
}

void parse_exprstmt(void)
{
        PARSE_LOG();

        Token tok = look_next_token();

        if (tokenInfo[tok].kind == TOKTYPE_LEFTBRACE) {
                parse_compound_exprstmt();
        }
        else {
                parse_simple_exprstmt();
        }
}

void parse_ifstmt(void)
{
        PARSE_LOG();

        parse_token_kind(TOKTYPE_LEFTPAREN);
        Expr x = parse_expr(0);
        parse_token_kind(TOKTYPE_RIGHTPAREN);
        parse_exprstmt();
}

void parse_whilestmt(void)
{
        PARSE_LOG();
}

void parse_forstmt(void)
{
        PARSE_LOG();
}

void parse_stmt(Scope scope)
{
        PARSE_LOG();

        Token tok;
       
        tok = look_next_token();

        if (tokenInfo[tok].kind == TOKTYPE_WORD) {
                String s = tokenInfo[tok].word.string;
                if (s == constStr[CONSTSTR_DATA]) {
                        parse_next_token();
                        parse_data(scope);
                }
                else if (s == constStr[CONSTSTR_IF]) {
                        parse_next_token();
                        parse_ifstmt();
                }
                else if (s == constStr[CONSTSTR_WHILE]) {
                        parse_next_token();
                        parse_whilestmt();
                }
                else if (s == constStr[CONSTSTR_FOR]) {
                        parse_next_token();
                        parse_forstmt();
                }
                else {
                        parse_exprstmt();
                }
        }
        else {
                parse_exprstmt();
        }
}

void parse_proc_body(Proc proc)
{
        PARSE_LOG();
        parse_compound_exprstmt();
}

void parse_proc(void)
{
        PARSE_LOG();

        Type rettp;  /* out-arg type */
        Type argtp;  /* in-arg type */
        Symbol argsym;  /* in-arg name */
        Symbol psym;  /* proc name */
        Proc proc;
        Token tok;

        rettp = parse_type();
        psym = parse_symbol();
        proc = add_proc(rettp, psym);

        parse_token_kind(TOKTYPE_LEFTPAREN);
        for (;;) {
                tok = look_next_token();
                if (tokenInfo[tok].kind == TOKTYPE_RIGHTPAREN)
                        break;
                argtp = parse_type();
                argsym = parse_symbol();
                add_procarg(proc, argtp, argsym);
                tok = look_next_token();
                if (tokenInfo[tok].kind != TOKTYPE_COMMA)
                        break;
        }
        parse_token_kind(TOKTYPE_RIGHTPAREN);

        parse_proc_body(proc);
}

void parse_global_scope(void)
{
        PARSE_LOG();

        Token tok;
        String s;

        for (;;) {
                tok = look_next_token();
                if (tok == -1)
                        break;
                parse_token_kind(TOKTYPE_WORD);
                s = tokenInfo[tok].word.string;
                if (s == constStr[CONSTSTR_ENTITY]) {
                        parse_entity();
                }
                else if (s == constStr[CONSTSTR_TABLE]) {
                        parse_table();
                }
                else if (s == constStr[CONSTSTR_DATA]) {
                        parse_data(42 /*global_scope*/);
                }
                else if (s == constStr[CONSTSTR_PROC]) {
                        parse_proc();
                }
                else {
                        PARSE_ERROR(tok, "Unexpected word %s\n", TS(tok));
                }
        }
}

int main(void)
{
        init_strings();
        add_file(intern_cstring("test.txt"));
        parse_global_scope();
        return 0;
}
