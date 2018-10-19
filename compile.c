#include "defs.h"
#include "api.h"

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
        mem_copy(&strbuf[pos], buf, len);
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
                    mem_compare(string_buffer(s), buf, len) == 0)
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
                        hsh = hash_string(string_buffer(s), string_length(s));
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
        return intern_string((const void *)str, cstr_length(str));
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
        columnInfo[x].table = table;
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

Scope add_global_scope(void)
{
        Scope x = scopeCnt++;
        BUF_RESERVE(scopeInfo, scopeInfoAlloc, scopeCnt);
        scopeInfo[x].kind = SCOPE_GLOBAL;
        return x;
}

Scope add_proc_scope(Scope parent)
{
        Scope x = scopeCnt++;
        BUF_RESERVE(scopeInfo, scopeInfoAlloc, scopeCnt);
        scopeInfo[x].kind = SCOPE_PROC;
        scopeInfo[x].tProc.parentScope = parent;
        return x;
}

Proc add_proc(Type tp, Symbol sym, Scope scope)
{
        Proc x = procCnt++;
        BUF_RESERVE(procInfo, procInfoAlloc, procCnt);
        procInfo[x].tp = tp;
        procInfo[x].sym = sym;
        procInfo[x].scope = scope;
        procInfo[x].nargs = 0;
        procInfo[x].body = -1;
        return x;
}

void add_procarg(Proc proc, Type argtp, Symbol argsym)
{
        ProcArg x = procArgCnt++;
        BUF_RESERVE(procArgInfo, procArgInfoAlloc, procArgCnt);
        procArgInfo[x].proc = proc;
        procArgInfo[x].tp = argtp;
        procArgInfo[x].sym = argsym;
        procArgInfo[x].argIdx = procInfo[proc].nargs;

        if (procInfo[proc].nargs == 0)
                procInfo[proc].firstArg = x;
        procInfo[proc].nargs++;
}

Expr add_symref_expr(Token UNUSED tok)
{
        return -1;
}

Expr add_literal_expr(Token UNUSED tok)
{
        return -1;
}

Expr add_call_expr(Expr UNUSED callee)
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
        exprInfo[x].binop.expr2 = expr2;
        return x;
}

Stmt add_data_stmt(void)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_DATA;
        return stmt;
}

Stmt add_if_stmt(Expr condExpr, Stmt childStmt)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_IF;
        stmtInfo[stmt].tIf.condExpr = condExpr;
        stmtInfo[stmt].tIf.childStmt = childStmt;
        return stmt;
}

Stmt add_while_stmt(Expr condExpr, Stmt childStmt)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_WHILE;
        stmtInfo[stmt].tWhile.condExpr = condExpr;
        stmtInfo[stmt].tWhile.childStmt = childStmt;
        return stmt;
}

Stmt add_for_stmt(Stmt initStmt, Expr condExpr, Stmt stepStmt, Stmt childStmt)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_FOR;
        stmtInfo[stmt].tFor.initStmt = initStmt;
        stmtInfo[stmt].tFor.condExpr = condExpr;
        stmtInfo[stmt].tFor.stepStmt = stepStmt;
        stmtInfo[stmt].tFor.childStmt = childStmt;
        return stmt;
}

Stmt add_expr_stmt(void)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_EXPR;
        return stmt;
}

Stmt add_compound_stmt(void)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_COMPOUND;
        stmtInfo[stmt].tCompound.numStatements = 0;
        stmtInfo[stmt].tCompound.firstStmt = -1;
        return stmt;
}

Stmt add_substmt(Stmt stmt, Stmt substmt)
{
        assert(stmtInfo[stmt].kind == STMT_COMPOUND);
        if (stmtInfo[stmt].tCompound.numStatements == 0)
                stmtInfo[stmt].tCompound.firstStmt = substmt;
        stmtInfo[stmt].tCompound.numStatements++;
        return stmt;
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

        for (int i = 0; i < LENGTH(tbl); i++) {
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
                        *out_optp = tbl[i].optp;
                        *out_prec = tbl[i].prec;
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
                if (current_offset < fileInfo[current_file].size) {
                        have_saved_char = 1;
                        saved_char = fileInfo[current_file].buf[current_offset];
                }
                else {
                        saved_char = -1;
                }
        }
        return saved_char;
}

/* offset may be 1 past the end of file (i.e., equal to file size) */
int compute_lineno(File file, int offset)
{
        int i;
        int line = 1;

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

#define PARSE_ERROR_AT(file, offset, mesg, ...) \
        FATAL("At %s %d:%d: " mesg, \
              string_buffer(fileInfo[file].filepath), \
              compute_lineno(file, offset), \
              compute_colno(file, offset), \
              ##__VA_ARGS__)
#define PARSE_ERROR(tok, mesg, ...) \
        FATAL("At %s %d:%d: ERROR parsing %s token. " mesg, \
              string_buffer(fileInfo[tokenInfo[tok].file].filepath), \
              compute_lineno(tokenInfo[tok].file, tokenInfo[tok].offset), \
              compute_colno(tokenInfo[tok].file, tokenInfo[tok].offset), \
              tokenKindString[tokenInfo[tok].kind], \
              ##__VA_ARGS__)
#define PARSE_LOG() \
        msg("AT %s() at %d:%d\n", \
            __func__, \
            compute_lineno(current_file, current_offset), \
            compute_colno(current_file, current_offset))

void push_scope(Scope scope)
{
        if (scope_stack_count >= LENGTH(scope_stack))
                PARSE_ERROR_AT(current_file, current_offset,
                               "Maximum scope nesting depth reached\n");
        scope_stack[scope_stack_count++] = scope;
        current_scope = scope;
}

void pop_scope(void)
{
        assert(scope_stack_count > 1);
        scope_stack_count--;
        current_scope = scope_stack[scope_stack_count-1];
}

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
                        lexbuf[idx] = (char) c;
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

Data parse_data(void)
{
        PARSE_LOG();

        Type tp;
        Symbol sym;

        tp = parse_type();
        sym = parse_symbol();
        parse_token_kind(TOKTYPE_SEMICOLON);

        return add_data(current_scope, tp, sym);
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

Stmt parse_expr_stmt(void)
{
        PARSE_LOG();

        parse_expr(0);
        parse_token_kind(TOKTYPE_SEMICOLON);
        return add_expr_stmt();
}

Stmt parse_expr_or_compound_stmt(void);
Stmt parse_stmt(void);

Stmt parse_compound_stmt(void)
{
        PARSE_LOG();

        Stmt stmt;
        Stmt substmt;
        
        stmt = add_compound_stmt();
        parse_token_kind(TOKTYPE_LEFTBRACE);
        while (look_token_kind(TOKTYPE_RIGHTBRACE) == -1) {
                substmt = parse_stmt();
                add_substmt(stmt, substmt);
        }
        parse_token_kind(TOKTYPE_RIGHTBRACE);
        return stmt;
}

Stmt parse_expr_or_compound_stmt(void)
{
        PARSE_LOG();

        Token tok = look_next_token();

        if (tokenInfo[tok].kind == TOKTYPE_LEFTBRACE) {
                return parse_compound_stmt();
        }
        else {
                return parse_expr_stmt();
        }
}

Stmt parse_ifstmt(void)
{
        PARSE_LOG();

        Stmt condExpr;
        Stmt childStmt;

        parse_token_kind(TOKTYPE_LEFTPAREN);
        condExpr = parse_expr(0);
        parse_token_kind(TOKTYPE_RIGHTPAREN);
        childStmt = parse_expr_or_compound_stmt();
        return add_if_stmt(condExpr, childStmt);
}

Stmt parse_while_stmt(void)
{
        PARSE_LOG();

        Expr condExpr;
        Stmt childStmt;

        parse_token_kind(TOKTYPE_LEFTPAREN);
        condExpr = parse_expr(0);
        parse_token_kind(TOKTYPE_RIGHTPAREN);
        childStmt = parse_expr_or_compound_stmt();
        return add_while_stmt(condExpr, childStmt);
}

Stmt parse_for_stmt(void)
{
        PARSE_LOG();

        Stmt initStmt;
        Expr condExpr;
        Stmt stepStmt;
        Stmt childStmt;

        parse_token_kind(TOKTYPE_LEFTPAREN);
        initStmt = parse_expr_stmt();
        parse_token_kind(TOKTYPE_SEMICOLON);
        condExpr = parse_expr(0);
        parse_token_kind(TOKTYPE_SEMICOLON);
        stepStmt = parse_expr_stmt();
        parse_token_kind(TOKTYPE_RIGHTPAREN);
        childStmt = parse_expr_or_compound_stmt();
        return add_for_stmt(initStmt, condExpr, stepStmt, childStmt);
}

Stmt parse_stmt(void)
{
        PARSE_LOG();

        Token tok;
       
        tok = look_next_token();
        if (tokenInfo[tok].kind == TOKTYPE_WORD) {
                String s = tokenInfo[tok].word.string;
                if (s == constStr[CONSTSTR_DATA]) {
                        parse_next_token();
                        parse_data();
                }
                else if (s == constStr[CONSTSTR_IF]) {
                        parse_next_token();
                        parse_ifstmt();
                }
                else if (s == constStr[CONSTSTR_WHILE]) {
                        parse_next_token();
                        parse_while_stmt();
                }
                else if (s == constStr[CONSTSTR_FOR]) {
                        parse_next_token();
                        parse_for_stmt();
                }
                else {
                        parse_expr_stmt();
                }
        }
        else {
                parse_expr_stmt();
        }
}

Stmt parse_proc_body(Proc proc)
{
        PARSE_LOG();
        return parse_compound_stmt();
}

void parse_proc(void)
{
        PARSE_LOG();

        Type rettp;  /* out-arg type */
        Type argtp;  /* in-arg type */
        Symbol argsym;  /* in-arg name */
        Symbol psym;  /* proc name */
        Scope pscope; /* proc scope */
        Proc proc;
        Token tok;
        Stmt body;

        rettp = parse_type();
        psym = parse_symbol();
        pscope = add_proc_scope(current_scope);
        proc = add_proc(rettp, psym, pscope);
        scopeInfo[pscope].tProc.proc = proc;

        push_scope(pscope);

        parse_token_kind(TOKTYPE_LEFTPAREN);
        for (;;) {
                tok = look_next_token();
                if (tokenInfo[tok].kind == TOKTYPE_RIGHTPAREN)
                        break;
                argtp = parse_type();
                argsym = parse_symbol();
                add_procarg(proc, argtp, argsym);
                if (look_token_kind(TOKTYPE_COMMA) == -1)
                        break;
                parse_next_token();
        }
        parse_token_kind(TOKTYPE_RIGHTPAREN);

        body = parse_proc_body(proc);
        procInfo[proc].body = body;

        pop_scope();
}

void parse_global_scope(void)
{
        PARSE_LOG();

        Token tok;
        String s;

        global_scope = add_global_scope();
        push_scope(global_scope);

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
                        parse_data();
                }
                else if (s == constStr[CONSTSTR_PROC]) {
                        parse_proc();
                }
                else {
                        PARSE_ERROR(tok, "Unexpected word %s\n", TS(tok));
                }
        }
}

void pprint_type(Type tp)
{
        msg("%s", SS(typeInfo[tp].sym));
}

void pprint_data(Data d)
{
        msg("data ");
        pprint_type(dataInfo[d].tp);
        msg(" ");
        msg("%s", SS(dataInfo[d].sym));
        msg(";\n");
}

void pprint_expr(Expr expr)
{
        msg("EXPR");
}

void pprint_expr_stmt(Expr expr)
{
        pprint_expr(expr);
        msg(";\n");
}

void pprint_compound_stmt(Stmt stmt)
{
        msg("{\n");
        msg("    COMPOUND\n");
        msg("}\n");
}

void pprint_stmt(Stmt stmt)
{
        switch (stmtInfo[stmt].kind) {
        case STMT_IF:
                msg("if (");
                pprint_expr(stmtInfo[stmt].tIf.condExpr);
                msg(")\n");
                pprint_stmt(stmtInfo[stmt].tIf.childStmt);
                break;
        case STMT_FOR:
                msg("for (");
                pprint_stmt(stmtInfo[stmt].tFor.initStmt);
                msg("; ");
                pprint_expr(stmtInfo[stmt].tFor.condExpr);
                msg("; ");
                pprint_expr_stmt(stmtInfo[stmt].tFor.stepStmt);
                msg(")\n");
                pprint_stmt(stmtInfo[stmt].tFor.childStmt);
                break;
        case STMT_WHILE:
                msg("if (");
                pprint_expr(stmtInfo[stmt].tWhile.condExpr);
                msg(")\n");
                pprint_stmt(stmtInfo[stmt].tWhile.childStmt);
                break;
        case STMT_EXPR:
                pprint_expr_stmt(stmt);
                break;
        case STMT_COMPOUND:
                pprint_compound_stmt(stmt);
                break;
        }
}

void pprint_proc(Proc p)
{
        msg("proc ");
        pprint_type(procInfo[p].tp);
        msg(" ");
        msg("%s", SS(procInfo[p].sym));
        msg("(");
        int firstArg = procInfo[p].firstArg;
        for (int i = 0; i < procInfo[p].nargs; i++) {
                if (i > 0)
                        msg(", ");
                pprint_type(procArgInfo[firstArg+i].tp);
                msg(" %s", SS(procArgInfo[firstArg+i].sym));
        }
        msg(")");
        msg(";\n");

        int nst = stmtInfo[procInfo[p].body].tCompound.numStatements;
        msg("%d sub-statements\n", nst);
        for (int i = 0; i < nst; i++)
                pprint_stmt(stmtInfo[procInfo[p].body].tCompound.firstStmt + i);
}

void prettyprint(void)
{
        for (Data i = 0; i < dataCnt; i++) {
                //if (scopeInfo[dataInfo[i].scope].kind == global_scope) {
                if (i[dataInfo].scope[scopeInfo].kind == global_scope) {
                        pprint_data(i);
                }
        }
        for (Proc i = 0; i < procCnt; i++) {
                pprint_proc(i);
        }
}

int main(void)
{
        init_strings();
        add_file(intern_cstring("test.txt"));
        parse_global_scope();
        prettyprint();
        return 0;
}
