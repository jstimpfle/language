#include "defs.h"
#include "api.h"

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

Symbol add_symbol(int kind, String name, Scope scope)
{
        Symbol x = symbolCnt++;
        BUF_RESERVE(symbolInfo, symbolInfoAlloc, symbolCnt);
        symbolInfo[x].kind = kind;
        symbolInfo[x].name = name;
        symbolInfo[x].scope = scope;
        return x;
}

Entity add_entity(Typeref tref, Symbol sym)
{
        Entity x = entityCnt++;
        BUF_RESERVE(entityInfo, entityInfoAlloc, entityCnt);
        entityInfo[x].tref = tref;
        entityInfo[x].sym = sym;
        return x;
}

Table add_table(Typeref tref, Symbol sym)
{
        Table x = tableCnt++;
        BUF_RESERVE(tableInfo, tableInfoAlloc, tableCnt);
        tableInfo[x].tref = tref;
        tableInfo[x].sym = sym;
        tableInfo[x].firstColumn = -1;
        tableInfo[x].numColumns = 0;
        return x;
}

Column add_column(Table table, Typeref tref, Symbol sym)
{
        Column x = columnCnt++;
        BUF_RESERVE(columnInfo, columnInfoAlloc, columnCnt);
        columnInfo[x].table = table;
        columnInfo[x].tref = tref;
        columnInfo[x].sym = sym;
        columnInfo[x].rank = x;
        return x;
}

Data add_data(Scope scope, Typeref tref, Symbol sym)
{
        Data x = dataCnt++;
        BUF_RESERVE(dataInfo, dataInfoAlloc, dataCnt);
        dataInfo[x].scope = scope;
        dataInfo[x].tref = tref;
        dataInfo[x].sym = sym;
        return x;
}

Scope add_global_scope(void)
{
        Scope x = scopeCnt++;
        BUF_RESERVE(scopeInfo, scopeInfoAlloc, scopeCnt);
        scopeInfo[x].parentScope = -1;
        scopeInfo[x].firstSymbol = -1;
        scopeInfo[x].numSymbols = 0;
        scopeInfo[x].kind = SCOPE_GLOBAL;
        return x;
}

Scope add_proc_scope(Scope parent)
{
        Scope x = scopeCnt++;
        BUF_RESERVE(scopeInfo, scopeInfoAlloc, scopeCnt);
        scopeInfo[x].parentScope = parent;
        scopeInfo[x].firstSymbol = -1;
        scopeInfo[x].numSymbols = 0;
        scopeInfo[x].kind = SCOPE_PROC;
        return x;
}

Proc add_proc(Typeref tref, Symbol sym, Scope scope)
{
        Proc x = procCnt++;
        BUF_RESERVE(procInfo, procInfoAlloc, procCnt);
        procInfo[x].tref = tref;
        procInfo[x].sym = sym;
        procInfo[x].scope = scope;
        procInfo[x].firstParam = -1;
        procInfo[x].nparams = 0;
        procInfo[x].body = -1;
        return x;
}

void add_procarg(Proc proc, Typeref argtref, Symbol argsym)
{
        ProcParam x = procParamCnt++;
        BUF_RESERVE(procParamInfo, procParamInfoAlloc, procParamCnt);
        procParamInfo[x].proc = proc;
        procParamInfo[x].tref = argtref;
        procParamInfo[x].sym = argsym;
        procParamInfo[x].rank = x;
}

Symref add_symref(Token tok, Scope refScope)
{
        Symref ref = symrefCnt++;
        BUF_RESERVE(symrefInfo, symrefInfoAlloc, symrefCnt);
        symrefInfo[ref].name = tokenInfo[tok].word.string;
        symrefInfo[ref].refScope = refScope;
        symrefInfo[ref].tok = tok;
        return ref;
}

Expr add_symref_expr(Symref ref)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_SYMREF;
        exprInfo[x].tSymref = ref;
        return x;
}

Expr add_literal_expr(Token tok)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_LITERAL;
        exprInfo[x].tLiteral.tok = tok;
        return x;
}

Expr add_call_expr(Expr callee)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_CALL;
        exprInfo[x].tCall.callee = callee;
        exprInfo[x].tCall.firstArgIdx = -1;
        exprInfo[x].tCall.nargs = 0;
        return x;
}

Expr add_unop_expr(int opkind, Expr expr)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_UNOP;
        exprInfo[x].tUnop.kind = opkind;
        exprInfo[x].tUnop.expr = expr;
        return x;
}

Expr add_binop_expr(int opkind, Expr expr1, Expr expr2)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_BINOP;
        exprInfo[x].tBinop.kind = opkind;
        exprInfo[x].tBinop.expr1 = expr1;
        exprInfo[x].tBinop.expr2 = expr2;
        return x;
}

Stmt add_data_stmt(Data data)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_DATA;
        stmtInfo[stmt].tData.data = data;
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

Stmt add_return_stmt(Expr expr)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_RETURN;
        stmtInfo[stmt].tReturn.expr = expr;
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

Stmt add_expr_stmt(Expr expr)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_EXPR;
        stmtInfo[stmt].tExpr.expr = expr;
        return stmt;
}

Stmt add_compound_stmt(void)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_COMPOUND;
        stmtInfo[stmt].tCompound.numStatements = 0;
        stmtInfo[stmt].tCompound.firstChildStmtIdx = -1;
        return stmt;
}

void add_ChildStmt(Stmt parent, Stmt child)
{
        int x = childStmtCnt++;
        BUF_RESERVE(childStmtInfo, childStmtInfoAlloc, childStmtCnt);
        childStmtInfo[x].parent = parent;
        childStmtInfo[x].child = child;
        childStmtInfo[x].rank = x;
}

void add_CallArg(Expr callExpr, Expr argExpr)
{
        int x = callArgCnt++;
        BUF_RESERVE(callArgInfo, callArgInfoAlloc, callArgCnt);
        callArgInfo[x].callExpr = callExpr;
        callArgInfo[x].argExpr = argExpr;
        callArgInfo[x].rank = x;
}

void init_strings(void)
{
        for (int i = 0; i < LENGTH(stringsToBeInterned); i++) {
                int idx = stringsToBeInterned[i].constant;
                const char *str = stringsToBeInterned[i].string;
                constStr[idx] = intern_cstring(str);
        }
}

void init_basetypes(void)
{
        basetypeCnt = basetypesToBeInitializedCnt;
        BUF_RESERVE(basetypeInfo, basetypeInfoAlloc, basetypeCnt);
        for (int i = 0; i < basetypesToBeInitializedCnt; i++) {
                Basetype x = i;
                int size = basetypesToBeInitialized[i].size;
                String name = intern_cstring(basetypesToBeInitialized[i].name);
                basetypeInfo[x].size = size;
                add_symbol(SYMBOL_TYPE, name, globalScope);
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

int token_is_unary_prefix_operator(Token tok, int *out_optype)
{
        int tp = tokenInfo[tok].kind;
        for (int i = 0; i < toktypeToPrefixUnopCnt; i++) {
                if (tp == toktypeToPrefixUnop[i].ttype) {
                        *out_optype = toktypeToPrefixUnop[i].optype;
                        return 1;
                }
        }
        return 0;
}

int token_is_unary_postfix_operator(Token tok, int *out_optype)
{
        int tp = tokenInfo[tok].kind;
        for (int i = 0; i < toktypeToPostfixUnopCnt; i++) {
                if (tp == toktypeToPostfixUnop[i].ttype) {
                        *out_optype = toktypeToPostfixUnop[i].optype;
                        return 1;
                }
        }
        return 0;
}

int token_is_binary_infix_operator(Token tok, int *out_optp)
{
        int tp = tokenInfo[tok].kind;
        for (int i = 0; i < toktypeToBinopCnt; i++) {
                if (tp == toktypeToBinop[i].ttype) {
                        *out_optp = toktypeToBinop[i].optype;
                        return 1;
                }
        }
        return 0;
}

int read_char(void)
{
        if (haveSavedChar) {
                haveSavedChar = 0;
                currentOffset++;
                return savedChar;
        }
        else if (currentOffset < fileInfo[currentFile].size) {
                int pos = currentOffset++;
                return fileInfo[currentFile].buf[pos];
        }
        else {
                return -1;
        }
}

int look_char(void)
{
        if (! haveSavedChar) {
                if (currentOffset < fileInfo[currentFile].size) {
                        haveSavedChar = 1;
                        savedChar = fileInfo[currentFile].buf[currentOffset];
                }
                else {
                        savedChar = -1;
                }
        }
        return savedChar;
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
        if (doDebug) \
                msg("AT %s() at %d:%d\n", \
                    __func__, \
                    compute_lineno(currentFile, currentOffset), \
                    compute_colno(currentFile, currentOffset))

void push_scope(Scope scope)
{
        if (scopeStackCnt >= LENGTH(scopeStack))
                PARSE_ERROR_AT(currentFile, currentOffset,
                               "Maximum scope nesting depth reached\n");
        scopeStack[scopeStackCnt++] = scope;
        currentScope = scope;
}

void pop_scope(void)
{
        assert(scopeStackCnt > 1);
        scopeStackCnt--;
        currentScope = scopeStack[scopeStackCnt-1];
}

Token parse_next_token(void)
{
        int c;
        int off;
        Token ans;

        if (haveSavedToken) {
                haveSavedToken = 0;
                return savedToken;
        }

        for (;;) {
                c = look_char();
                if (c == -1)
                        return -1;
                if (char_is_invalid(c))
                        PARSE_ERROR_AT(currentFile, currentOffset,
                                       "Invalid byte %d\n", c);
                if (! char_is_whitespace(c))
                        break;
                read_char();
        }

        off = currentOffset;

        c = read_char();
        if (char_is_alpha(c)) {
                lexbufCnt = 0;
                for (;;) {
                        int idx = lexbufCnt++;
                        BUF_RESERVE(lexbuf, lexbufAlloc, lexbufCnt);
                        lexbuf[idx] = (char) c;
                        c = look_char();
                        if (! char_is_alpha(c) && ! char_is_digit(c))
                                break;
                        read_char();
                }
                ans = add_word_token(currentFile, off, lexbuf, lexbufCnt);
        }
        else if (char_is_digit(c)) {
                long long x = c - '0';
                for (;;) {
                        c = look_char();
                        if (! char_is_digit(c))
                                break;
                        read_char();
                        x = 10 * x + c - '0';
                }
                ans = add_integer_token(currentFile, off, x);
        }
        else if (c == '(') {
                ans = add_bare_token(currentFile, off, TOKTYPE_LEFTPAREN);
        }
        else if (c == ')') {
                ans = add_bare_token(currentFile, off, TOKTYPE_RIGHTPAREN);
        }
        else if (c == '{') {
                ans = add_bare_token(currentFile, off, TOKTYPE_LEFTBRACE);
        }
        else if (c == '}') {
                ans = add_bare_token(currentFile, off, TOKTYPE_RIGHTBRACE);
        }
        else if (c == '[') {
                ans = add_bare_token(currentFile, off, TOKTYPE_LEFTBRACKET);
        }
        else if (c == ']') {
                ans = add_bare_token(currentFile, off, TOKTYPE_RIGHTBRACKET);
        }
        else if (c == '-') {
                c = look_char();
                if (c == '-') {
                        read_char();
                        ans = add_bare_token(currentFile, off,
                                             TOKTYPE_DOUBLEMINUS);
                }
                else {
                        ans = add_bare_token(currentFile, off,
                                             TOKTYPE_MINUS);
                }
        }
        else if (c == '+') {
                c = look_char();
                if (c == '+') {
                        read_char();
                        ans = add_bare_token(currentFile, off,
                                             TOKTYPE_DOUBLEPLUS);
                }
                else {
                        ans = add_bare_token(currentFile, off, TOKTYPE_PLUS);
                }
        }
        else if (c == '*') {
                ans = add_bare_token(currentFile, off, TOKTYPE_ASTERISK);
        }
        else if (c == '/') {
                ans = add_bare_token(currentFile, off, TOKTYPE_SLASH);
        }
        else if (c == ',') {
                ans = add_bare_token(currentFile, off, TOKTYPE_COMMA);
        }
        else if (c == ';') {
                ans = add_bare_token(currentFile, off, TOKTYPE_SEMICOLON);
        }
        else if (c == ':') {
                ans = add_bare_token(currentFile, off, TOKTYPE_COLON);
        }
        else if (c == '&') {
                ans = add_bare_token(currentFile, off, TOKTYPE_AMPERSAND);
        }
        else if (c == '|') {
                ans = add_bare_token(currentFile, off, TOKTYPE_PIPE);
        }
        else if (c == '^') {
                ans = add_bare_token(currentFile, off, TOKTYPE_CARET);
        }
        else if (c == '!') {
                ans = add_bare_token(currentFile, off, TOKTYPE_BANG);
        }
        else if (c == '=') {
                c = look_char();
                if (c == '=') {
                        read_char();
                        ans = add_bare_token(currentFile, off,
                                             TOKTYPE_DOUBLEEQUALS);
                }
                else {
                        ans = add_bare_token(currentFile, off,
                                             TOKTYPE_ASSIGNEQUALS);
                }
        }
        else {
                PARSE_ERROR_AT(currentFile, currentOffset,
                               "Failed to lex token\n");
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
                PARSE_ERROR_AT(currentFile, currentOffset,
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

Symbol parse_symbol(int kind)
{
        PARSE_LOG();
        Token tok = parse_token_kind(TOKTYPE_WORD);
        return add_symbol(kind, tokenInfo[tok].word.string, currentScope);
}

Symref parse_symref(void)
{
        PARSE_LOG();
        Token tok = parse_token_kind(TOKTYPE_WORD);
        return add_symref(tok, currentScope);
}

Typeref parse_typeref(void)
{
        PARSE_LOG();
        return parse_symref(); // XXX
}

Entity parse_entity(void)
{
        Typeref tref;
        Symbol sym;

        PARSE_LOG();
        tref = parse_typeref();
        sym = parse_symbol(SYMBOL_TYPE);
        parse_token_kind(TOKTYPE_SEMICOLON);

        return add_entity(tref, sym);
}

void parse_column(Table table)
{
        Typeref tref;
        Symbol sym;

        PARSE_LOG();
        tref = parse_typeref();
        sym = parse_symbol(SYMBOL_LOCALDATA /*TODO: other kind?*/);
        parse_token_kind(TOKTYPE_SEMICOLON);

        add_column(table, tref, sym);
}

void parse_table(void)
{
        Token tok;
        Typeref tref;
        Symbol sym;
        Table table;

        PARSE_LOG();
        tref = parse_typeref();
        sym = parse_symbol(SYMBOL_TYPE);

        table = add_table(tref, sym);

        parse_token_kind(TOKTYPE_LEFTBRACE);
        for (;;) {
                tok = look_next_token();
                if (tok == -1)
                        break;
                if (token_is_word(tok, constStr[CONSTSTR_COLUMN])) {
                        parse_next_token();
                        parse_column(table);
                }
                else
                        break;
        }
        parse_token_kind(TOKTYPE_RIGHTBRACE);
}

Data parse_data(void)
{
        Typeref tref;
        Symbol sym;

        PARSE_LOG();
        tref = parse_typeref();
        sym = parse_symbol(SYMBOL_DATA);
        parse_token_kind(TOKTYPE_SEMICOLON);
        return add_data(currentScope, tref, sym);
}

Expr parse_expr(int minprec)
{
        Token tok;
        Expr expr;
        Expr subexpr;
        int opkind;
        int opprec;

        PARSE_LOG();
        tok = look_next_token();
        if (token_is_unary_prefix_operator(tok, &opkind)) {
                parse_next_token();
                subexpr = parse_expr(42  /* TODO: unop precedence */);
                expr = add_unop_expr(opkind, subexpr);
        }
        else if (tokenInfo[tok].kind == TOKTYPE_WORD) {
                Symref ref = parse_symref();
                expr = add_symref_expr(ref);
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
                        expr = add_call_expr(expr);
                        while (look_token_kind(TOKTYPE_RIGHTPAREN) == -1) {
                                subexpr = parse_expr(0);
                                add_CallArg(expr, subexpr);
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
                if (! token_is_binary_infix_operator(tok, &opkind))
                        break;
                opprec = binopInfo[opkind].prec;
                if (opprec < minprec)
                        break;
                parse_next_token();
                subexpr = parse_expr(opprec + 1);
                expr = add_binop_expr(opkind, expr, subexpr);
        }

        return expr;
}

Stmt parse_data_stmt(void)
{
        Data data = parse_data();
        return add_data_stmt(data);
}

Stmt parse_expr_stmt(void)
{
        Expr expr;

        PARSE_LOG();
        expr = parse_expr(0);
        parse_token_kind(TOKTYPE_SEMICOLON);
        return add_expr_stmt(expr);
}

Stmt parse_expr_or_compound_stmt(void);
Stmt parse_stmt(void);

Stmt parse_compound_stmt(void)
{
        Stmt stmt;
        Stmt substmt;

        PARSE_LOG();
        stmt = add_compound_stmt();
        parse_token_kind(TOKTYPE_LEFTBRACE);
        while (look_token_kind(TOKTYPE_RIGHTBRACE) == -1) {
                substmt = parse_stmt();
                add_ChildStmt(stmt, substmt);
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
        Stmt condExpr;
        Stmt childStmt;

        PARSE_LOG();
        parse_token_kind(TOKTYPE_LEFTPAREN);
        condExpr = parse_expr(0);
        parse_token_kind(TOKTYPE_RIGHTPAREN);
        childStmt = parse_expr_or_compound_stmt();
        return add_if_stmt(condExpr, childStmt);
}

Stmt parse_while_stmt(void)
{
        Expr condExpr;
        Stmt childStmt;

        PARSE_LOG();
        parse_token_kind(TOKTYPE_LEFTPAREN);
        condExpr = parse_expr(0);
        parse_token_kind(TOKTYPE_RIGHTPAREN);
        childStmt = parse_expr_or_compound_stmt();
        return add_while_stmt(condExpr, childStmt);
}

Stmt parse_for_stmt(void)
{
        Stmt initStmt;
        Expr condExpr;
        Stmt stepStmt;
        Stmt childStmt;

        PARSE_LOG();
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

Stmt parse_return_stmt(void)
{
        PARSE_LOG();
        Expr expr = parse_expr(0);
        parse_token_kind(TOKTYPE_SEMICOLON);
        return add_return_stmt(expr);
}

Stmt parse_stmt(void)
{
        Token tok;

        PARSE_LOG();
        tok = look_next_token();
        if (tokenInfo[tok].kind == TOKTYPE_WORD) {
                String s = tokenInfo[tok].word.string;
                if (s == constStr[CONSTSTR_DATA]) {
                        parse_next_token();
                        return parse_data_stmt();
                }
                else if (s == constStr[CONSTSTR_IF]) {
                        parse_next_token();
                        return parse_ifstmt();
                }
                else if (s == constStr[CONSTSTR_WHILE]) {
                        parse_next_token();
                        return parse_while_stmt();
                }
                else if (s == constStr[CONSTSTR_FOR]) {
                        parse_next_token();
                        return parse_for_stmt();
                }
                else if (s == constStr[CONSTSTR_RETURN]) {
                        parse_next_token();
                        return parse_return_stmt();
                }
                else {
                        return parse_expr_stmt();
                }
        }
        else {
                return parse_expr_stmt();
        }
}

void parse_proc(void)
{
        Typeref rettref;  /* out-arg type */
        Typeref argtref;  /* in-arg type */
        Symbol argsym;  /* in-arg name */
        Symbol psym;  /* proc name */
        Scope pscope; /* proc scope */
        Proc proc;
        Token tok;
        Stmt body;

        PARSE_LOG();
        rettref = parse_typeref();
        psym = parse_symbol(SYMBOL_PROC);
        pscope = add_proc_scope(currentScope);
        proc = add_proc(rettref, psym, pscope);
        scopeInfo[pscope].tProc.proc = proc;

        push_scope(pscope);
        parse_token_kind(TOKTYPE_LEFTPAREN);
        for (;;) {
                tok = look_next_token();
                if (tokenInfo[tok].kind == TOKTYPE_RIGHTPAREN)
                        break;
                argtref = parse_typeref();
                argsym = parse_symbol(SYMBOL_LOCALDATA /*TODO: new kind?*/);
                add_procarg(proc, argtref, argsym);
                if (look_token_kind(TOKTYPE_COMMA) == -1)
                        break;
                parse_next_token();
        }
        parse_token_kind(TOKTYPE_RIGHTPAREN);
        body = parse_compound_stmt();
        procInfo[proc].body = body;
        pop_scope();
}

int compare_Symbol(const void *a, const void *b)
{
        const Symbol *x = a;
        const Symbol *y = b;
        return symbolInfo[*x].scope - symbolInfo[*y].scope;
}

int compare_ColumnInfo(const void *a, const void *b)
{
        const struct ColumnInfo *x = a;
        const struct ColumnInfo *y = b;
        if (x->table != y->table)
                return x->table - y->table;
        return x->rank - y->rank;
}

int compare_ProcParamInfo(const void *a, const void *b)
{
        const struct ProcParamInfo *x = a;
        const struct ProcParamInfo *y = b;
        if (x->proc != y->proc)
                return x->proc - y->proc;
        return x->rank - y->rank;
}

int compare_ChildStmtInfo(const void *a, const void *b)
{
        const struct ChildStmtInfo *x = a;
        const struct ChildStmtInfo *y = b;
        if (x->parent != y->parent)
                return x->parent - y->parent;
        return x->rank - y->rank;
}

int compare_CallArgInfo(const void *a, const void *b)
{
        const struct CallArgInfo *x = a;
        const struct CallArgInfo *y = b;
        if (x->callExpr != y->callExpr)
                return x->callExpr - y->callExpr;
        return x->rank - y->rank;
}

void parse_global_scope(void)
{
        Token tok;
        String s;

        PARSE_LOG();
        globalScope = add_global_scope();
        push_scope(globalScope);
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

        /* fix up symbolInfo table: add references to various entities */
        for (Type x = 0; x < typeCnt; x++)
                symbolInfo[typeInfo[x].sym].tType = x;
        for (Data x = 0; x < dataCnt; x++)
                symbolInfo[dataInfo[x].sym].tData = x;
        for (Proc x = 0; x < procCnt; x++)
                symbolInfo[procInfo[x].sym].tProc = x;

        {
                /* permute Symbol array so they are grouped by defining scope */
                /* TODO: this kind of renaming should be abstracted */
                Symbol *order;
                Symbol *newname;
                struct Alloc orderAlloc;
                struct Alloc newnameAlloc;
                BUF_INIT(order, orderAlloc);
                BUF_INIT(newname, newnameAlloc);
                BUF_RESERVE(order, orderAlloc, symbolCnt);
                BUF_RESERVE(newname, newnameAlloc, symbolCnt);
                for (Symbol i = 0; i < symbolCnt; i++)
                        order[i] = i;
                sort_array(order, symbolCnt, sizeof *order,
                           compare_Symbol);
                for (Symbol i = 0; i < symbolCnt; i++)
                        newname[order[i]] = i;
                for (Type i = 0; i < typeCnt; i++)
                        typeInfo[i].sym = newname[typeInfo[i].sym];
                for (Data i = 0; i < dataCnt; i++)
                        dataInfo[i].sym = newname[dataInfo[i].sym];
                for (Proc i = 0; i < procCnt; i++)
                        procInfo[i].sym = newname[procInfo[i].sym];
                for (Symbol i = 0; i < symbolCnt; i++) {
                        Symbol j = newname[i];
                        while (j != i) {
                                struct SymbolInfo tmp = symbolInfo[i];
                                symbolInfo[i] = symbolInfo[j];
                                symbolInfo[j] = tmp;
                                Symbol next = newname[j];
                                newname[j] = j;
                                j = next;
                        }
                }
                BUF_EXIT(order, orderAlloc);
                BUF_EXIT(newname, newnameAlloc);
        }

        sort_array(columnInfo, columnCnt, sizeof *columnInfo,
                   compare_ColumnInfo);
        sort_array(procParamInfo, procParamCnt, sizeof *procParamInfo,
                   compare_ProcParamInfo);
        sort_array(childStmtInfo, childStmtCnt, sizeof *childStmtInfo,
                   compare_ChildStmtInfo);
        sort_array(callArgInfo, callArgCnt, sizeof *callArgInfo,
                   compare_CallArgInfo);

        for (Column col = columnCnt; col --> 0;) {
                Table table = columnInfo[col].table;
                tableInfo[table].numColumns++;
                tableInfo[table].firstColumn = col;
        }

        for (ProcParam param = procParamCnt; param --> 0;) {
                Proc proc = procParamInfo[param].proc;
                procInfo[proc].nparams++;
                procInfo[proc].firstParam = param;
        }

        for (int i = childStmtCnt; i --> 0;) {
                Stmt parent = childStmtInfo[i].parent;
                assert(stmtInfo[parent].kind == STMT_COMPOUND);
                stmtInfo[parent].tCompound.numStatements++;
                stmtInfo[parent].tCompound.firstChildStmtIdx = i;
        }

        for (int i = callArgCnt; i --> 0;) {
                Expr callee = callArgInfo[i].callExpr;
                assert(exprInfo[callee].kind == EXPR_CALL);
                exprInfo[callee].tCall.nargs++;
                exprInfo[callee].tCall.firstArgIdx = i;
        }

        for (Symbol i = symbolCnt; i --> 0;) {
                scopeInfo[symbolInfo[i].scope].numSymbols++;
                scopeInfo[symbolInfo[i].scope].firstSymbol = i;
        }
}

Symbol find_symbol_in_scope(String name, Scope scope)
{
        //msg("RESOLVE %s\n", string_buffer(name));
        for (; scope != -1; scope = scopeInfo[scope].parentScope) {
                Symbol first = scopeInfo[scope].firstSymbol;
                Symbol last = first + scopeInfo[scope].numSymbols;
                for (Symbol i = first; i < last; i++) {
                        if (symbolInfo[i].name == name) {
                                //msg("FOUND symbol %s\n", string_buffer(name));
                                return i;
                        }
                }
        }
        msg("Symbol %s MISSING\n", string_buffer(name));
        return -1;
}

void resolve_symbols(void)
{
        for (Symref ref = 0; ref < symrefCnt; ref++) {
                String name = symrefInfo[ref].name;
                Scope refScope = symrefInfo[ref].refScope;
                symrefInfo[ref].sym = find_symbol_in_scope(name, refScope);
        }
}

int main(int argc, const char **argv)
{
        init_strings();
        init_basetypes();
        for (int i = 1; i < argc; i++)
                if (cstr_compare(argv[i], "-debug") == 0)
                        doDebug = 1;
        add_file(intern_cstring("test.txt"));
        parse_global_scope();
        resolve_symbols();
        msg("\n\n\n");
        prettyprint();
        return 0;
}
