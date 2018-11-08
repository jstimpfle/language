#include "defs.h"
#include "api.h"

Token add_word_token(File file, int offset, String string)
{
        Token x = tokenCnt++;
        RESIZE_GLOBAL_BUFFER(tokenInfo, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = TOKTYPE_WORD;
        tokenInfo[x].tWord.string = string;
        return x;
}

Token add_integer_token(File file, int offset, long long value)
{
        Token x = tokenCnt++;
        RESIZE_GLOBAL_BUFFER(tokenInfo, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = TOKTYPE_INTEGER;
        tokenInfo[x].tInteger.value = value;
        return x;
}

Token add_bare_token(File file, int offset, int kind)
{
        Token x = tokenCnt++;
        RESIZE_GLOBAL_BUFFER(tokenInfo, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = kind;
        return x;
}

Type add_base_type(String name, int size)
{
        Type x = typeCnt++;
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
        typeInfo[x].kind = TYPE_BASE;
        typeInfo[x].tBase.name = name;
        typeInfo[x].tBase.size = size;
        return x;
}

Type add_entity_type(String name, Type tp)
{
        Type x = typeCnt++;
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
        typeInfo[x].kind = TYPE_ENTITY;
        typeInfo[x].tEntity.name = name;
        typeInfo[x].tEntity.tp = tp;
        return x;
}

Type add_pointer_type(Type tp)
{
        Type x = typeCnt++;
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
        typeInfo[x].kind = TYPE_POINTER;
        typeInfo[x].tPointer.tp = tp;
        return x;
}

Type add_proc_type(Type rettp, int nargs, int firstParamtype)
{
        Type x = typeCnt++;
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
        typeInfo[x].kind = TYPE_PROC;
        typeInfo[x].tProc.rettp = rettp;
        typeInfo[x].tProc.nargs = nargs;
        typeInfo[x].tProc.firstParamtype = firstParamtype;
        return x;
}

Type add_ref_type(Symref ref)
{
        Type x = typeCnt++;
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
        typeInfo[x].kind = TYPE_REFERENCE;
        typeInfo[x].tRef.ref = ref;
        typeInfo[x].tRef.resolvedTp = -1;
        return x;
}

Type add_paramtype(Type proctp, Type argtp)
{
        int x = paramtypeCnt++;
        RESIZE_GLOBAL_BUFFER(paramtypeInfo, paramtypeCnt);
        paramtypeInfo[x].proctp = proctp;
        paramtypeInfo[x].argtp = argtp;
        paramtypeInfo[x].rank = x;
        return x;
}

Symbol add_type_symbol(String name, Scope scope, Type tp)
{
        Symbol x = symbolCnt++;
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
        symbolInfo[x].name = name;
        symbolInfo[x].scope = scope;
        symbolInfo[x].kind = SYMBOL_TYPE;
        symbolInfo[x].tType = tp;
        return x;
}

Symbol add_param_symbol(String name, Scope scope, Param param)
{
        Symbol x = symbolCnt++;
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
        symbolInfo[x].name = name;
        symbolInfo[x].scope = scope;
        symbolInfo[x].kind = SYMBOL_PARAM;
        symbolInfo[x].tParam = param;
        return x;
}

Scope add_global_scope(void)
{
        Scope x = scopeCnt++;
        RESIZE_GLOBAL_BUFFER(scopeInfo, scopeCnt);
        scopeInfo[x].parentScope = -1;
        scopeInfo[x].firstSymbol = -1;
        scopeInfo[x].numSymbols = 0;
        scopeInfo[x].kind = SCOPE_GLOBAL;
        return x;
}

Symref add_symref(Token tok, Scope refScope)
{
        Symref ref = symrefCnt++;
        RESIZE_GLOBAL_BUFFER(symrefInfo, symrefCnt);
        symrefInfo[ref].name = tokenInfo[tok].tWord.string;
        symrefInfo[ref].refScope = refScope;
        symrefInfo[ref].tok = tok;
        return ref;
}

Expr add_symref_expr(Symref ref)
{
        Expr x = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[x].kind = EXPR_SYMREF;
        exprInfo[x].tSymref.ref = ref;
        return x;
}

Expr add_literal_expr(Token tok)
{
        Expr x = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[x].kind = EXPR_LITERAL;
        exprInfo[x].tLiteral.tok = tok;
        return x;
}

Expr add_call_expr(Expr callee)
{
        Expr x = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[x].kind = EXPR_CALL;
        exprInfo[x].tCall.callee = callee;
        exprInfo[x].tCall.firstArgIdx = -1;
        exprInfo[x].tCall.nargs = 0;
        return x;
}

Expr add_unop_expr(int opkind, Token tok, Expr expr)
{
        Expr x = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[x].kind = EXPR_UNOP;
        exprInfo[x].tUnop.kind = opkind;
        exprInfo[x].tUnop.tok = tok;
        exprInfo[x].tUnop.expr = expr;
        return x;
}

Expr add_binop_expr(int opkind, Token tok, Expr expr1, Expr expr2)
{
        Expr x = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[x].kind = EXPR_BINOP;
        exprInfo[x].tBinop.kind = opkind;
        exprInfo[x].tBinop.tok = tok;
        exprInfo[x].tBinop.expr1 = expr1;
        exprInfo[x].tBinop.expr2 = expr2;
        return x;
}

Expr add_member_expr(Expr expr, String name)
{
        Expr x = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[x].kind = EXPR_MEMBER;
        exprInfo[x].tMember.expr = expr;
        exprInfo[x].tMember.name = name;
        return x;
}

Expr add_subscript_expr(Expr expr1, Expr expr2)
{
        Expr x = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[x].kind = EXPR_SUBSCRIPT;
        exprInfo[x].tSubscript.expr1 = expr1;
        exprInfo[x].tSubscript.expr2 = expr2;
        return x;
}

Param add_Param(Proc proc, Type tp)
{
        Param x = paramCnt++;
        RESIZE_GLOBAL_BUFFER(paramInfo, paramCnt);
        paramInfo[x].proc = proc;
        paramInfo[x].sym = -1; // later
        paramInfo[x].tp = tp;
        paramInfo[x].rank = x;
        return x;
}

void add_ChildStmt(Stmt parent, Stmt child)
{
        int x = childStmtCnt++;
        RESIZE_GLOBAL_BUFFER(childStmtInfo, childStmtCnt);
        childStmtInfo[x].parent = parent;
        childStmtInfo[x].child = child;
        childStmtInfo[x].rank = x;
}

void add_CallArg(Expr callExpr, Expr argExpr)
{
        int x = callArgCnt++;
        RESIZE_GLOBAL_BUFFER(callArgInfo, callArgCnt);
        callArgInfo[x].callExpr = callExpr;
        callArgInfo[x].argExpr = argExpr;
        callArgInfo[x].rank = x;
}

void initialize_pseudo_constant_data(void)
{
        for (int i = 0; i < LENGTH(stringsToBeInterned); i++) {
                int idx = stringsToBeInterned[i].constant;
                const char *str = stringsToBeInterned[i].string;
                constStr[idx] = intern_cstring(str);
        }

        for (int i = 0; i < basetypesToBeInitializedCnt; i++) {
                String name = intern_cstring(basetypesToBeInitialized[i].name);
                int size = basetypesToBeInitialized[i].size;
                Type tp = add_base_type(name, size);
                add_type_symbol(name, globalScope, tp);
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

int look_char(void)
{
        if (! haveSavedChar) {
                if (currentOffset < fileInfo[currentFile].size) {
                        haveSavedChar = 1;
                        int c = fileInfo[currentFile].buf[currentOffset];
                        if (char_is_invalid(c)) {
                                FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
                                                     "Invalid byte %d\n", c);
                        }
                        savedChar = c;
                }
                else {
                        savedChar = -1;
                }
        }
        return savedChar;
}

int read_char(void)
{
        int c = look_char();
        if (c != -1) {
                currentOffset++;
                haveSavedChar = 0;
        }
        return c;
}

void push_scope(Scope scope)
{
        if (scopeStackCnt >= LENGTH(scopeStack))
                FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
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
                if (! char_is_whitespace(c))
                        break;
        }

        /* good to go. Variable c contains first character to lex */
        off = currentOffset - 1;
        if (char_is_alpha(c)) {
                lexbufCnt = 0;
                for (;;) {
                        int idx = lexbufCnt++;
                        RESIZE_GLOBAL_BUFFER(lexbuf, lexbufCnt);
                        lexbuf[idx] = (char) c;
                        c = look_char();
                        if (! char_is_alpha(c) && ! char_is_digit(c))
                                break;
                        read_char();
                }
                ans = add_word_token(currentFile, off,
                                     intern_string(lexbuf, lexbufCnt));
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
        else if (c == '.') {
                ans = add_bare_token(currentFile, off, TOKTYPE_DOT);
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
        else if (c == '~') {
                ans = add_bare_token(currentFile, off, TOKTYPE_TILDE);
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
                FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
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

String parse_name(void)
{
        PARSE_LOG();
        Token tok = parse_token_kind(TOKTYPE_WORD);
        return tokenInfo[tok].tWord.string;
}

Symref parse_symref(void)
{
        PARSE_LOG();
        Token tok = parse_token_kind(TOKTYPE_WORD);
        return add_symref(tok, currentScope);
}

Type parse_type(void)
{
        PARSE_LOG();
        Symref ref = parse_symref();
        Type tp = add_ref_type(ref);
        Token tok = look_next_token();
        if (tokenInfo[tok].kind == TOKTYPE_ASTERISK) {
                parse_next_token();
                tp = add_pointer_type(tp);
        }
        return tp;
}

Type parse_entity(void)
{
        Type tp;
        Type etp;
        String name;
        Symbol sym;

        PARSE_LOG();
        tp = parse_type();
        name = parse_name();
        etp = add_entity_type(name, tp);
        sym = add_type_symbol(name, currentScope, etp);
        parse_token_kind(TOKTYPE_SEMICOLON);
        return etp;
}

Array parse_array(void)
{
        PARSE_LOG();

        Type valuetp = parse_type();
        String name = parse_name();
        parse_token_kind(TOKTYPE_LEFTBRACKET);
        Type idxtp = parse_type();
        parse_token_kind(TOKTYPE_RIGHTBRACKET);
        parse_token_kind(TOKTYPE_SEMICOLON);

        Type tp = typeCnt++;
        Array array = arrayCnt++;
        Symbol sym = symbolCnt++;

        RESIZE_GLOBAL_BUFFER(arrayInfo, arrayCnt);
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);

        arrayInfo[array].scope = currentScope;
        arrayInfo[array].tp = tp;
        arrayInfo[array].sym = sym;

        typeInfo[tp].kind = TYPE_ARRAY;
        typeInfo[tp].tArray.idxtp = idxtp;
        typeInfo[tp].tArray.valuetp = valuetp;

        symbolInfo[sym].name = name;
        symbolInfo[sym].scope = currentScope;
        symbolInfo[sym].kind = SYMBOL_ARRAY;
        symbolInfo[sym].tArray = array;
        add_type_symbol(name, currentScope, tp);

        return array;
}

Data parse_data(void)
{
        PARSE_LOG();

        Type tp = parse_type();
        String name = parse_name();
        parse_token_kind(TOKTYPE_SEMICOLON);

        Scope scope = currentScope;
        Data data = dataCnt++;
        Symbol sym = symbolCnt++;

        RESIZE_GLOBAL_BUFFER(dataInfo, dataCnt);
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);

        dataInfo[data].scope = scope;
        dataInfo[data].tp = tp;
        dataInfo[data].sym = sym;

        symbolInfo[sym].name = name;
        symbolInfo[sym].scope = scope;
        symbolInfo[sym].kind = SYMBOL_DATA;
        symbolInfo[sym].tData = data;

        return data;
}

Expr parse_expr(int minprec)
{
        PARSE_LOG();

        Expr expr;
        int opkind;
        Token tok = look_next_token();
        if (token_is_unary_prefix_operator(tok, &opkind)) {
                parse_next_token();
                Expr subexpr = parse_expr(42  /* TODO: unop precedence */);
                expr = add_unop_expr(opkind, tok, subexpr);
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
                FATAL_PARSE_ERROR_AT_TOK(tok, "Expected expression\n");
        }

        for (;;) {
                tok = look_next_token();
                if (token_is_unary_postfix_operator(tok, &opkind)) {
                        parse_next_token();
                        expr = add_unop_expr(opkind, tok, expr);
                }
                else if (tokenInfo[tok].kind == TOKTYPE_LEFTPAREN) {
                        parse_next_token();
                        expr = add_call_expr(expr);
                        while (look_token_kind(TOKTYPE_RIGHTPAREN) == -1) {
                                Expr subexpr = parse_expr(0);
                                add_CallArg(expr, subexpr);
                                if (look_token_kind(TOKTYPE_COMMA) == -1)
                                        break;
                                parse_next_token();
                        }
                        parse_token_kind(TOKTYPE_RIGHTPAREN);
                }
                else if (tokenInfo[tok].kind == TOKTYPE_DOT) {
                        parse_next_token();
                        Token x = parse_token_kind(TOKTYPE_WORD);
                        String name = tokenInfo[x].tWord.string;
                        expr = add_member_expr(expr, name);
                }
                else if (tokenInfo[tok].kind == TOKTYPE_LEFTBRACKET) {
                        parse_next_token();
                        Expr subexpr = parse_expr(0);
                        parse_token_kind(TOKTYPE_RIGHTBRACKET);
                        expr = add_subscript_expr(expr, subexpr);
                }
                else if (token_is_binary_infix_operator(tok, &opkind)) {
                        int opprec = binopInfo[opkind].prec;
                        if (opprec < minprec)
                                break;
                        parse_next_token();
                        Expr subexpr = parse_expr(opprec + 1);
                        expr = add_binop_expr(opkind, tok, expr, subexpr);
                }
                else {
                        break;
                }
        }
        return expr;
}

Stmt parse_data_stmt(void)
{
        PARSE_LOG();
        Data data = parse_data();
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_DATA;
        stmtInfo[stmt].tData = data;
        return stmt;
}

Stmt parse_array_stmt(void)
{
        PARSE_LOG();
        Array array = parse_array();
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_ARRAY;
        stmtInfo[stmt].tArray = array;
        return stmt;
}

Stmt parse_expr_stmt(void)
{
        PARSE_LOG();
        Expr expr = parse_expr(0);
        parse_token_kind(TOKTYPE_SEMICOLON);
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_EXPR;
        stmtInfo[stmt].tExpr.expr = expr;
        return stmt;
}

Stmt parse_expr_or_compound_stmt(void);
Stmt parse_stmt(void);

Stmt parse_compound_stmt(void)
{
        PARSE_LOG();

        Stmt stmt = stmtCnt++;
        parse_token_kind(TOKTYPE_LEFTBRACE);
        while (look_token_kind(TOKTYPE_RIGHTBRACE) == -1) {
                Stmt substmt = parse_stmt();
                add_ChildStmt(stmt, substmt);
        }
        parse_token_kind(TOKTYPE_RIGHTBRACE);
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_COMPOUND;
        stmtInfo[stmt].tCompound.numStatements = 0;
        stmtInfo[stmt].tCompound.firstChildStmtIdx = -1;
        return stmt;
}

Stmt parse_expr_or_compound_stmt(void)
{
        PARSE_LOG();
        Token tok = look_next_token();
        if (tokenInfo[tok].kind == TOKTYPE_LEFTBRACE)
                return parse_compound_stmt();
        else
                return parse_expr_stmt();
}

Stmt parse_if_stmt(void)
{
        PARSE_LOG();
        Stmt stmt = stmtCnt++;
        parse_token_kind(TOKTYPE_LEFTPAREN);
        Expr condExpr = parse_expr(0);
        parse_token_kind(TOKTYPE_RIGHTPAREN);
        Stmt childStmt = parse_expr_or_compound_stmt();
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_IF;
        stmtInfo[stmt].tIf.condExpr = condExpr;
        stmtInfo[stmt].tIf.childStmt = childStmt;
        return stmt;
}

Stmt parse_while_stmt(void)
{
        PARSE_LOG();
        parse_token_kind(TOKTYPE_LEFTPAREN);
        Expr condExpr = parse_expr(0);
        parse_token_kind(TOKTYPE_RIGHTPAREN);
        Stmt childStmt = parse_expr_or_compound_stmt();
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_WHILE;
        stmtInfo[stmt].tWhile.condExpr = condExpr;
        stmtInfo[stmt].tWhile.childStmt = childStmt;
        return stmt;
}

Stmt parse_for_stmt(void)
{
        PARSE_LOG();
        parse_token_kind(TOKTYPE_LEFTPAREN);
        Stmt initStmt = parse_expr_stmt();
        parse_token_kind(TOKTYPE_SEMICOLON);
        Expr condExpr = parse_expr(0);
        parse_token_kind(TOKTYPE_SEMICOLON);
        Stmt stepStmt = parse_expr_stmt();
        parse_token_kind(TOKTYPE_RIGHTPAREN);
        Stmt childStmt = parse_expr_or_compound_stmt();
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_FOR;
        stmtInfo[stmt].tFor.initStmt = initStmt;
        stmtInfo[stmt].tFor.condExpr = condExpr;
        stmtInfo[stmt].tFor.stepStmt = stepStmt;
        stmtInfo[stmt].tFor.childStmt = childStmt;
        return stmt;
}

Stmt parse_return_stmt(void)
{
        PARSE_LOG();
        Expr expr = parse_expr(0);
        parse_token_kind(TOKTYPE_SEMICOLON);
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_RETURN;
        stmtInfo[stmt].tReturn.expr = expr;
        return stmt;
}

Stmt parse_stmt(void)
{
        Token tok;

        PARSE_LOG();
        tok = look_next_token();
        if (tokenInfo[tok].kind == TOKTYPE_WORD) {
                String s = tokenInfo[tok].tWord.string;
                if (s == constStr[CONSTSTR_DATA]) {
                        parse_next_token();
                        return parse_data_stmt();
                }
                else if (s == constStr[CONSTSTR_ARRAY]) {
                        parse_next_token();
                        return parse_array_stmt();
                }
                else if (s == constStr[CONSTSTR_IF]) {
                        parse_next_token();
                        return parse_if_stmt();
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
        PARSE_LOG();

        Type rettp = parse_type();
        String pname = parse_name();

        Scope parentScope = currentScope;
        Scope pscope = scopeCnt++;
        Proc proc = procCnt++;
        Symbol psym = symbolCnt++;

        RESIZE_GLOBAL_BUFFER(scopeInfo, scopeCnt);
        RESIZE_GLOBAL_BUFFER(procInfo, procCnt);
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);

        push_scope(pscope);
        parse_token_kind(TOKTYPE_LEFTPAREN);
        for (;;) {
                Token tok = look_next_token();
                if (tokenInfo[tok].kind == TOKTYPE_RIGHTPAREN)
                        break;
                Type paramtp = parse_type();
                String paramname = parse_name();
                Param param = add_Param(proc, paramtp);
                Symbol paramsym = add_param_symbol(paramname, pscope, param);
                paramInfo[param].sym = paramsym;
                if (look_token_kind(TOKTYPE_COMMA) == -1)
                        break;
                parse_next_token();
        }
        parse_token_kind(TOKTYPE_RIGHTPAREN);

        Stmt pbody = parse_compound_stmt();

        scopeInfo[pscope].parentScope = parentScope;
        scopeInfo[pscope].firstSymbol = -1;
        scopeInfo[pscope].numSymbols = 0;
        scopeInfo[pscope].kind = SCOPE_PROC;
        scopeInfo[pscope].tProc.proc = proc;

        procInfo[proc].tp = rettp;
        procInfo[proc].sym = psym;
        procInfo[proc].scope = pscope;
        procInfo[proc].firstParam = -1;
        procInfo[proc].nparams = 0;
        procInfo[proc].body = pbody;

        symbolInfo[psym].name = pname;
        symbolInfo[psym].scope = parentScope;
        symbolInfo[psym].kind = SYMBOL_PROC;
        symbolInfo[psym].tProc = proc;

        pop_scope();
}

int compare_Symbol(const void *a, const void *b)
{
        const Symbol *x = a;
        const Symbol *y = b;
        return symbolInfo[*x].scope - symbolInfo[*y].scope;
}

int compare_ParamInfo(const void *a, const void *b)
{
        const struct ParamInfo *x = a;
        const struct ParamInfo *y = b;
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
                s = tokenInfo[tok].tWord.string;
                if (s == constStr[CONSTSTR_ENTITY])
                        parse_entity();
                else if (s == constStr[CONSTSTR_ARRAY])
                        parse_array();
                else if (s == constStr[CONSTSTR_DATA])
                        parse_data();
                else if (s == constStr[CONSTSTR_PROC])
                        parse_proc();
                else
                        FATAL_PARSE_ERROR_AT_TOK(tok,
                            "Unexpected word %s\n", TS(tok));
        }

        /* fix up symbolInfo table: add references to various entities */
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
                BUF_INIT(&order, &orderAlloc);
                BUF_INIT(&newname, &newnameAlloc);
                BUF_RESERVE(&order, &orderAlloc, symbolCnt);
                BUF_RESERVE(&newname, &newnameAlloc, symbolCnt);
                for (Symbol i = 0; i < symbolCnt; i++)
                        order[i] = i;
                sort_array(order, symbolCnt, sizeof *order,
                           compare_Symbol);
                for (Symbol i = 0; i < symbolCnt; i++)
                        newname[order[i]] = i;
                for (Data i = 0; i < dataCnt; i++)
                        dataInfo[i].sym = newname[dataInfo[i].sym];
                for (Array i = 0; i < arrayCnt; i++)
                        arrayInfo[i].sym = newname[arrayInfo[i].sym];
                for (Proc i = 0; i < procCnt; i++)
                        procInfo[i].sym = newname[procInfo[i].sym];
                for (Param i = 0; i < paramCnt; i++)
                        paramInfo[i].sym = newname[paramInfo[i].sym];
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
                BUF_EXIT(&order, &orderAlloc);
                BUF_EXIT(&newname, &newnameAlloc);
        }

        /* sort paramInfo array and fix symbols */
        sort_array(paramInfo, paramCnt, sizeof *paramInfo,
                   compare_ParamInfo);
        for (Param i = 0; i < paramCnt; i++)
                symbolInfo[paramInfo[i].sym].tParam = i;

        sort_array(childStmtInfo, childStmtCnt, sizeof *childStmtInfo,
                   compare_ChildStmtInfo);
        sort_array(callArgInfo, callArgCnt, sizeof *callArgInfo,
                   compare_CallArgInfo);

        for (Param param = paramCnt; param --> 0;) {
                Proc proc = paramInfo[param].proc;
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
