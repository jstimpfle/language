#include "defs.h"
#include "api.h"

INTERNAL
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

INTERNAL
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

INTERNAL
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

INTERNAL
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
                Type tp = typeCnt++;
                RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
                typeInfo[tp].kind = TYPE_BASE;
                typeInfo[tp].tBase.name = name;
                typeInfo[tp].tBase.size = size;
                add_type_symbol(name, globalScope, tp);
        }
}

INTERNAL
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

INTERNAL
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

INTERNAL
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

INTERNAL
int look_char(void)
{
        if (! haveSavedChar) {
                if (currentOffset < fileInfo[currentFile].size) {
                        haveSavedChar = 1;
                        int c = fileInfo[currentFile].buf[currentOffset];
                        if (c < 32 && c != ' ' && c != '\n')
                                FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
                                                     "Invalid byte %d\n", c);
                        savedChar = c;
                }
                else {
                        savedChar = -1;
                }
        }
        return savedChar;
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

INTERNAL
void push_scope(Scope scope)
{
        if (scopeStackCnt >= LENGTH(scopeStack))
                FATAL_PARSE_ERROR_AT(currentFile, currentOffset,
                                     "Maximum scope nesting depth reached\n");
        scopeStack[scopeStackCnt++] = scope;
        currentScope = scope;
}

INTERNAL
void pop_scope(void)
{
        assert(scopeStackCnt > 1);
        scopeStackCnt--;
        currentScope = scopeStack[scopeStackCnt-1];
}

INTERNAL
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
                if (c != ' ' && c != '\n')
                        break;
        }

        /* good to go. Variable c contains first character to lex */
        off = currentOffset - 1;
        if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) {
                lexbufCnt = 0;
                for (;;) {
                        int idx = lexbufCnt++;
                        RESIZE_GLOBAL_BUFFER(lexbuf, lexbufCnt);
                        lexbuf[idx] = (char) c;
                        c = look_char();
                        if (!('a' <= c && c <= 'z') &&
                            !('A' <= c && c <= 'Z') &&
                            !('0' <= c && c <= '9'))
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

INTERNAL
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

INTERNAL
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

INTERNAL
Token look_token_kind(int tkind)
{
        Token tok = look_next_token();
        if (tok == -1 || tokenInfo[tok].kind != tkind)
                return -1;
        return tok;
}

INTERNAL
String parse_name(void)
{
        PARSE_LOG();
        Token tok = parse_token_kind(TOKTYPE_WORD);
        return tokenInfo[tok].tWord.string;
}

INTERNAL
Symref parse_symref(void)
{
        PARSE_LOG();
        Token tok = parse_token_kind(TOKTYPE_WORD);
        Scope refScope = currentScope;
        Symref ref = symrefCnt++;
        RESIZE_GLOBAL_BUFFER(symrefInfo, symrefCnt);
        symrefInfo[ref].name = tokenInfo[tok].tWord.string;
        symrefInfo[ref].refScope = refScope;
        symrefInfo[ref].tok = tok;
        return ref;
}

INTERNAL
Type parse_type(void)
{
        PARSE_LOG();
        Symref ref = parse_symref();
        Type tp = typeCnt++;
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
        typeInfo[tp].kind = TYPE_REFERENCE;
        typeInfo[tp].tRef.ref = ref;
        typeInfo[tp].tRef.resolvedTp = -1;
        Token tok = look_next_token();
        if (tokenInfo[tok].kind == TOKTYPE_ASTERISK) {
                parse_next_token();
                Type ptp = typeCnt++;
                RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
                typeInfo[ptp].kind = TYPE_POINTER;
                typeInfo[ptp].tPointer.tp = tp;
                tp = ptp;
        }
        return tp;
}

INTERNAL
Type parse_entity(void)
{
        PARSE_LOG();

        Type tp = parse_type();
        String name = parse_name();
        parse_token_kind(TOKTYPE_SEMICOLON);
        Type etp = typeCnt++;
        add_type_symbol(name, currentScope, etp);

        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
        typeInfo[etp].kind = TYPE_ENTITY;
        typeInfo[etp].tEntity.name = name;
        typeInfo[etp].tEntity.tp = tp;
        return etp;
}

INTERNAL
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

INTERNAL
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

INTERNAL
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
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].kind = EXPR_SYMREF;
                exprInfo[expr].tSymref.ref = ref;
        }
        else if (tokenInfo[tok].kind == TOKTYPE_INTEGER) {
                parse_next_token();
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].kind = EXPR_LITERAL;
                exprInfo[expr].tLiteral.tok = tok;
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
                        Expr calleeExpr = expr;
                        expr = exprCnt++;
                        while (look_token_kind(TOKTYPE_RIGHTPAREN) == -1) {
                                Expr argExpr = parse_expr(0);
                                int x = callArgCnt++;
                                RESIZE_GLOBAL_BUFFER(callArgInfo, callArgCnt);
                                callArgInfo[x].callExpr = expr;
                                callArgInfo[x].argExpr = argExpr;
                                callArgInfo[x].rank = x;
                                if (look_token_kind(TOKTYPE_COMMA) == -1)
                                        break;
                                parse_next_token();
                        }
                        parse_token_kind(TOKTYPE_RIGHTPAREN);
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].kind = EXPR_CALL;
                        exprInfo[expr].tCall.callee = calleeExpr;
                        exprInfo[expr].tCall.firstArgIdx = -1;
                        exprInfo[expr].tCall.nargs = 0;
                }
                else if (tokenInfo[tok].kind == TOKTYPE_DOT) {
                        parse_next_token();
                        Token x = parse_token_kind(TOKTYPE_WORD);
                        String name = tokenInfo[x].tWord.string;
                        Expr enclosingExpr = expr;
                        expr = exprCnt++;
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].kind = EXPR_MEMBER;
                        exprInfo[expr].tMember.expr = enclosingExpr;
                        exprInfo[expr].tMember.name = name;
                }
                else if (tokenInfo[tok].kind == TOKTYPE_LEFTBRACKET) {
                        parse_next_token();
                        Expr expr1 = expr;
                        Expr expr2 = parse_expr(0);
                        parse_token_kind(TOKTYPE_RIGHTBRACKET);
                        expr = exprCnt++;
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].kind = EXPR_SUBSCRIPT;
                        exprInfo[expr].tSubscript.expr1 = expr1;
                        exprInfo[expr].tSubscript.expr2 = expr2;
                }
                else if (token_is_binary_infix_operator(tok, &opkind)) {
                        int opprec = binopInfo[opkind].prec;
                        if (opprec < minprec)
                                break;
                        parse_next_token();
                        Expr expr1 = expr;
                        Expr expr2 = parse_expr(opprec + 1);
                        expr = exprCnt++;
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].kind = EXPR_BINOP;
                        exprInfo[expr].tBinop.kind = opkind;
                        exprInfo[expr].tBinop.tok = tok;
                        exprInfo[expr].tBinop.expr1 = expr1;
                        exprInfo[expr].tBinop.expr2 = expr2;
                }
                else {
                        break;
                }
        }
        return expr;
}

INTERNAL
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

INTERNAL
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

INTERNAL
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

INTERNAL Stmt parse_expr_or_compound_stmt(void);
INTERNAL Stmt parse_stmt(void);

INTERNAL
Stmt parse_compound_stmt(void)
{
        PARSE_LOG();

        Stmt stmt = stmtCnt++;
        parse_token_kind(TOKTYPE_LEFTBRACE);
        while (look_token_kind(TOKTYPE_RIGHTBRACE) == -1) {
                Stmt child = parse_stmt();
                int x = childStmtCnt++;
                RESIZE_GLOBAL_BUFFER(childStmtInfo, childStmtCnt);
                childStmtInfo[x].parent = stmt;
                childStmtInfo[x].child = child;
                childStmtInfo[x].rank = x;
        }
        parse_token_kind(TOKTYPE_RIGHTBRACE);
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_COMPOUND;
        stmtInfo[stmt].tCompound.numStatements = 0;
        stmtInfo[stmt].tCompound.firstChildStmtIdx = -1;
        return stmt;
}

INTERNAL
Stmt parse_expr_or_compound_stmt(void)
{
        PARSE_LOG();
        Token tok = look_next_token();
        if (tokenInfo[tok].kind == TOKTYPE_LEFTBRACE)
                return parse_compound_stmt();
        else
                return parse_expr_stmt();
}

INTERNAL
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

INTERNAL
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

INTERNAL
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

INTERNAL
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

INTERNAL
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

INTERNAL
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
                Param param = paramCnt++;
                Symbol paramsym = symbolCnt++;
                RESIZE_GLOBAL_BUFFER(paramInfo, paramCnt);
                RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
                paramInfo[param].proc = proc;
                paramInfo[param].sym = paramsym;
                paramInfo[param].tp = paramtp;
                paramInfo[param].rank = param;
                symbolInfo[paramsym].name = paramname;
                symbolInfo[paramsym].scope = pscope;
                symbolInfo[paramsym].kind = SYMBOL_PARAM;
                symbolInfo[paramsym].tParam = param;
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

INTERNAL
int compare_Symbol(const void *a, const void *b)
{
        const Symbol *x = a;
        const Symbol *y = b;
        return symbolInfo[*x].scope - symbolInfo[*y].scope;
}

INTERNAL
int compare_ParamInfo(const void *a, const void *b)
{
        const struct ParamInfo *x = a;
        const struct ParamInfo *y = b;
        if (x->proc != y->proc)
                return x->proc - y->proc;
        return x->rank - y->rank;
}

INTERNAL
int compare_ChildStmtInfo(const void *a, const void *b)
{
        const struct ChildStmtInfo *x = a;
        const struct ChildStmtInfo *y = b;
        if (x->parent != y->parent)
                return x->parent - y->parent;
        return x->rank - y->rank;
}

INTERNAL
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
