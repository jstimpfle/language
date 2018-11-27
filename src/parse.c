#include "defs.h"
#include "api.h"

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
        exprInfo[x].proc = currentProc;
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
        ASSERT(scopeStackCnt > 1);
        scopeStackCnt--;
        currentScope = scopeStack[scopeStackCnt-1];
}

INTERNAL
String parse_name(void)
{
        PARSE_LOG();
        Token tok = parse_token_kind(TOKEN_WORD);
        return tokenInfo[tok].tWord.string;
}

INTERNAL
Symref parse_symref(void)
{
        PARSE_LOG();
        Token tok = parse_token_kind(TOKEN_WORD);
        Scope refScope = currentScope;
        Symref ref = symrefCnt++;
        RESIZE_GLOBAL_BUFFER(symrefInfo, symrefCnt);
        RESIZE_GLOBAL_BUFFER(symrefToToken, symrefCnt);
        symrefInfo[ref].name = tokenInfo[tok].tWord.string;
        symrefInfo[ref].refScope = refScope;
        symrefToToken[ref] = tok;
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
        if (tokenInfo[tok].kind == TOKEN_ASTERISK) {
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
        parse_token_kind(TOKEN_SEMICOLON);
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
        parse_token_kind(TOKEN_LEFTBRACKET);
        Type idxtp = parse_type();
        parse_token_kind(TOKEN_RIGHTBRACKET);
        parse_token_kind(TOKEN_SEMICOLON);

        Array array = arrayCnt++;
        Type tp = typeCnt++;
        Symbol sym = symbolCnt++;
        add_type_symbol(name, currentScope, tp);
        RESIZE_GLOBAL_BUFFER(arrayInfo, arrayCnt);
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
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
        return array;
}

INTERNAL
Data parse_data(void)
{
        PARSE_LOG();
        Type tp = parse_type();
        String name = parse_name();
        parse_token_kind(TOKEN_SEMICOLON);

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
        symbolInfo[sym].tData.tp = tp;
        symbolInfo[sym].tData.optionaldata = data;
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
        else if (tokenInfo[tok].kind == TOKEN_WORD) {
                Symref ref = parse_symref();
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].proc = currentProc;
                exprInfo[expr].kind = EXPR_SYMREF;
                exprInfo[expr].tSymref.ref = ref;
        }
        else if (tokenInfo[tok].kind == TOKEN_INTEGER) {
                parse_next_token();
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].proc = currentProc;
                exprInfo[expr].kind = EXPR_LITERAL;
                exprInfo[expr].tLiteral.kind = LITERAL_INTEGER;
                exprInfo[expr].tLiteral.tok = tok;
        }
        else if (tokenInfo[tok].kind == TOKEN_STRING) {
                parse_next_token();
                String string = tokenInfo[tok].tString.value;
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].proc = currentProc;
                exprInfo[expr].kind = EXPR_LITERAL;
                exprInfo[expr].tLiteral.kind = LITERAL_STRING;
                exprInfo[expr].tLiteral.tString = string;
        }
        else if (tokenInfo[tok].kind == TOKEN_LEFTPAREN) {
                parse_next_token();
                expr = parse_expr(0);
                parse_token_kind(TOKEN_RIGHTPAREN);
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
                else if (tokenInfo[tok].kind == TOKEN_LEFTPAREN) {
                        parse_next_token();
                        Expr calleeExpr = expr;
                        expr = exprCnt++;
                        while (look_token_kind(TOKEN_RIGHTPAREN) == -1) {
                                Expr argExpr = parse_expr(0);
                                int x = callArgCnt++;
                                RESIZE_GLOBAL_BUFFER(callArgInfo, callArgCnt);
                                callArgInfo[x].callExpr = expr;
                                callArgInfo[x].argExpr = argExpr;
                                callArgInfo[x].rank = x;
                                if (look_token_kind(TOKEN_COMMA) == -1)
                                        break;
                                parse_next_token();
                        }
                        parse_token_kind(TOKEN_RIGHTPAREN);
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].proc = currentProc;
                        exprInfo[expr].kind = EXPR_CALL;
                        exprInfo[expr].tCall.callee = calleeExpr;
                        exprInfo[expr].tCall.firstArgIdx = -1;
                        exprInfo[expr].tCall.nargs = 0;
                }
                else if (tokenInfo[tok].kind == TOKEN_DOT) {
                        parse_next_token();
                        Token x = parse_token_kind(TOKEN_WORD);
                        String name = tokenInfo[x].tWord.string;
                        Expr enclosingExpr = expr;
                        expr = exprCnt++;
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].proc = currentProc;
                        exprInfo[expr].kind = EXPR_MEMBER;
                        exprInfo[expr].tMember.expr = enclosingExpr;
                        exprInfo[expr].tMember.name = name;
                }
                else if (tokenInfo[tok].kind == TOKEN_LEFTBRACKET) {
                        parse_next_token();
                        Expr expr1 = expr;
                        Expr expr2 = parse_expr(0);
                        parse_token_kind(TOKEN_RIGHTBRACKET);
                        expr = exprCnt++;
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].proc = currentProc;
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
                        exprInfo[expr].proc = currentProc;
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
        parse_token_kind(TOKEN_SEMICOLON);
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_EXPR;
        stmtInfo[stmt].tExpr.expr = expr;
        return stmt;
}

INTERNAL Stmt parse_imperative_statement(void);
INTERNAL Stmt parse_if_stmt(void);
INTERNAL Stmt parse_while_stmt(void);
INTERNAL Stmt parse_for_stmt(void);
INTERNAL Stmt parse_return_stmt(void);
INTERNAL Stmt parse_stmt(void);

INTERNAL
Stmt parse_compound_stmt(void)
{
        PARSE_LOG();

        Stmt stmt = stmtCnt++;
        parse_token_kind(TOKEN_LEFTBRACE);
        while (look_token_kind(TOKEN_RIGHTBRACE) == -1) {
                Stmt child = parse_stmt();
                int x = childStmtCnt++;
                RESIZE_GLOBAL_BUFFER(childStmtInfo, childStmtCnt);
                childStmtInfo[x].parent = stmt;
                childStmtInfo[x].child = child;
                childStmtInfo[x].rank = x;
        }
        parse_token_kind(TOKEN_RIGHTBRACE);
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_COMPOUND;
        stmtInfo[stmt].tCompound.numStatements = 0;
        stmtInfo[stmt].tCompound.firstChildStmtIdx = -1;
        return stmt;
}

INTERNAL
Stmt parse_imperative_statement(void)
{
        PARSE_LOG();
        Token tok = look_next_token();
        if (tokenInfo[tok].kind == TOKEN_WORD) {
                String s = tokenInfo[tok].tWord.string;
                if (s == constStr[CONSTSTR_IF]) {
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
        else if (tokenInfo[tok].kind == TOKEN_LEFTBRACE) {
                return parse_compound_stmt();
        }
        else {
                return parse_expr_stmt();
        }
}

INTERNAL
Stmt parse_if_stmt(void)
{
        PARSE_LOG();

        Stmt stmt;
        Expr condExpr;
        Stmt ifbody;

        stmt = stmtCnt++;
        parse_token_kind(TOKEN_LEFTPAREN);
        condExpr = parse_expr(0);
        parse_token_kind(TOKEN_RIGHTPAREN);
        ifbody = parse_imperative_statement();

        Token tok;
        if ((tok = look_token_kind(TOKEN_WORD)) != -1 &&
            tokenInfo[tok].tWord.string == constStr[CONSTSTR_ELSE]) {
                parse_next_token();
                Stmt elsebody = parse_imperative_statement();
                RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
                stmtInfo[stmt].kind = STMT_IFELSE;
                stmtInfo[stmt].tIfelse.condExpr = condExpr;
                stmtInfo[stmt].tIfelse.ifbody = ifbody;
                stmtInfo[stmt].tIfelse.elsebody = elsebody;
        }
        else {
                RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
                stmtInfo[stmt].kind = STMT_IF;
                stmtInfo[stmt].tIf.condExpr = condExpr;
                stmtInfo[stmt].tIf.ifbody = ifbody;
        }

        return stmt;
}

INTERNAL
Stmt parse_while_stmt(void)
{
        PARSE_LOG();
        parse_token_kind(TOKEN_LEFTPAREN);
        Expr condExpr = parse_expr(0);
        parse_token_kind(TOKEN_RIGHTPAREN);
        Stmt whilebody = parse_imperative_statement();
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_WHILE;
        stmtInfo[stmt].tWhile.condExpr = condExpr;
        stmtInfo[stmt].tWhile.whilebody = whilebody;
        return stmt;
}

INTERNAL
Stmt parse_for_stmt(void)
{
        PARSE_LOG();
        parse_token_kind(TOKEN_LEFTPAREN);
        Stmt initStmt = parse_expr_stmt();
        parse_token_kind(TOKEN_SEMICOLON);
        Expr condExpr = parse_expr(0);
        parse_token_kind(TOKEN_SEMICOLON);
        Stmt stepStmt = parse_expr_stmt();
        parse_token_kind(TOKEN_RIGHTPAREN);
        Stmt forbody = parse_imperative_statement();
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_FOR;
        stmtInfo[stmt].tFor.initStmt = initStmt;
        stmtInfo[stmt].tFor.condExpr = condExpr;
        stmtInfo[stmt].tFor.stepStmt = stepStmt;
        stmtInfo[stmt].tFor.forbody = forbody;
        return stmt;
}

INTERNAL
Stmt parse_return_stmt(void)
{
        PARSE_LOG();
        Expr expr = parse_expr(0);
        parse_token_kind(TOKEN_SEMICOLON);
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].kind = STMT_RETURN;
        stmtInfo[stmt].tReturn.expr = expr;
        return stmt;
}

INTERNAL
Stmt parse_stmt(void)
{
        PARSE_LOG();
        Token tok = look_next_token();
        if (tokenInfo[tok].kind == TOKEN_WORD) {
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
Proc parse_proc(void)
{
        PARSE_LOG();

        Type rettp = parse_type();
        String pname = parse_name();

        Scope parentScope = currentScope;
        Scope pscope = scopeCnt++;
        Proc proc = procCnt++;
        Symbol psym = symbolCnt++;
        Type ptype = typeCnt++;

        RESIZE_GLOBAL_BUFFER(scopeInfo, scopeCnt);
        RESIZE_GLOBAL_BUFFER(procInfo, procCnt);
        RESIZE_GLOBAL_BUFFER(procToType, procCnt);
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);

        currentProc = proc;
        push_scope(pscope);

        parse_token_kind(TOKEN_LEFTPAREN);
        int nparams = 0;
        for (;;) {
                Token tok = look_next_token();
                if (tokenInfo[tok].kind == TOKEN_RIGHTPAREN)
                        break;
                nparams++;
                Type paramtp = parse_type();
                String paramname = parse_name();
                Param param = paramCnt++;
                Symbol paramsym = symbolCnt++;
                Data paramdata = dataCnt++;
                RESIZE_GLOBAL_BUFFER(paramInfo, paramCnt);
                RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
                RESIZE_GLOBAL_BUFFER(dataInfo, dataCnt);
                paramInfo[param].proctp = ptype;
                paramInfo[param].tp = paramtp;
                paramInfo[param].sym = paramsym;
                paramInfo[param].rank = param;
                symbolInfo[paramsym].name = paramname;
                symbolInfo[paramsym].scope = pscope;
                symbolInfo[paramsym].kind = SYMBOL_DATA;
                symbolInfo[paramsym].tData.tp = paramtp;
                symbolInfo[paramsym].tData.optionaldata = paramdata;
                dataInfo[paramdata].scope = pscope;
                dataInfo[paramdata].tp = paramtp;
                dataInfo[paramdata].sym = paramsym;
                if (look_token_kind(TOKEN_COMMA) == -1)
                        break;
                parse_next_token();
        }
        parse_token_kind(TOKEN_RIGHTPAREN);
        Stmt pbody = parse_compound_stmt();
        pop_scope();

        scopeInfo[pscope].parentScope = parentScope;
        scopeInfo[pscope].firstSymbol = -1;
        scopeInfo[pscope].numSymbols = 0;
        scopeInfo[pscope].kind = SCOPE_PROC;
        scopeInfo[pscope].tProc.proc = proc;
        procInfo[proc].sym = psym;
        procInfo[proc].scope = pscope;
        procInfo[proc].nparams = 0;
        procInfo[proc].body = pbody;
        procToType[proc] = ptype;
        symbolInfo[psym].name = pname;
        symbolInfo[psym].scope = parentScope;
        symbolInfo[psym].kind = SYMBOL_PROC;
        symbolInfo[psym].tProc.tp = ptype;
        symbolInfo[psym].tProc.optionalproc = proc;
        typeInfo[ptype].kind = TYPE_PROC;
        typeInfo[ptype].tProc.rettp = rettp;
        typeInfo[ptype].tProc.nparams = nparams;
        typeInfo[ptype].tProc.firstParam = 0; /* TODO: we can't do this here
                                                  since param must be sorted
                                                  first */
        return proc;
}

void parse_global_scope(void)
{
        PARSE_LOG();

        globalScope = add_global_scope();
        push_scope(globalScope);
        while (look_token_kind(TOKEN_WORD) != -1) {
                Token tok = parse_token_kind(TOKEN_WORD);
                String s = tokenInfo[tok].tWord.string;
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
}
