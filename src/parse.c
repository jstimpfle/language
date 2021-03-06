#include "defs.h"
#include "api.h"

static Scope currentScope;
static Scope scopeStack[16];
static int scopeStackCnt;
static Proc currentProc;

INTERNAL
void set_symbol_token(Symbol symbol, Token token)
{
        ASSERT(0 <= symbol && symbol < symbolCnt);
        RESIZE_GLOBAL_BUFFER(symbolToToken, symbolCnt);
        symbolToToken[symbol] = token;
}

INTERNAL
Symbol add_type_symbol(String name, Scope scope, Token nameToken, Type tp)
{
        Symbol symbol = symbolCnt++;
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
        symbolInfo[symbol].name = name;
        symbolInfo[symbol].scope = scope;
        symbolInfo[symbol].symbolKind = SYMBOL_TYPE;
        symbolInfo[symbol].tType = tp;
        set_symbol_token(symbol, nameToken);
        return symbol;
}

INTERNAL
Expr add_unop_expr(int opkind, Token tok, Expr subexpr)
{
        Expr expr = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[expr].proc = currentProc;
        exprInfo[expr].exprKind = EXPR_UNOP;
        exprInfo[expr].tUnop.unopKind = opkind;
        exprInfo[expr].tUnop.tok = tok;
        exprInfo[expr].tUnop.expr = subexpr;
        return expr;
}

INTERNAL
Expr add_compilervalue_expr(Token hashtok, int cvkind)
{
        Expr expr = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[expr].proc = currentProc;
        exprInfo[expr].exprKind = EXPR_COMPILERVALUE;
        exprInfo[expr].tCompilervalue.hashtok = hashtok;
        exprInfo[expr].tCompilervalue.compilervalueKind = cvkind;
        return expr;
}

INTERNAL
int token_is_unary_prefix_operator(Token tok, int *out_optype)
{
        int tp = tokenInfo[tok].tokenKind;
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
        int tp = tokenInfo[tok].tokenKind;
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
        int tp = tokenInfo[tok].tokenKind;
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
                FATAL_PARSE_ERROR("Maximum scope nesting depth reached\n");
        scopeStack[scopeStackCnt++] = scope;
        currentScope = scope;
}

INTERNAL
void pop_scope(void)
{
        ASSERT(scopeStackCnt > 0);
        scopeStackCnt--;
        if (scopeStackCnt > 0)
                currentScope = scopeStack[scopeStackCnt-1];
        else
                currentScope = -1;
}


INTERNAL int haveSavedToken;
INTERNAL Token savedToken;

INTERNAL
Token look_next_token(void)
{
        if (!haveSavedToken) {
                Token token = lex_token();
                if (token == (Token) -1)
                        return (Token) -1;
                savedToken = token;
                haveSavedToken = 1;
        }
        return savedToken;
}

INTERNAL
Token look_token_kind(int tkind)
{
        Token tok = look_next_token();
        if (tok == -1 || tokenInfo[tok].tokenKind != tkind)
                return -1;
        return tok;
}

INTERNAL
void consume_token(void)
{
        ASSERT(haveSavedToken);
        haveSavedToken = 0;
}

// TODO: think about dependencies between the parsed things here. parse_macro()
// is used for top-level macros as well as in procs. It also depends on
// parse_expr which is why we need this forward declaration.  Same for
// parse_type().
INTERNAL Expr parse_expr(int minprec);

/* some expression are not inside a normal proc. As long as every expr has
 * a proc field (which is probably not good design anyway), we need to
 * temporarily unset the currentProc variable to parse those expression who
 * don't have a proc. */
INTERNAL
Expr parse_expr_without_currentProc(void)
{
        Proc proc = currentProc;
        currentProc = (Proc) -1;  // macro expr shouldn't have a proc
        Expr expr = parse_expr(0);
        ASSERT(exprInfo[expr].proc == (Proc) -1);
        currentProc = proc;
        return expr;
}

INTERNAL
Token parse_token_kind(int tkind)
{
        Token tok = look_next_token();
        if (tok == -1)
                FATAL_PARSE_ERROR_AT_TOK(tok,
                                "Unexpected end of file. Expected %s token\n",
                                tokenKindString[tkind]);
        if (tokenInfo[tok].tokenKind != tkind)
                FATAL_PARSE_ERROR_AT_TOK(tok,
                                "Unexpected %s token. Expected %s token\n",
                                tokenKindString[tokenInfo[tok].tokenKind],
                                tokenKindString[tkind]);
        consume_token();
        return tok;
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
Type parse_type(int prec)
{
        PARSE_LOG();

        Type tp;

        if (look_token_kind(TOKEN_CARET) != -1) {
                consume_token();
                Type subtp = parse_type(1);
                tp = typeCnt++;
                RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
                typeInfo[tp].typeKind = TYPE_POINTER;
                typeInfo[tp].tPointer.tp = subtp;
        }
        else if (look_token_kind(TOKEN_LEFTBRACKET) != (Token) -1) {
                consume_token();
                Expr lengthExpr = parse_expr_without_currentProc();
                parse_token_kind(TOKEN_RIGHTBRACKET);
                tp = typeCnt++;
                Constant lengthConstant = constantCnt++;
                Type valueTp = parse_type(1);
                RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
                typeInfo[tp].typeKind = TYPE_ARRAY;
                typeInfo[tp].tArray.valueTp = valueTp;
                typeInfo[tp].tArray.lengthConstant = lengthConstant;
                RESIZE_GLOBAL_BUFFER(constantInfo, constantCnt);
                /* Currently parse_enum_directive() has to resize the
                 * constantValue buffer along with constantInfo.
                 * For consistency we do that here, too. */
                RESIZE_GLOBAL_BUFFER(constantValue, constantCnt);
                constantInfo[lengthConstant].constantKind = CONSTANT_EXPRESSION;
                constantInfo[lengthConstant].symbol = (Symbol) -1;
                constantInfo[lengthConstant].scope = (Scope) -1;
                constantInfo[lengthConstant].tExpr = lengthExpr;
        }
        else if (look_token_kind(TOKEN_LEFTPAREN) != (Token) -1) {
                consume_token();
                tp = parse_type(0);
                parse_token_kind(TOKEN_RIGHTPAREN);
        }
        else if (look_token_kind(TOKEN_WORD) != (Token) -1) {
                Symref ref = parse_symref();
                tp = typeCnt++;
                RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
                typeInfo[tp].typeKind = TYPE_REFERENCE;
                typeInfo[tp].tRef.ref = ref;
                typeInfo[tp].tRef.resolvedTp = (Type) -1;
        }
        else {
                Token token = look_next_token();
                FATAL_PARSE_ERROR_AT_TOK(token, "Expected type\n");
        }

        if (prec > 0)
                return tp;

        while (look_token_kind(TOKEN_LEFTPAREN) != (Token) -1) {
                consume_token();
                Type proctp = typeCnt++;
                RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
                typeInfo[proctp].typeKind = TYPE_PROC;
                typeInfo[proctp].tProc.rettp = tp;
                typeInfo[proctp].tProc.nparams = 0;
                for (int rank = 0;; rank++) {
                        if (look_token_kind(TOKEN_RIGHTPAREN) != (Token) -1)
                                break;
                        Type ptp = parse_type(0);
                        Param param = paramCnt++;
                        RESIZE_GLOBAL_BUFFER(paramInfo, paramCnt);
                        paramInfo[param].proctp = proctp;
                        paramInfo[param].tp = ptp;
                        /* XXX this is fake. TODO change ParamInfo or require
                         * parameter name in syntax */
                        paramInfo[param].sym = (Symbol) 0;
                        paramInfo[param].rank = rank;
                        if (look_token_kind(TOKEN_COMMA) == (Token) -1)
                                break;
                        consume_token();
                }
                parse_token_kind(TOKEN_RIGHTPAREN);
                tp = proctp;
        }

        return tp;
}

INTERNAL
void parse_struct_member(Type structTp)
{
        Token tok = parse_token_kind(TOKEN_WORD);
        if (tokenInfo[tok].tWord.string != constStr[CONSTSTR_DATA])
                FATAL_PARSE_ERROR_AT_TOK(tok, "struct data member expected\n");
        Token nameToken = parse_token_kind(TOKEN_WORD);
        String memberName = tokenInfo[nameToken].tWord.string;
        Type memberTp = parse_type(0);
        parse_token_kind(TOKEN_SEMICOLON);
        Structmember y = structmemberCnt++;
        RESIZE_GLOBAL_BUFFER(structmemberInfo, structmemberCnt);
        structmemberInfo[y].structTp = structTp;
        structmemberInfo[y].memberName = memberName;
        structmemberInfo[y].memberTp = memberTp;
}

INTERNAL
Type parse_struct(void)
{
        PARSE_LOG();
        Token nameToken = parse_token_kind(TOKEN_WORD);
        String name = tokenInfo[nameToken].tWord.string;
        Type tp = typeCnt++;
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
        typeInfo[tp].typeKind = TYPE_STRUCT;
        typeInfo[tp].tStruct.name = name;
        add_type_symbol(name, globalScope, nameToken, tp);
        parse_token_kind(TOKEN_LEFTBRACE);
        while (look_token_kind(TOKEN_RIGHTBRACE) == -1)
                parse_struct_member(tp);
        parse_token_kind(TOKEN_RIGHTBRACE);
        return tp;
}

INTERNAL
Data parse_data(void)
{
        PARSE_LOG();
        Token nameToken = parse_token_kind(TOKEN_WORD);
        String name = tokenInfo[nameToken].tWord.string;
        Type tp = parse_type(0);
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
        symbolInfo[sym].symbolKind = SYMBOL_DATA;
        symbolInfo[sym].tData.tp = tp;
        symbolInfo[sym].tData.optionaldata = data;
        set_symbol_token(sym, nameToken);
        return data;
}

INTERNAL
Macro parse_macro(void)
{
        PARSE_LOG();
        Token nameToken = parse_token_kind(TOKEN_WORD);
        String name = tokenInfo[nameToken].tWord.string;
        Macro macro = macroCnt++;
        Symbol symbol = symbolCnt++;
        Scope scope = scopeCnt++;
        MacroParam firstMacroParam = macroParamCnt;
        int nparams = 0;
        RESIZE_GLOBAL_BUFFER(macroInfo, macroCnt);
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
        RESIZE_GLOBAL_BUFFER(scopeInfo, scopeCnt);
        push_scope(scope);

        if (look_token_kind(TOKEN_LEFTPAREN) != (Token) -1) {
                consume_token();
                while (look_token_kind(TOKEN_RIGHTPAREN) == -1) {
                        Token paramToken = parse_token_kind(TOKEN_WORD);
                        String paramname = tokenInfo[paramToken].tWord.string;
                        MacroParam param = macroParamCnt++;
                        Symbol paramsym = symbolCnt++;
                        RESIZE_GLOBAL_BUFFER(macroParamInfo, macroParamCnt);
                        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
                        macroParamInfo[param].macro = macro;
                        macroParamInfo[param].name = paramname;
                        symbolInfo[paramsym].name = paramname;
                        symbolInfo[paramsym].scope = scope;
                        symbolInfo[paramsym].symbolKind = SYMBOL_MACROPARAM;
                        symbolInfo[paramsym].tMacroParam = param;
                        set_symbol_token(paramsym, paramToken);
                        nparams++;
                        if (look_token_kind(TOKEN_COMMA) == -1)
                                break;
                        consume_token();
                }
                parse_token_kind(TOKEN_RIGHTPAREN);
                macroInfo[macro].macroKind = MACRO_FUNCTION;
                macroInfo[macro].tFunction.firstMacroParam = firstMacroParam;
                macroInfo[macro].tFunction.nparams = nparams;
        }
        else {
                macroInfo[macro].macroKind = MACRO_VALUE;
        }
        parse_token_kind(TOKEN_RIGHTARROW);
        Expr expr = parse_expr_without_currentProc();
        parse_token_kind(TOKEN_SEMICOLON);
        pop_scope();
        symbolInfo[symbol].name = name;
        symbolInfo[symbol].scope = currentScope;
        symbolInfo[symbol].symbolKind = SYMBOL_MACRO;
        symbolInfo[symbol].tMacro = macro;
        set_symbol_token(symbol, nameToken);
        scopeInfo[scope].parentScope = currentScope;
        scopeInfo[scope].firstSymbol = -1;
        scopeInfo[scope].numSymbols = 0;
        scopeInfo[scope].scopeKind = SCOPE_MACRO;
        scopeInfo[scope].tMacro = macro;
        macroInfo[macro].symbol = symbol;
        macroInfo[macro].scope = scope;
        macroInfo[macro].expr = expr;
        return macro;
}

INTERNAL Expr parse_compilercall_expr(Token hashtok, int compilercallKind)
{
        parse_token_kind(TOKEN_LEFTPAREN);
        Expr subexpr = parse_expr(0);
        parse_token_kind(TOKEN_RIGHTPAREN);
        Expr expr = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[expr].proc = currentProc;
        exprInfo[expr].exprKind = EXPR_COMPILERCALL;
        exprInfo[expr].tCompilercall.hashtok = hashtok;
        exprInfo[expr].tCompilercall.compilercallKind = compilercallKind;
        exprInfo[expr].tCompilercall.expr = subexpr;
        return expr;
}

INTERNAL Expr parse_compiler_expr(void)
{
        Token hashtok = look_next_token();
        consume_token();
        Token token = parse_token_kind(TOKEN_WORD);
        String string = tokenInfo[token].tWord.string;
        if (string == constStr[CONSTSTR_FILE])
                return add_compilervalue_expr(hashtok, COMPILERVALUE_FILE);
        else if (string == constStr[CONSTSTR_LINE])
                return add_compilervalue_expr(hashtok, COMPILERVALUE_LINE);
        else if (string == constStr[CONSTSTR_PROCNAME])
                return add_compilervalue_expr(hashtok, COMPILERVALUE_PROCNAME);
        else if (string == constStr[CONSTSTR_SIZEOF])
                return parse_compilercall_expr(hashtok, COMPILERCALL_SIZEOF);
        else if (string == constStr[CONSTSTR_LENGTHOF])
                return parse_compilercall_expr(hashtok, COMPILERCALL_LENGTHOF);
        else if (string == constStr[CONSTSTR_STRINGIFY])
                return parse_compilercall_expr(hashtok, COMPILERCALL_STRINGIFY);
        else
                FATAL_PARSE_ERROR_AT_TOK(token,
                        "Invalid compiler directive #%s\n", TS(token));
}

INTERNAL
Expr parse_expr(int minprec)
{
        PARSE_LOG();

        Expr expr;
        int opkind;
        Token tok = look_next_token();
        if (token_is_unary_prefix_operator(tok, &opkind)) {
                consume_token();
                Expr subexpr = parse_expr(42  /* TODO: unop precedence */);
                expr = add_unop_expr(opkind, tok, subexpr);
        }
        else if (tokenInfo[tok].tokenKind == TOKEN_HASH) {
                expr = parse_compiler_expr();
        }
        else if (tokenInfo[tok].tokenKind == TOKEN_WORD) {
                Symref ref = parse_symref();
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].proc = currentProc;
                exprInfo[expr].exprKind = EXPR_SYMREF;
                exprInfo[expr].tSymref.ref = ref;
        }
        else if (tokenInfo[tok].tokenKind == TOKEN_INTEGER) {
                consume_token();
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].proc = currentProc;
                exprInfo[expr].exprKind = EXPR_LITERAL;
                exprInfo[expr].tLiteral.tok = tok;
                exprInfo[expr].tLiteral.literalKind = LITERAL_INTEGER;
        }
        else if (tokenInfo[tok].tokenKind == TOKEN_FLOAT) {
                consume_token();
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].proc = currentProc;
                exprInfo[expr].exprKind = EXPR_LITERAL;
                exprInfo[expr].tLiteral.tok = tok;
                exprInfo[expr].tLiteral.literalKind = LITERAL_FLOAT;
        }
        else if (tokenInfo[tok].tokenKind == TOKEN_STRING) {
                consume_token();
                String string = tokenInfo[tok].tString.value;
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].proc = currentProc;
                exprInfo[expr].exprKind = EXPR_LITERAL;
                exprInfo[expr].tLiteral.tok = tok;
                exprInfo[expr].tLiteral.literalKind = LITERAL_STRING;
                exprInfo[expr].tLiteral.tString = string;
        }
        else if (tokenInfo[tok].tokenKind == TOKEN_CHARACTER) {
                consume_token();
                int value = tokenInfo[tok].tCharacter.value;
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].proc = currentProc;
                exprInfo[expr].exprKind = EXPR_LITERAL;
                exprInfo[expr].tLiteral.tok = tok;
                exprInfo[expr].tLiteral.literalKind = LITERAL_CHARACTER;
                exprInfo[expr].tLiteral.tCharacter = value;
        }
        else if (tokenInfo[tok].tokenKind == TOKEN_LEFTBRACE) {
                consume_token();
                expr = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[expr].proc = currentProc;
                exprInfo[expr].exprKind = EXPR_COMPOUND;
                exprInfo[expr].tCompound.initialToken = tok;
                exprInfo[expr].tCompound.firstCompoundExprLink = -1;
                exprInfo[expr].tCompound.numChilds = 0;
                while (look_token_kind(TOKEN_RIGHTBRACE) == (Token) -1) {
                        Expr child = parse_expr(0);
                        int cpe = compoundExprLinkCnt++;
                        RESIZE_GLOBAL_BUFFER(compoundExprLink,
                                             compoundExprLinkCnt);
                        compoundExprLink[cpe].parentExpr = expr;
                        compoundExprLink[cpe].childExpr = child;
                        if (look_token_kind(TOKEN_COMMA) == (Token) -1)
                                break;
                        consume_token();
                }
                parse_token_kind(TOKEN_RIGHTBRACE);
        }
        else if (tokenInfo[tok].tokenKind == TOKEN_LEFTPAREN) {
                consume_token();
                expr = parse_expr(0);
                parse_token_kind(TOKEN_RIGHTPAREN);
        }
        else {
                FATAL_PARSE_ERROR_AT_TOK(tok, "Expected expression\n");
        }

        for (;;) {
                tok = look_next_token();
                if (token_is_unary_postfix_operator(tok, &opkind)) {
                        consume_token();
                        expr = add_unop_expr(opkind, tok, expr);
                }
                else if (tokenInfo[tok].tokenKind == TOKEN_LEFTPAREN) {
                        consume_token();
                        Expr calleeExpr = expr;
                        expr = exprCnt++;
                        while (look_token_kind(TOKEN_RIGHTPAREN) == -1) {
                                Expr argExpr = parse_expr(0);
                                int x = callArgCnt++;
                                RESIZE_GLOBAL_BUFFER(callArgInfo, callArgCnt);
                                callArgInfo[x].callExpr = expr;
                                callArgInfo[x].argExpr = argExpr;
                                if (look_token_kind(TOKEN_COMMA) == -1)
                                        break;
                                consume_token();
                        }
                        parse_token_kind(TOKEN_RIGHTPAREN);
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].proc = currentProc;
                        exprInfo[expr].exprKind = EXPR_CALL;
                        exprInfo[expr].tCall.callee = calleeExpr;
                        exprInfo[expr].tCall.firstArgIdx = -1;
                        exprInfo[expr].tCall.nargs = 0;
                }
                else if (tokenInfo[tok].tokenKind == TOKEN_DOT) {
                        consume_token();
                        Token x = parse_token_kind(TOKEN_WORD);
                        String name = tokenInfo[x].tWord.string;
                        Expr enclosingExpr = expr;
                        expr = exprCnt++;
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].proc = currentProc;
                        exprInfo[expr].exprKind = EXPR_MEMBER;
                        exprInfo[expr].tMember.expr = enclosingExpr;
                        exprInfo[expr].tMember.name = name;
                }
                else if (tokenInfo[tok].tokenKind == TOKEN_LEFTBRACKET) {
                        consume_token();
                        Expr expr1 = expr;
                        Expr expr2 = parse_expr(0);
                        parse_token_kind(TOKEN_RIGHTBRACKET);
                        expr = exprCnt++;
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].proc = currentProc;
                        exprInfo[expr].exprKind = EXPR_SUBSCRIPT;
                        exprInfo[expr].tSubscript.expr1 = expr1;
                        exprInfo[expr].tSubscript.expr2 = expr2;
                }
                else if (token_is_binary_infix_operator(tok, &opkind)) {
                        int opprec = binopPrec[opkind];
                        if (opprec < minprec)
                                break;
                        consume_token();
                        Expr expr1 = expr;
                        Expr expr2 = parse_expr(opprec + 1);
                        expr = exprCnt++;
                        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                        exprInfo[expr].proc = currentProc;
                        exprInfo[expr].exprKind = EXPR_BINOP;
                        exprInfo[expr].tBinop.binopKind = opkind;
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

INTERNAL Stmt parse_imperative_statement(void);
INTERNAL Stmt parse_if_stmt(void);
INTERNAL Stmt parse_while_stmt(void);
INTERNAL Stmt parse_for_stmt(void);
INTERNAL Stmt parse_range_stmt(void);
INTERNAL Stmt parse_return_stmt(void);
INTERNAL Stmt parse_stmt(void);

INTERNAL
Stmt parse_data_stmt(void)
{
        PARSE_LOG();
        Data data = parse_data();
        Expr expr;
        if (look_token_kind(TOKEN_ASSIGNEQUALS) != (Token) -1) {
                consume_token();
                expr = parse_expr(0);
        }
        else {
                expr = (Expr) -1;
        }
        parse_token_kind(TOKEN_SEMICOLON);
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].stmtKind = STMT_DATA;
        stmtInfo[stmt].tData.data = data;
        stmtInfo[stmt].tData.optionalInitializerExpr = expr;
        return stmt;
}

INTERNAL
Stmt parse_macro_stmt(void)
{
        PARSE_LOG();
        Macro macro = parse_macro();
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].stmtKind = STMT_MACRO;
        stmtInfo[stmt].tMacro = macro;
        return stmt;
}

INTERNAL
Stmt parse_ignore_stmt(void)
{
        PARSE_LOG();
        Stmt stmt = stmtCnt++;
        Stmt substmt = parse_imperative_statement();
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].stmtKind = STMT_IGNORE;
        stmtInfo[stmt].tIgnore = substmt;
        return stmt;
}

INTERNAL
Stmt parse_expr_stmt_without_semicolon(void)
{
        PARSE_LOG();
        Expr expr = parse_expr(0);
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].stmtKind = STMT_EXPR;
        stmtInfo[stmt].tExpr.expr = expr;
        return stmt;
}

INTERNAL
Stmt parse_expr_stmt(void)
{
        PARSE_LOG();
        Stmt stmt = parse_expr_stmt_without_semicolon();
        parse_token_kind(TOKEN_SEMICOLON);
        return stmt;
}

INTERNAL
Stmt parse_compound_stmt(void)
{
        PARSE_LOG();
        Stmt stmt = stmtCnt++;
        Scope scope = scopeCnt++;
        parse_token_kind(TOKEN_LEFTBRACE);
        push_scope(scope);
        while (look_token_kind(TOKEN_RIGHTBRACE) == -1) {
                Stmt child = parse_stmt();
                int x = childStmtCnt++;
                RESIZE_GLOBAL_BUFFER(childStmtInfo, childStmtCnt);
                childStmtInfo[x].parent = stmt;
                childStmtInfo[x].child = child;
        }
        pop_scope();
        parse_token_kind(TOKEN_RIGHTBRACE);
        RESIZE_GLOBAL_BUFFER(scopeInfo, scopeCnt);
        scopeInfo[scope].parentScope = currentScope;
        scopeInfo[scope].firstSymbol = -1;
        scopeInfo[scope].numSymbols = 0;
        scopeInfo[scope].scopeKind = SCOPE_PROC; //XXX
        scopeInfo[scope].tProc = currentProc;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].stmtKind = STMT_COMPOUND;
        stmtInfo[stmt].tCompound.numStatements = 0;
        stmtInfo[stmt].tCompound.firstChildStmtIdx = -1;
        return stmt;
}

INTERNAL
Stmt parse_imperative_statement(void)
{
        PARSE_LOG();
        Token tok = look_next_token();
        if (tokenInfo[tok].tokenKind == TOKEN_WORD) {
                String s = tokenInfo[tok].tWord.string;
                if (s == constStr[CONSTSTR_IF]) {
                        consume_token();
                        return parse_if_stmt();
                }
                else if (s == constStr[CONSTSTR_WHILE]) {
                        consume_token();
                        return parse_while_stmt();
                }
                else if (s == constStr[CONSTSTR_FOR]) {
                        consume_token();
                        /* XXX: There are two possible syntactical forms
                         * following the 'for' keyword. */
                        if (look_token_kind(TOKEN_LEFTPAREN) != (Token) -1)
                                return parse_for_stmt();
                        else
                                return parse_range_stmt();

                }
                else if (s == constStr[CONSTSTR_RETURN]) {
                        consume_token();
                        return parse_return_stmt();
                }
                else {
                        return parse_expr_stmt();
                }
        }
        else if (tokenInfo[tok].tokenKind == TOKEN_HASH) {
                consume_token();
                tok = parse_token_kind(TOKEN_WORD);
                if (tokenInfo[tok].tWord.string == constStr[CONSTSTR_IGNORE])
                        return parse_ignore_stmt();
                else {
                        FATAL("Unsupported statement introducer: #%s\n",
                              string_buffer(tokenInfo[tok].tWord.string));
                }
        }
        else if (tokenInfo[tok].tokenKind == TOKEN_LEFTBRACE) {
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
        Stmt stmt = stmtCnt++;
        parse_token_kind(TOKEN_LEFTPAREN);
        Expr condExpr = parse_expr(0);
        parse_token_kind(TOKEN_RIGHTPAREN);
        Stmt ifbody = parse_imperative_statement();
        Token tok;
        if ((tok = look_token_kind(TOKEN_WORD)) != -1 &&
            tokenInfo[tok].tWord.string == constStr[CONSTSTR_ELSE]) {
                consume_token();
                Stmt elsebody = parse_imperative_statement();
                RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
                stmtInfo[stmt].stmtKind = STMT_IFELSE;
                stmtInfo[stmt].tIfelse.condExpr = condExpr;
                stmtInfo[stmt].tIfelse.ifbody = ifbody;
                stmtInfo[stmt].tIfelse.elsebody = elsebody;
        }
        else {
                RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
                stmtInfo[stmt].stmtKind = STMT_IF;
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
        stmtInfo[stmt].stmtKind = STMT_WHILE;
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
        Expr condExpr = parse_expr(0);
        parse_token_kind(TOKEN_SEMICOLON);
        Stmt stepStmt = parse_expr_stmt_without_semicolon();
        parse_token_kind(TOKEN_RIGHTPAREN);
        Stmt forbody = parse_imperative_statement();
        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);
        stmtInfo[stmt].stmtKind = STMT_FOR;
        stmtInfo[stmt].tFor.initStmt = initStmt;
        stmtInfo[stmt].tFor.condExpr = condExpr;
        stmtInfo[stmt].tFor.stepStmt = stepStmt;
        stmtInfo[stmt].tFor.forbody = forbody;
        return stmt;
}

INTERNAL
Stmt parse_range_stmt(void)
{
        PARSE_LOG();
        Token varnameToken = parse_token_kind(TOKEN_WORD);
        String varname = tokenInfo[varnameToken].tWord.string;
        {
                Token tok = parse_token_kind(TOKEN_WORD);
                String word = tokenInfo[tok].tWord.string;
                if (tokenInfo[tok].tWord.string != constStr[CONSTSTR_FROM])
                        FATAL_PARSE_ERROR_AT_TOK(tok,
                                "Keyword 'from' expected, got: %s\n",
                                string_buffer(word));
        }
        Expr startExpr = parse_expr(0);
        int directionIsDown;
        {
                Token tok = parse_token_kind(TOKEN_WORD);
                String word = tokenInfo[tok].tWord.string;
                if (word == constStr[CONSTSTR_TO])
                        directionIsDown = 0;
                else if (word == constStr[CONSTSTR_DOWNTO])
                        directionIsDown = 1;
                else
                        FATAL_PARSE_ERROR_AT_TOK(tok,
                                "Keyword 'to' or 'downto' expected, got: %s\n",
                                string_buffer(word));
        }
        Expr stopExpr = parse_expr(0);
        {
                Token tok = parse_token_kind(TOKEN_WORD);
                String word = tokenInfo[tok].tWord.string;
                if (tokenInfo[tok].tWord.string != constStr[CONSTSTR_DO])
                        FATAL_PARSE_ERROR_AT_TOK(tok,
                                "Keyword 'do' expected, got: %s\n",
                                string_buffer(word));
        }

        /* Make an extra scope containing the iteration variable */
        Scope scope = scopeCnt++;
        Data data = dataCnt++;
        Symbol sym = symbolCnt++;
        /* for now only iteration over integers is possible */
        Type dataTp = builtinType[BUILTINTYPE_INT];

        push_scope(scope);
        Stmt rangebody = parse_imperative_statement();
        pop_scope();

        Stmt stmt = stmtCnt++;
        RESIZE_GLOBAL_BUFFER(scopeInfo, scopeCnt);
        RESIZE_GLOBAL_BUFFER(dataInfo, dataCnt);
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
        RESIZE_GLOBAL_BUFFER(stmtInfo, stmtCnt);

        scopeInfo[scope].parentScope = currentScope;
        scopeInfo[scope].firstSymbol = -1;
        scopeInfo[scope].numSymbols = 0;
        scopeInfo[scope].scopeKind = SCOPE_PROC; //XXX
        scopeInfo[scope].tProc = currentProc;

        dataInfo[data].scope = scope;
        dataInfo[data].tp = dataTp;
        dataInfo[data].sym = sym;

        symbolInfo[sym].name = varname;
        symbolInfo[sym].scope = scope;
        symbolInfo[sym].symbolKind = SYMBOL_DATA;
        symbolInfo[sym].tData.tp = dataTp;
        symbolInfo[sym].tData.optionaldata = data;
        set_symbol_token(sym, varnameToken);

        stmtInfo[stmt].stmtKind = STMT_RANGE;
        stmtInfo[stmt].tRange.variable = data;
        stmtInfo[stmt].tRange.startExpr = startExpr;
        stmtInfo[stmt].tRange.stopExpr = stopExpr;
        stmtInfo[stmt].tRange.directionIsDown = directionIsDown;
        stmtInfo[stmt].tRange.rangebody = rangebody;

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
        stmtInfo[stmt].stmtKind = STMT_RETURN;
        stmtInfo[stmt].tReturn.expr = expr;
        return stmt;
}

INTERNAL
Stmt parse_stmt(void)
{
        PARSE_LOG();
        Token tok = look_next_token();
        if (tokenInfo[tok].tokenKind == TOKEN_WORD) {
                String s = tokenInfo[tok].tWord.string;
                if (s == constStr[CONSTSTR_DATA]) {
                        consume_token();
                        return parse_data_stmt();
                }
                else if (s == constStr[CONSTSTR_MACRO]) {
                        consume_token();
                        return parse_macro_stmt();
                }
        }
        return parse_imperative_statement();
}

INTERNAL
void parse_extern_directive(Directive directive)
{
        Token nameToken = parse_token_kind(TOKEN_WORD);
        String name = tokenInfo[nameToken].tWord.string;
        Type tp = parse_type(0);
        parse_token_kind(TOKEN_SEMICOLON);
        Type tt = referenced_type(tp);
        Symbol sym = symbolCnt++;
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
        symbolInfo[sym].name = name;
        symbolInfo[sym].scope = currentScope;
        switch (typeInfo[tt].typeKind) {
        case TYPE_POINTER:
        case TYPE_BASE: {
                symbolInfo[sym].symbolKind = SYMBOL_DATA;
                symbolInfo[sym].tData.tp = tp;
                symbolInfo[sym].tData.optionaldata = (Data) -1;
                break;
        }
        case TYPE_PROC: {
                symbolInfo[sym].symbolKind = SYMBOL_PROC;
                symbolInfo[sym].tProc.tp = tp;
                symbolInfo[sym].tProc.optionalproc = (Proc) -1;
                break;
        }
        default:
                UNHANDLED_CASE();
        }
        set_symbol_token(sym, nameToken);

        directiveInfo[directive].directiveKind = BUILTINDIRECTIVE_EXTERN;
        directiveInfo[directive].tExtern.symbol = sym;
}

INTERNAL
void parse_data_directive(Directive directive)
{
        Data data = parse_data();
        Expr expr;
        if (look_token_kind(TOKEN_ASSIGNEQUALS) != (Token) -1) {
                consume_token();
                expr = parse_expr(0);
        }
        else {
                expr = (Expr) -1;
        }
        parse_token_kind(TOKEN_SEMICOLON);
        directiveInfo[directive].directiveKind = BUILTINDIRECTIVE_DATA;
        directiveInfo[directive].tData.data = data;
        directiveInfo[directive].tData.optionalInitializerExpr = expr;
}

INTERNAL
void parse_struct_directive(Directive directive)
{
        Type tp = parse_struct();
        directiveInfo[directive].directiveKind = BUILTINDIRECTIVE_STRUCT;
        directiveInfo[directive].tStruct.tp = tp;
}


INTERNAL
void parse_macro_directive(Directive directive)
{
        Macro macro = parse_macro();
        directiveInfo[directive].directiveKind = BUILTINDIRECTIVE_MACRO;
        directiveInfo[directive].tMacro = macro;
}

INTERNAL
void parse_constant_directive(Directive directive)
{
        PARSE_LOG();
        Symbol symbol = symbolCnt++;
        Constant constant = constantCnt++;
        Token nameToken = parse_token_kind(TOKEN_WORD);
        String name = tokenInfo[nameToken].tWord.string;
        parse_token_kind(TOKEN_ASSIGNEQUALS);
        Expr expr;
        {
                Proc proc = currentProc;
                currentProc = (Proc) -1; // constant expr shouldn't have a proc
                expr = parse_expr(0);
                ASSERT(exprInfo[expr].proc == (Proc) -1);
                currentProc = proc;
        }
        parse_token_kind(TOKEN_SEMICOLON);
        RESIZE_GLOBAL_BUFFER(scopeInfo, scopeCnt);
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
        RESIZE_GLOBAL_BUFFER(constantInfo, constantCnt);
        /* Currently parse_enum_directive() has to resize the constantValue
        buffer along with constantInfo. For consistency we do that here, too. */
        RESIZE_GLOBAL_BUFFER(constantValue, constantCnt);
        symbolInfo[symbol].name = name;
        symbolInfo[symbol].scope = currentScope;
        symbolInfo[symbol].symbolKind = SYMBOL_CONSTANT;
        symbolInfo[symbol].tConstant = constant;
        set_symbol_token(symbol, nameToken);
        constantInfo[constant].constantKind = CONSTANT_EXPRESSION;
        constantInfo[constant].symbol = symbol;
        constantInfo[constant].scope = currentScope;
        constantInfo[constant].tExpr = expr;
        directiveInfo[directive].directiveKind = BUILTINDIRECTIVE_CONSTANT;
        directiveInfo[directive].tConstant = constant;
}

INTERNAL
void parse_enum_directive(Directive directive)
{
        PARSE_LOG();
        int size = 0;
        parse_token_kind(TOKEN_LEFTBRACE);
        while (look_token_kind(TOKEN_RIGHTBRACE) == (Token) -1) {
                Token nameToken = parse_token_kind(TOKEN_WORD);
                String name = tokenInfo[nameToken].tWord.string;
                parse_token_kind(TOKEN_SEMICOLON);
                int value = size++;
                Symbol symbol = symbolCnt++;
                Constant constant = constantCnt++;
                RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
                RESIZE_GLOBAL_BUFFER(constantInfo, constantCnt);
                RESIZE_GLOBAL_BUFFER(constantValue, constantCnt);
                symbolInfo[symbol].name = name;
                symbolInfo[symbol].scope = currentScope;
                symbolInfo[symbol].symbolKind = SYMBOL_CONSTANT;
                symbolInfo[symbol].tConstant = constant;
                set_symbol_token(symbol, nameToken);
                constantInfo[constant].constantKind = CONSTANT_INTEGER;
                constantInfo[constant].symbol = symbol;
                constantInfo[constant].scope = currentScope;
                constantInfo[constant].tExpr = (Expr) -1;
                constantValue[constant].valueKind = VALUE_INTEGER;
                constantValue[constant].tInteger = value;
        }
        parse_token_kind(TOKEN_RIGHTBRACE);

        directiveInfo[directive].directiveKind = BUILTINDIRECTIVE_ENUM;
        // TODO: DirectiveInfo lacks a structure to bind the enum constants
        // together
}

INTERNAL
void parse_proc_directive(Directive directive)
{
        PARSE_LOG();

        Token pnameToken = parse_token_kind(TOKEN_WORD);
        String pname = tokenInfo[pnameToken].tWord.string;

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

        currentProc = proc;  // "push proc"
        push_scope(pscope);

        parse_token_kind(TOKEN_LEFTPAREN);
        for (int rank = 0; ; rank++) {
                Token tok = look_next_token();
                if (tokenInfo[tok].tokenKind == TOKEN_RIGHTPAREN)
                        break;
                Token paramnameToken = parse_token_kind(TOKEN_WORD);
                String paramname = tokenInfo[paramnameToken].tWord.string;
                Type paramtp = parse_type(0);
                Param param = paramCnt++;
                Symbol paramsym = symbolCnt++;
                Data paramdata = dataCnt++;
                RESIZE_GLOBAL_BUFFER(paramInfo, paramCnt);
                RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
                RESIZE_GLOBAL_BUFFER(dataInfo, dataCnt);
                paramInfo[param].proctp = ptype;
                paramInfo[param].tp = paramtp;
                paramInfo[param].sym = paramsym;
                paramInfo[param].rank = rank;
                symbolInfo[paramsym].name = paramname;
                symbolInfo[paramsym].scope = pscope;
                symbolInfo[paramsym].symbolKind = SYMBOL_DATA;
                symbolInfo[paramsym].tData.tp = paramtp;
                symbolInfo[paramsym].tData.optionaldata = paramdata;
                set_symbol_token(paramsym, paramnameToken);
                dataInfo[paramdata].scope = pscope;
                dataInfo[paramdata].tp = paramtp;
                dataInfo[paramdata].sym = paramsym;
                if (look_token_kind(TOKEN_COMMA) == -1)
                        break;
                consume_token();
        }
        parse_token_kind(TOKEN_RIGHTPAREN);

        Type rettp = parse_type(0);
        Stmt pbody = parse_compound_stmt();
        pop_scope();

        ASSERT(currentScope == parentScope);
        ASSERT(currentProc == proc);
        scopeInfo[pscope].parentScope = currentScope;
        scopeInfo[pscope].firstSymbol = -1;
        scopeInfo[pscope].numSymbols = 0;
        scopeInfo[pscope].scopeKind = SCOPE_PROC;
        scopeInfo[pscope].tProc = currentProc;
        procInfo[proc].sym = psym;
        procInfo[proc].scope = pscope;
        procInfo[proc].body = pbody;
        procToType[proc] = ptype;
        symbolInfo[psym].name = pname;
        symbolInfo[psym].scope = currentScope;
        symbolInfo[psym].symbolKind = SYMBOL_PROC;
        symbolInfo[psym].tProc.tp = ptype;
        symbolInfo[psym].tProc.optionalproc = currentProc;
        set_symbol_token(psym, pnameToken);
        typeInfo[ptype].typeKind = TYPE_PROC;
        typeInfo[ptype].tProc.rettp = rettp;
        typeInfo[ptype].tProc.nparams = 0;

        currentProc = (Proc) -1; // "pop proc"

        directiveInfo[directive].directiveKind = BUILTINDIRECTIVE_PROC;
        directiveInfo[directive].tProc = proc;
}

INTERNAL
void parse_export_directive(Directive directive)
{
        PARSE_LOG();
        Symref ref = parse_symref();
        parse_token_kind(TOKEN_SEMICOLON);
        Export export = exportCnt++;
        RESIZE_GLOBAL_BUFFER(exportInfo, exportCnt);
        exportInfo[export].ref = ref;

        directiveInfo[directive].directiveKind = BUILTINDIRECTIVE_EXPORT;
        directiveInfo[directive].tExport = export;
        /* TODO: Maybe we want to remove the "Export" data structure and be
        happy with just DirectiveInfo.tExport where we can store the ref */
}

INTERNAL
void parse_typealias_directive(Directive directive)
{
        Token nameToken = parse_token_kind(TOKEN_WORD);
        String name = tokenInfo[nameToken].tWord.string;
        parse_token_kind(TOKEN_ASSIGNEQUALS);
        Type type = parse_type(0);
        parse_token_kind(TOKEN_SEMICOLON);

        /* XXX Maybe later we want to distinguish between type symbols and
         * typealias symbols.  But for now, we simply make an ordinary type
         * symbol. */
        Symbol symbol = add_type_symbol(name, currentScope, nameToken, type);

        directiveInfo[directive].directiveKind = BUILTINDIRECTIVE_TYPEALIAS;
        directiveInfo[directive].tTypealias.symbol = symbol;
}

void parse_file(File file)
{
        currentFile = file;
        currentOffset = 0;
        push_scope(globalScope);
        while (look_next_token() != -1) {
                Token tok = parse_token_kind(TOKEN_WORD);
                String s = tokenInfo[tok].tWord.string;

                int kind = 0;
                while (kind < directiveKindCnt &&
                       s != directiveKindInfo[kind].keyword)
                        kind++;
                if (kind == directiveKindCnt)
                        FATAL_PARSE_ERROR_AT_TOK(tok,
                                    "Unexpected word %s\n", TS(tok));
                ASSERT( directiveKindInfo[kind].parser );

                Directive directive = directiveCnt++;
                RESIZE_GLOBAL_BUFFER(directiveInfo, directiveCnt);

                /* parse! */
                directiveKindInfo[kind].parser(directive);
        }
        pop_scope();
        currentFile = (File) -1;
}


INTERNAL
int compare_Symbol(const void *a, const void *b)
{
        Symbol x = *(const Symbol *) a;
        Symbol y = *(const Symbol *) b;
        Scope s1 = symbolInfo[x].scope;
        Scope s2 = symbolInfo[y].scope;
        if (s1 != s2)
                return s1 - s2;
        String n1 = symbolInfo[x].name;
        String n2 = symbolInfo[y].name;
        return (n2 < n1) - (n1 < n2);
}

INTERNAL
int compare_ParamInfo(const void *a, const void *b)
{
        const struct ParamInfo *x = a;
        const struct ParamInfo *y = b;
        if (x->proctp != y->proctp)
                return x->proctp - y->proctp;
        return x->rank - y->rank;
}

INTERNAL
int compare_ChildStmtInfo(const void *a, const void *b)
{
        const struct ChildStmtInfo *x = a;
        const struct ChildStmtInfo *y = b;
        if (x->parent != y->parent)
                return x->parent - y->parent;
        return x->child - y->child;
}

INTERNAL
int compare_CallArgInfo(const void *a, const void *b)
{
        const struct CallArgInfo *x = a;
        const struct CallArgInfo *y = b;
        if (x->callExpr != y->callExpr)
                return x->callExpr - y->callExpr;
        return x->argExpr - y->argExpr;
}

INTERNAL
int compare_CompoundExprLink(const void *a, const void *b)
{
        const struct CompoundExprLink *x = a;
        const struct CompoundExprLink *y = b;
        if (x->parentExpr != y->parentExpr)
                return x->parentExpr - y->parentExpr;
        return x->childExpr - y->childExpr;
}

/*
 * Post-parsing stage.
 *
 * Sort a few items to a particular efficient ordering, and rename the
 * respective cross-references from other structures.
 *
 * Setup simple indices for O(1) access instead of O(n). Sometimes also compute
 * run counts.
 */
void fixup_parsed_data(void)
{
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
                for (Proc i = 0; i < procCnt; i++)
                        procInfo[i].sym = newname[procInfo[i].sym];
                for (Param i = 0; i < paramCnt; i++)
                        paramInfo[i].sym = newname[paramInfo[i].sym];
                for (Constant i = 0; i < constantCnt; i++)
                        constantInfo[i].symbol = newname[constantInfo[i].symbol];
                for (Symbol i = 0; i < symbolCnt; i++) {
                        Symbol j = newname[i];
                        while (j != i) {
                                {
                                struct SymbolInfo tmp = symbolInfo[i];
                                symbolInfo[i] = symbolInfo[j];
                                symbolInfo[j] = tmp;
                                }
                                {
                                Token tmp = symbolToToken[i];
                                symbolToToken[i] = symbolToToken[j];
                                symbolToToken[j] = tmp;
                                }
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

        RESIZE_GLOBAL_BUFFER(firstProctypeParam, typeCnt);
        for (Type i = 0; i < typeCnt; i++) {
                firstProctypeParam[i] = 0; /* some proctypes do not have params.
                                              we should initialize the value
                                              nevertheless */
                if (typeInfo[i].typeKind == TYPE_PROC)
                        typeInfo[i].tProc.nparams = 0;
        }
        for (Param param = paramCnt; param --> 0;) {
                Type proctp = paramInfo[param].proctp;
                ASSERT(0 <= proctp && proctp < typeCnt);
                ASSERT(typeInfo[proctp].typeKind == TYPE_PROC);
                firstProctypeParam[proctp] = param;
                typeInfo[proctp].tProc.nparams ++;
        }

        sort_array(childStmtInfo, childStmtCnt, sizeof *childStmtInfo,
                   compare_ChildStmtInfo);
        sort_array(callArgInfo, callArgCnt, sizeof *callArgInfo,
                   compare_CallArgInfo);
        sort_array(compoundExprLink, compoundExprLinkCnt,
                   sizeof *compoundExprLink, compare_CompoundExprLink);

        for (int i = childStmtCnt; i --> 0;) {
                Stmt parent = childStmtInfo[i].parent;
                ASSERT(stmtInfo[parent].stmtKind == STMT_COMPOUND);
                stmtInfo[parent].tCompound.numStatements++;
                stmtInfo[parent].tCompound.firstChildStmtIdx = i;
        }

        for (int i = callArgCnt; i --> 0;) {
                Expr callee = callArgInfo[i].callExpr;
                ASSERT(exprInfo[callee].exprKind == EXPR_CALL);
                exprInfo[callee].tCall.nargs++;
                exprInfo[callee].tCall.firstArgIdx = i;
        }

        for (int i = compoundExprLinkCnt; i --> 0;) {
                Expr parentExpr = compoundExprLink[i].parentExpr;
                ASSERT(exprInfo[parentExpr].exprKind == EXPR_COMPOUND);
                exprInfo[parentExpr].tCompound.numChilds++;
                exprInfo[parentExpr].tCompound.firstCompoundExprLink = i;
        }

        for (Scope scope = 0; scope < scopeCnt; scope++) {
                scopeInfo[scope].numSymbols = 0;
                scopeInfo[scope].firstSymbol = 0;
        }
        for (Symbol i = symbolCnt; i --> 0;) {
                scopeInfo[symbolInfo[i].scope].numSymbols++;
                scopeInfo[symbolInfo[i].scope].firstSymbol = i;
        }

        for (Type t = 0; t < typeCnt; t++) {
                if (typeInfo[t].typeKind == TYPE_STRUCT) {
                        typeInfo[t].tStruct.firstStructmember = -1;
                        typeInfo[t].tStruct.numMembers = 0;
                }
        }
        for (Structmember m = structmemberCnt; m --> 0;) {
                Type t = structmemberInfo[m].structTp;
                typeInfo[t].tStruct.firstStructmember = m;
                typeInfo[t].tStruct.numMembers ++;
        }
}


/* initializer for directiveKindInfo */
const struct BuiltinDirectiveKindInfo builtinDirectiveKindInfo[] = {
        [BUILTINDIRECTIVE_EXTERN]    = { CONSTSTR_EXTERN,    parse_extern_directive },
        [BUILTINDIRECTIVE_DATA]      = { CONSTSTR_DATA,      parse_data_directive },
        [BUILTINDIRECTIVE_STRUCT]    = { CONSTSTR_STRUCT,    parse_struct_directive },
        [BUILTINDIRECTIVE_PROC]      = { CONSTSTR_PROC,      parse_proc_directive },
        [BUILTINDIRECTIVE_MACRO]     = { CONSTSTR_MACRO,     parse_macro_directive },
        [BUILTINDIRECTIVE_ENUM]      = { CONSTSTR_ENUM,      parse_enum_directive },
        [BUILTINDIRECTIVE_CONSTANT]  = { CONSTSTR_CONSTANT,  parse_constant_directive },
        [BUILTINDIRECTIVE_EXPORT]    = { CONSTSTR_EXPORT,    parse_export_directive },
        [BUILTINDIRECTIVE_TYPEALIAS] = { CONSTSTR_TYPEALIAS, parse_typealias_directive },
};
const int builtinDirectiveKindCnt = LENGTH(builtinDirectiveKindInfo);
