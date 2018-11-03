#include "defs.h"
#include "api.h"

const char lvl_debug[] = "DEBUG";
const char lvl_info[] = "INFO";
const char lvl_warn[] = "WARN";
const char lvl_error[] = "ERROR";
const char lvl_fatal[] = "FATAL";

/* offset may be 1 past the end of file (i.e., equal to file size) */
int compute_lineno(File file, int offset)
{
        int i;
        int line = 1;

        for (i = 0; i < offset; i++)
                if (fileInfo[file].buf[i] == '\n')
                        line++;
        return line;
}

/* offset may be 1 past the end of file (i.e., equal to file size) */
int compute_colno(File file, int offset)
{
        int i;
        int column = 1;

        for (i = 0; i < offset; i++)
                if (fileInfo[file].buf[i] == '\n')
                        column = 1;
                else
                        column ++;
        return column;
}

void find_expr_position(Expr x, File *file, int *offset)
{
        // TODO: should this be added as hard data to ExprInfo?
        Token tok = -1;
        for (;;) {
                switch (exprInfo[x].kind) {
                case EXPR_LITERAL:
                        tok = exprInfo[x].tLiteral.tok;
                        break;
                case EXPR_SYMREF:
                        tok = symrefInfo[exprInfo[x].tSymref.ref].tok;
                        break;
                case EXPR_UNOP:
                        tok = exprInfo[x].tUnop.tok;
                        break;
                case EXPR_BINOP:
                        x = exprInfo[x].tBinop.expr1;
                        continue;
                case EXPR_MEMBER:
                        x = exprInfo[x].tMember.expr;
                        continue;
                case EXPR_SUBSCRIPT:
                        x = exprInfo[x].tSubscript.expr1;
                        continue;
                case EXPR_CALL:
                        x = exprInfo[x].tCall.callee;
                        continue;
                default:
                        UNHANDLED_CASE();
                }
                break;
        }
        assert(tok != -1);
        *file = tokenInfo[tok].file;
        *offset = tokenInfo[tok].offset;
}

void _msg_at_v(const char *srcfilename, int srcline,
             const char *lvl, File file, int offset,
             const char *fmt, va_list ap)
{
        const char *filepath = string_buffer(fileInfo[file].filepath);
        int line = compute_lineno(file, offset);
        int col = compute_colno(file, offset);
        _msg_begin(srcfilename, srcline, lvl);
        _msg_printf("At %s %d:%d: ", filepath, line, col);
        _msg_printfv(fmt, ap);
        _msg_end();
}

void _msg_at(const char *srcfilename, int srcline,
             const char *lvl, File file, int offset,
             const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        _msg_at_v(srcfilename, srcline, lvl, file, offset, fmt, ap);
        va_end(ap);
}

void _msg_at_tok(const char *srcfilename, int srcline,
                 const char *lvl, Token tok,
                 const char *fmt, ...)
{
        File file = tokenInfo[tok].file;
        int offset = tokenInfo[tok].offset;
        va_list ap;
        va_start(ap, fmt);
        _msg_at_v(srcfilename, srcline, lvl, file, offset, fmt, ap);
        va_end(ap);
}

void _msg_at_expr(const char *srcfilename, int srcline,
                 const char *lvl, Expr expr,
                 const char *fmt, ...)
{
        va_list ap;
        int file;
        int offset;

        find_expr_position(expr, &file, &offset);
        va_start(ap, fmt);
        _msg_at_v(srcfilename, srcline, lvl, file, offset, fmt, ap);
        va_end(ap);
}

#define MSG_AT(...) _msg_at(__FILE__, __LINE__, __VA_ARGS__)
#define MSG_AT_TOK(...) _msg_at_tok(__FILE__, __LINE__, __VA_ARGS__)
#define MSG_AT_EXPR(...) _msg_at_expr(__FILE__, __LINE__, __VA_ARGS__)

#define FATAL_PARSE_ERROR_AT(...) \
        do { MSG_AT(lvl_fatal, __VA_ARGS__); ABORT(); } while (0)
#define FATAL_PARSE_ERROR_AT_TOK(...) \
        do { MSG_AT_TOK(lvl_fatal, __VA_ARGS__); ABORT(); } while (0)
#define LOG_TYPE_ERROR_EXPR(...) MSG_AT_EXPR(lvl_error, __VA_ARGS__)
#define LOG_TYPE_ERROR_EXPR(...) MSG_AT_EXPR(lvl_error, __VA_ARGS__)

#define PARSE_LOG() \
        if (doDebug) \
                MSG_AT(lvl_debug, currentFile, currentOffset, \
                       "%s()\n", __func__);

File add_file(String filepath)
{
        File x = fileCnt++;
        BUF_RESERVE(fileInfo, fileInfoAlloc, fileCnt);
        fileInfo[x].filepath = filepath;
        read_whole_file(x);
        return x;
}

Token add_word_token(File file, int offset, String string)
{
        Token x = tokenCnt++;
        BUF_RESERVE(tokenInfo, tokenInfoAlloc, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = TOKTYPE_WORD;
        tokenInfo[x].tWord.string = string;
        return x;
}

Token add_integer_token(File file, int offset, long long value)
{
        Token x = tokenCnt++;
        BUF_RESERVE(tokenInfo, tokenInfoAlloc, tokenCnt);
        tokenInfo[x].file = file;
        tokenInfo[x].offset = offset;
        tokenInfo[x].kind = TOKTYPE_INTEGER;
        tokenInfo[x].tInteger.value = value;
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

Type add_base_type(String name, int size)
{
        Type x = typeCnt++;
        BUF_RESERVE(typeInfo, typeInfoAlloc, typeCnt);
        typeInfo[x].kind = TYPE_BASE;
        typeInfo[x].tBase.name = name;
        typeInfo[x].tBase.size = size;
        return x;
}

Type add_entity_type(String name, Type tp)
{
        Type x = typeCnt++;
        BUF_RESERVE(typeInfo, typeInfoAlloc, typeCnt);
        typeInfo[x].kind = TYPE_ENTITY;
        typeInfo[x].tEntity.name = name;
        typeInfo[x].tEntity.tp = tp;
        return x;
}

Type add_array_type(Type idxtp, Type valuetp)
{
        Type x = typeCnt++;
        BUF_RESERVE(typeInfo, typeInfoAlloc, typeCnt);
        typeInfo[x].kind = TYPE_ARRAY;
        typeInfo[x].tArray.idxtp = idxtp;
        typeInfo[x].tArray.valuetp = valuetp;
        return x;
}

Type add_pointer_type(Type tp)
{
        Type x = typeCnt++;
        BUF_RESERVE(typeInfo, typeInfoAlloc, typeCnt);
        typeInfo[x].kind = TYPE_POINTER;
        typeInfo[x].tPointer.tp = tp;
        return x;
}

Type add_proc_type(Type rettp, int nargs, int firstParamtype)
{
        Type x = typeCnt++;
        BUF_RESERVE(typeInfo, typeInfoAlloc, typeCnt);
        typeInfo[x].kind = TYPE_PROC;
        typeInfo[x].tProc.rettp = rettp;
        typeInfo[x].tProc.nargs = nargs;
        typeInfo[x].tProc.firstParamtype = firstParamtype;
        return x;
}

Type add_ref_type(Symref ref)
{
        Type x = typeCnt++;
        BUF_RESERVE(typeInfo, typeInfoAlloc, typeCnt);
        typeInfo[x].kind = TYPE_REFERENCE;
        typeInfo[x].tRef.ref = ref;
        typeInfo[x].tRef.resolvedTp = -1;
        return x;
}

Type add_paramtype(Type proctp, Type argtp)
{
        int x = paramtypeCnt++;
        BUF_RESERVE(paramtypeInfo, paramtypeInfoAlloc, paramtypeCnt);
        paramtypeInfo[x].proctp = proctp;
        paramtypeInfo[x].argtp = argtp;
        paramtypeInfo[x].rank = x;
        return x;
}

Symbol add_type_symbol(String name, Scope scope, Type tp)
{
        Symbol x = symbolCnt++;
        BUF_RESERVE(symbolInfo, symbolInfoAlloc, symbolCnt);
        symbolInfo[x].name = name;
        symbolInfo[x].scope = scope;
        symbolInfo[x].kind = SYMBOL_TYPE;
        symbolInfo[x].tType = tp;
        return x;
}

Symbol add_data_symbol(String name, Scope scope, Data data)
{
        Symbol x = symbolCnt++;
        BUF_RESERVE(symbolInfo, symbolInfoAlloc, symbolCnt);
        symbolInfo[x].name = name;
        symbolInfo[x].scope = scope;
        symbolInfo[x].kind = SYMBOL_DATA;
        symbolInfo[x].tData = data;
        return x;
}

Symbol add_array_symbol(String name, Scope scope, Array array)
{
        Symbol x = symbolCnt++;
        BUF_RESERVE(symbolInfo, symbolInfoAlloc, symbolCnt);
        symbolInfo[x].name = name;
        symbolInfo[x].scope = scope;
        symbolInfo[x].kind = SYMBOL_ARRAY;
        symbolInfo[x].tArray = array;
        return x;
}

Symbol add_proc_symbol(String name, Scope scope, Proc proc)
{
        Symbol x = symbolCnt++;
        BUF_RESERVE(symbolInfo, symbolInfoAlloc, symbolCnt);
        symbolInfo[x].name = name;
        symbolInfo[x].scope = scope;
        symbolInfo[x].kind = SYMBOL_PROC;
        symbolInfo[x].tProc = proc;
        return x;
}

Symbol add_param_symbol(String name, Scope scope, Param param)
{
        Symbol x = symbolCnt++;
        BUF_RESERVE(symbolInfo, symbolInfoAlloc, symbolCnt);
        symbolInfo[x].name = name;
        symbolInfo[x].scope = scope;
        symbolInfo[x].kind = SYMBOL_PARAM;
        symbolInfo[x].tParam = param;
        return x;
}

Array add_array(Scope scope, Type tp)
{
        Array x = arrayCnt++;
        BUF_RESERVE(arrayInfo, arrayInfoAlloc, arrayCnt);
        arrayInfo[x].scope = scope;
        arrayInfo[x].tp = tp;
        arrayInfo[x].sym = -1; // later
        return x;
}

Data add_data(Scope scope, Type tp)
{
        Data x = dataCnt++;
        BUF_RESERVE(dataInfo, dataInfoAlloc, dataCnt);
        dataInfo[x].scope = scope;
        dataInfo[x].tp = tp;
        dataInfo[x].sym = -1;  // later
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

Proc add_proc(Type tp, Scope scope)
{
        Proc x = procCnt++;
        BUF_RESERVE(procInfo, procInfoAlloc, procCnt);
        procInfo[x].tp = tp;
        procInfo[x].sym = -1; // later
        procInfo[x].scope = scope;
        procInfo[x].firstParam = -1;
        procInfo[x].nparams = 0;
        procInfo[x].body = -1;
        return x;
}

Symref add_symref(Token tok, Scope refScope)
{
        Symref ref = symrefCnt++;
        BUF_RESERVE(symrefInfo, symrefInfoAlloc, symrefCnt);
        symrefInfo[ref].name = tokenInfo[tok].tWord.string;
        symrefInfo[ref].refScope = refScope;
        symrefInfo[ref].tok = tok;
        return ref;
}

Expr add_symref_expr(Symref ref)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_SYMREF;
        exprInfo[x].tSymref.ref = ref;
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

Expr add_unop_expr(int opkind, Token tok, Expr expr)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_UNOP;
        exprInfo[x].tUnop.kind = opkind;
        exprInfo[x].tUnop.tok = tok;
        exprInfo[x].tUnop.expr = expr;
        return x;
}

Expr add_binop_expr(int opkind, Token tok, Expr expr1, Expr expr2)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
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
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_MEMBER;
        exprInfo[x].tMember.expr = expr;
        exprInfo[x].tMember.name = name;
        return x;
}

Expr add_subscript_expr(Expr expr1, Expr expr2)
{
        Expr x = exprCnt++;
        BUF_RESERVE(exprInfo, exprInfoAlloc, exprCnt);
        exprInfo[x].kind = EXPR_SUBSCRIPT;
        exprInfo[x].tSubscript.expr1 = expr1;
        exprInfo[x].tSubscript.expr2 = expr2;
        return x;
}

Stmt add_data_stmt(Data data)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_DATA;
        stmtInfo[stmt].tData = data;
        return stmt;
}

Stmt add_array_stmt(Array array)
{
        Stmt stmt = stmtCnt++;
        BUF_RESERVE(stmtInfo, stmtInfoAlloc, stmtCnt);
        stmtInfo[stmt].kind = STMT_ARRAY;
        stmtInfo[stmt].tArray = array;
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

Param add_Param(Proc proc, Type tp)
{
        Param x = paramCnt++;
        BUF_RESERVE(paramInfo, paramInfoAlloc, paramCnt);
        paramInfo[x].proc = proc;
        paramInfo[x].sym = -1; // later
        paramInfo[x].tp = tp;
        paramInfo[x].rank = x;
        return x;
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

void compiler_init(void)
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

void compiler_exit(void)
{
        for (File i = 0; i < fileCnt; i++)
                BUF_EXIT(fileInfo[i].buf, fileInfo[i].bufAlloc);
        BUF_EXIT(lexbuf, lexbufAlloc);
        BUF_EXIT(strbuf, strbufAlloc);
        BUF_EXIT(stringInfo, stringInfoAlloc);
        BUF_EXIT(strBucketInfo, strBucketInfoAlloc);
        BUF_EXIT(fileInfo, fileInfoAlloc);
        BUF_EXIT(tokenInfo, tokenInfoAlloc);
        BUF_EXIT(typeInfo, typeInfoAlloc);
        BUF_EXIT(paramtypeInfo, paramtypeInfoAlloc);
        BUF_EXIT(symbolInfo, symbolInfoAlloc);
        BUF_EXIT(dataInfo, dataInfoAlloc);
        BUF_EXIT(arrayInfo, arrayInfoAlloc);
        BUF_EXIT(scopeInfo, scopeInfoAlloc);
        BUF_EXIT(procInfo, procInfoAlloc);
        BUF_EXIT(paramInfo, paramInfoAlloc);
        BUF_EXIT(symrefInfo, symrefInfoAlloc);
        BUF_EXIT(exprInfo, exprInfoAlloc);
        BUF_EXIT(stmtInfo, stmtInfoAlloc);
        BUF_EXIT(childStmtInfo, childStmtInfoAlloc);
        BUF_EXIT(callArgInfo, callArgInfoAlloc);
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
                        BUF_RESERVE(lexbuf, lexbufAlloc, lexbufCnt);
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
        Type idxtp;
        Type valuetp;
        Type tp;
        Array array;
        String name;
        Symbol sym;

        PARSE_LOG();
        valuetp = parse_type();
        name = parse_name();
        parse_token_kind(TOKTYPE_LEFTBRACKET);
        idxtp = parse_type();
        tp = add_array_type(idxtp, valuetp);
        array = add_array(currentScope, tp);
        sym = add_array_symbol(name, currentScope, array);
        arrayInfo[array].sym = sym;
        parse_token_kind(TOKTYPE_RIGHTBRACKET);
        parse_token_kind(TOKTYPE_SEMICOLON);
        add_type_symbol(name, currentScope, tp);
        return array;
}

Data parse_data(void)
{
        Type tp;
        String name;
        Symbol sym;
        Data data;

        PARSE_LOG();
        tp = parse_type();
        name = parse_name();
        data = add_data(currentScope, tp);
        sym = add_data_symbol(name, currentScope, data);
        dataInfo[data].sym = sym;
        parse_token_kind(TOKTYPE_SEMICOLON);
        return data;
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
                                subexpr = parse_expr(0);
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
                        subexpr = parse_expr(0);
                        parse_token_kind(TOKTYPE_RIGHTBRACKET);
                        expr = add_subscript_expr(expr, subexpr);
                }
                else if (token_is_binary_infix_operator(tok, &opkind)) {
                        opprec = binopInfo[opkind].prec;
                        if (opprec < minprec)
                                break;
                        parse_next_token();
                        subexpr = parse_expr(opprec + 1);
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
        return add_data_stmt(data);
}

Stmt parse_array_stmt(void)
{
        PARSE_LOG();
        Array array = parse_array();
        return add_array_stmt(array);
}

Stmt parse_expr_stmt(void)
{
        PARSE_LOG();
        Expr expr = parse_expr(0);
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
        if (tokenInfo[tok].kind == TOKTYPE_LEFTBRACE)
                return parse_compound_stmt();
        else
                return parse_expr_stmt();
}

Stmt parse_if_stmt(void)
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
        Type rettp;  /* result type */
        String name;  /* proc name */
        Symbol psym;  /* proc symbol */
        Scope pscope; /* proc scope */
        Proc proc;
        Token tok;
        Stmt body;

        PARSE_LOG();
        rettp = parse_type();
        name = parse_name();
        pscope = add_proc_scope(currentScope);
        proc = add_proc(rettp, pscope);
        psym = add_proc_symbol(name, currentScope, proc);
        procInfo[proc].sym = psym;
        scopeInfo[pscope].tProc.proc = proc;

        push_scope(pscope);
        parse_token_kind(TOKTYPE_LEFTPAREN);
        for (;;) {
                Param param;
                String paramname;
                Symbol paramsym;
                Type paramtp;

                tok = look_next_token();
                if (tokenInfo[tok].kind == TOKTYPE_RIGHTPAREN)
                        break;
                paramtp = parse_type();
                paramname = parse_name();
                param = add_Param(proc, paramtp);
                paramsym = add_param_symbol(paramname, pscope, param);
                paramInfo[param].sym = paramsym;
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
                if (s == constStr[CONSTSTR_ENTITY]) {
                        parse_entity();
                }
                else if (s == constStr[CONSTSTR_ARRAY]) {
                        parse_array();
                }
                else if (s == constStr[CONSTSTR_DATA]) {
                        parse_data();
                }
                else if (s == constStr[CONSTSTR_PROC]) {
                        parse_proc();
                }
                else {
                        FATAL_PARSE_ERROR_AT_TOK(tok,
                            "Unexpected word %s\n", TS(tok));
                }
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
                BUF_EXIT(order, orderAlloc);
                BUF_EXIT(newname, newnameAlloc);
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

Symbol find_symbol_in_scope(String name, Scope scope)
{
        //MSG("RESOLVE %s\n", string_buffer(name));
        for (; scope != -1; scope = scopeInfo[scope].parentScope) {
                Symbol first = scopeInfo[scope].firstSymbol;
                Symbol last = first + scopeInfo[scope].numSymbols;
                for (Symbol i = first; i < last; i++) {
                        if (symbolInfo[i].name == name) {
                                //MSG("FOUND symbol %s\n", string_buffer(name));
                                return i;
                        }
                }
        }
        return -1;
}

void resolve_symbol_references(void)
{
        for (Symref ref = 0; ref < symrefCnt; ref++) {
                String name = symrefInfo[ref].name;
                Scope refScope = symrefInfo[ref].refScope;
                Symbol sym = find_symbol_in_scope(name, refScope);
                if (sym < 0) {
                        MSG_AT_TOK("ERROR", symrefInfo[ref].tok,
                                   "unresolved symbol reference %s\n",
                                   string_buffer(name));
                }
                symrefInfo[ref].sym = sym;
        }
}

void resolve_ref_type(Type t)
{
        if (typeInfo[t].isComplete >= 0) {
                /* already processed */
                return;
        }
        if (typeInfo[t].isComplete == -1) {
                WARN("Type #%d: cyclic type reference\n", t);
                typeInfo[t].isComplete = 0;
                return;
        }

        const int unassigned = -42;

        int isComplete = unassigned;

        switch (typeInfo[t].kind) {
        case TYPE_BASE:
                isComplete = 1;
                break;
        case TYPE_ENTITY:
                resolve_ref_type(typeInfo[t].tEntity.tp);
                isComplete = typeInfo[typeInfo[t].tEntity.tp].isComplete;
                break;
        case TYPE_ARRAY:
                resolve_ref_type(typeInfo[t].tArray.idxtp);
                resolve_ref_type(typeInfo[t].tArray.valuetp);
                isComplete =
                        typeInfo[typeInfo[t].tArray.idxtp].isComplete &&
                        typeInfo[typeInfo[t].tArray.valuetp].isComplete;
                break;
        case TYPE_POINTER:
                resolve_ref_type(typeInfo[t].tPointer.tp);
                isComplete = typeInfo[typeInfo[t].tPointer.tp].isComplete;
                break;
        case TYPE_PROC:
                isComplete = 0;
                // TODO
                break;
        case TYPE_REFERENCE: {
                isComplete = 0;
                typeInfo[t].isComplete = -1;
                Symbol sym = symrefInfo[typeInfo[t].tRef.ref].sym;
                if (sym != -1 && symbolInfo[sym].kind == SYMBOL_TYPE) {
                        Type symtp = symbolInfo[sym].tType;
                        if (symtp != -1) {
                                resolve_ref_type(symtp);
                                isComplete = typeInfo[symtp].isComplete;
                        }
                        typeInfo[t].tRef.resolvedTp = symtp;
                }
                break;
        }
        default:
                UNHANDLED_CASE();
        }
        assert(isComplete != unassigned);
        typeInfo[t].isComplete = isComplete;
}

void resolve_type_references(void)
{
        /* isComplete -2 means "TO DO" */
        /* isComplete -1 means "currently resolving" */
        for (Type t = 0; t < typeCnt; t++)
                typeInfo[t].isComplete = -2;
        for (Type t = 0; t < typeCnt; t++)
                if (typeInfo[t].kind == TYPE_REFERENCE)
                        typeInfo[t].tRef.resolvedTp = -1;
        for (Type t = 0; t < typeCnt; t++)
                if (typeInfo[t].isComplete == -2)
                        resolve_ref_type(t);
        for (Type t = 0; t < typeCnt; t++) {
                assert(typeInfo[t].isComplete == 1 ||
                       typeInfo[t].isComplete == 0);
        }
}

int is_integral_type(Type t)
{
        return typeInfo[t].kind == TYPE_BASE; //XXX
}

int is_bad_type(Type t)
{
        assert(t >= 0);  // is this really true?
        return ! typeInfo[t].isComplete;
}

int type_equal(Type a, Type b)
{
        if (!typeInfo[a].isComplete)
                return 0;
        if (!typeInfo[b].isComplete)
                return 0;
        while (typeInfo[a].kind == TYPE_REFERENCE) {
                a = typeInfo[a].tRef.resolvedTp;
                assert(a != -1);
        }
        while (typeInfo[b].kind == TYPE_REFERENCE) {
                b = typeInfo[b].tRef.resolvedTp;
                assert(a != -1);
        }
        return a == b;
}

Type check_literal_expr_type(Expr x)
{
        //XXX
        Type tp = 0;
        exprInfo[x].tp = tp;
        return tp;
}

Type check_symref_expr_type(Expr x)
{
        Symref ref = exprInfo[x].tSymref.ref;
        // XXX: symbol resolved?
        Symbol sym = symrefInfo[ref].sym;
        Type tp = -1;
        if (sym == -1) {
                const char *name = string_buffer(symrefInfo[ref].name);
                LOG_TYPE_ERROR_EXPR(x,
                      "Can't check type: symbol \"%s\" unresolved\n", name);
                goto out;
        }
        switch (symbolInfo[sym].kind) {
                case SYMBOL_TYPE:
                        // Maybe something like the "type" type?
                        // Or fatal() ?
                        UNHANDLED_CASE();
                        break;
                case SYMBOL_DATA:
                        tp = dataInfo[symbolInfo[sym].tData].tp;
                        break;
                case SYMBOL_ARRAY:
                        tp = arrayInfo[symbolInfo[sym].tArray].tp;
                        break;
                case SYMBOL_PROC:
                        tp = procInfo[symbolInfo[sym].tProc].tp;
                        break;
                case SYMBOL_PARAM:
                        tp = paramInfo[symbolInfo[sym].tParam].tp;
                        break;
                default:
                        UNHANDLED_CASE();
                        break;
        }
out:
        exprInfo[x].tp = tp;
        return tp;
}

Type check_expr_type(Expr x);

Type check_unop_expr_type(Expr x)
{
        int op = exprInfo[x].tUnop.kind;
        Expr xx = exprInfo[x].tUnop.expr;
        Type tt = check_expr_type(xx);
        Type tp = -1;
        if (tt != -1) {
                switch (op) {
                case UNOP_INVERTBITS:
                case UNOP_NOT:
                case UNOP_ADDRESSOF:
                case UNOP_DEREF:
                case UNOP_NEGATIVE:
                case UNOP_POSITIVE:
                case UNOP_PREDECREMENT:
                case UNOP_PREINCREMENT:
                case UNOP_POSTDECREMENT:
                case UNOP_POSTINCREMENT:
                        if (is_integral_type(tt))
                                tp = tt;
                        break;
                default:
                        UNHANDLED_CASE();
                }
        }
        exprInfo[x].tp = tp;
        return tp;
}

Type check_binop_expr_type(Expr x)
{
        int op = exprInfo[x].tUnop.kind;
        Expr x1 = exprInfo[x].tBinop.expr1;
        Expr x2 = exprInfo[x].tBinop.expr2;
        Type t1 = check_expr_type(x1);
        Type t2 = check_expr_type(x2);
        Type tp = -1;
        if (t1 != -1 && t2 != -1) {
                switch (op) {
                case BINOP_ASSIGN:
                case BINOP_EQUALS:
                case BINOP_MINUS:
                case BINOP_PLUS:
                case BINOP_MUL:
                case BINOP_DIV:
                case BINOP_BITAND:
                case BINOP_BITOR:
                case BINOP_BITXOR:
                        if (is_integral_type(t1) && is_integral_type(t2)
                            && t1 == t2)
                                tp = t1;
                        break;
                default:
                        UNHANDLED_CASE();
                }
        }
        exprInfo[x].tp = tp;
        return tp;
}

Type check_member_expr_type(Expr x)
{
        /*
        Expr xx = exprInfo[x].tMember.expr;
        String name = exprInfo[x].tMember.name;
        Type tt = exprInfo[xx].tMember.expr;
        */
        // TODO: lookup member and infer type
        Type tp = -1;
        exprInfo[x].tp = tp;
        return tp;
}

Type check_subscript_expr_type(Expr x)
{
        Expr x1 = exprInfo[x].tSubscript.expr1;
        Expr x2 = exprInfo[x].tSubscript.expr2;
        Type t1 = check_expr_type(x1);
        Type t2 = check_expr_type(x2);
        Type tp = -1;

        if (is_bad_type(t1) || is_bad_type(t2)) {
                LOG_TYPE_ERROR_EXPR(x,
                        "Cannot typecheck subscript expression: "
                        "bad array or index type\n");
        }
        else if (typeInfo[t1].kind != TYPE_ARRAY)
                LOG_TYPE_ERROR_EXPR(x,
                            "Subscript expression invalid: "
                            "indexed object is not array type\n");
        else if (! type_equal(t2, typeInfo[t1].tArray.idxtp))
                LOG_TYPE_ERROR_EXPR(x,
                            "Incompatible type of index "
                            "in subscript expression\n");
        else
                tp = typeInfo[t1].tArray.valuetp;
        /*
        MSG("t1=%d, t2=%d, idxtp=%d, valuetp=%d, tp=%d\n",
            t1, t2, typeInfo[t1].tArray.idxtp, typeInfo[t1].tArray.valuetp, tp);
            */
        exprInfo[x].tp = tp;
        return tp;
}

Type check_call_expr_type(Expr x)
{
        //XXX total mess and incomplete and wrong
        Expr callee = exprInfo[x].tCall.callee;
        Type calleeTp = check_expr_type(callee);
        if (calleeTp == -1)
                return -1;
        int calleeTpKind = typeInfo[calleeTp].kind;
        if (calleeTpKind != TYPE_PROC)
                LOG_TYPE_ERROR_EXPR(callee,
                    "Called expression: Expected proc type but found %s\n",
                    typeKindString[calleeTpKind]);
        int first = exprInfo[x].tCall.firstArgIdx;
        int last = first + exprInfo[x].tCall.nargs;
        for (int i = first; i < last; i++) {
                Expr argx = callArgInfo[i].argExpr;
                check_expr_type(argx);
                // TODO: check that argument type matches param of called proc
        }
        return -1;
}

Type check_expr_type(Expr x)
{
        Type tp = -1;
        switch (exprInfo[x].kind) {
        case EXPR_LITERAL:
                tp = check_literal_expr_type(x);
                break;
        case EXPR_SYMREF:
                tp = check_symref_expr_type(x);
                break;
        case EXPR_UNOP:
                tp = check_unop_expr_type(x);
                break;
        case EXPR_BINOP:
                tp = check_binop_expr_type(x);
                break;
        case EXPR_MEMBER:
                tp = check_member_expr_type(x);
                break;
        case EXPR_SUBSCRIPT:
                tp = check_subscript_expr_type(x);
                break;
        case EXPR_CALL:
                tp = check_call_expr_type(x);
                break;
        default:
                UNHANDLED_CASE();
        }
        exprInfo[x].tp = tp;
        return tp;
}

void check_types(void)
{
        for (Expr x = 0; x < exprCnt; x++)
                check_expr_type(x);
        for (Expr x = 0; x < exprCnt; x++) {
                if (exprInfo[x].tp == -1)
                        LOG_TYPE_ERROR_EXPR(
                                x, "Type check of expression failed\n");
        }
}

int main(int argc, const char **argv)
{
        compiler_init();

        const char *fileToParse = "test.txt";
        for (int i = 1; i < argc; i++)
                if (cstr_compare(argv[i], "-debug") == 0)
                        doDebug = 1;
                else
                        fileToParse = argv[i];

        add_file(intern_cstring(fileToParse));
        MSG("INFO", "Parsing file %s\n", fileToParse);
        parse_global_scope();
        MSG("INFO", "Resolving symbol references...\n");
        resolve_symbol_references();
        MSG("INFO", "Resolving type references...\n");
        resolve_type_references();
        MSG("INFO", "Checking types...\n");
        check_types();
        MSG("INFO", "Pretty printing input...\n\n");
        prettyprint();
        compiler_exit();
        return 0;
}
