#include "defs.h"
#include "api.h"

#define LOG_TYPE_ERROR_EXPR(...) MSG_AT_EXPR(lvl_error, __VA_ARGS__)

INTERNAL
Symbol find_symbol_in_scope(String name, Scope scope)
{
        for (; scope != -1; scope = scopeInfo[scope].parentScope) {
                Symbol first = scopeInfo[scope].firstSymbol;
                Symbol last = first + scopeInfo[scope].numSymbols;
                for (Symbol i = first; i < last; i++) {
                        if (symbolInfo[i].name == name) {
                                return i;
                        }
                }
        }
        return -1;
}

const char *const extsymname[NUM_EXTSYMS] = {
#define MAKE(name) [EXTSYM_##name] = #name
        MAKE( add64 ),
        MAKE( sub64 ),
        MAKE( mul64 ),
        MAKE( div64 ),
        MAKE( gt64 ),
        MAKE( lt64 ),
        MAKE( ge64 ),
        MAKE( le64 ),
        MAKE( eq64 ),
        MAKE( ne64 ),
        MAKE( print64 ),
        MAKE( prints ),
#undef MAKE
};

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

void resolve_symbol_references(void)
{
        DEBUG("Add external symbols\n");
        for (int i = 0; i < NUM_EXTSYMS; i++) {
                const char *name = extsymname[i];
                DEBUG("Add external symbol %s\n", name);

                Type tp = typeCnt++;
                Symbol sym = symbolCnt++;

                RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
                typeInfo[tp].kind = TYPE_PROC;
                typeInfo[tp].tProc.rettp = (Type) 0; //XXX
                typeInfo[tp].tProc.nparams = 0;

                RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
                symbolInfo[sym].name = intern_cstring(name);
                symbolInfo[sym].scope = (Scope) 0;
                symbolInfo[sym].kind = SYMBOL_PROC;  //XXX or sth like SYMBOL_UNDEFINED?
                symbolInfo[sym].tProc.tp = tp;
                symbolInfo[sym].tProc.optionalproc = -1;

                extsymToSymbol[i] = sym;
        }

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
                for (int i = 0; i < NUM_EXTSYMS; i++)
                        extsymToSymbol[i] = newname[extsymToSymbol[i]];
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

        for (Param param = paramCnt; param --> 0;) {
                Type proctp = paramInfo[param].proctp;
                ASSERT(typeInfo[proctp].kind == TYPE_PROC);
                typeInfo[proctp].tProc.firstParam = param;
        }

        sort_array(childStmtInfo, childStmtCnt, sizeof *childStmtInfo,
                   compare_ChildStmtInfo);
        sort_array(callArgInfo, callArgCnt, sizeof *callArgInfo,
                   compare_CallArgInfo);

        for (int i = childStmtCnt; i --> 0;) {
                Stmt parent = childStmtInfo[i].parent;
                ASSERT(stmtInfo[parent].kind == STMT_COMPOUND);
                stmtInfo[parent].tCompound.numStatements++;
                stmtInfo[parent].tCompound.firstChildStmtIdx = i;
        }

        for (int i = callArgCnt; i --> 0;) {
                Expr callee = callArgInfo[i].callExpr;
                ASSERT(exprInfo[callee].kind == EXPR_CALL);
                exprInfo[callee].tCall.nargs++;
                exprInfo[callee].tCall.firstArgIdx = i;
        }

        for (Symbol i = symbolCnt; i --> 0;) {
                scopeInfo[symbolInfo[i].scope].numSymbols++;
                scopeInfo[symbolInfo[i].scope].firstSymbol = i;
        }

        RESIZE_GLOBAL_BUFFER(symrefToSym, symrefCnt);
        int bad = 0;
        for (Symref ref = 0; ref < symrefCnt; ref++) {
                String name = symrefInfo[ref].name;
                Scope refScope = symrefInfo[ref].refScope;
                Symbol sym = find_symbol_in_scope(name, refScope);
                if (sym < 0) {
                        MSG_AT_TOK(lvl_error, symrefToToken[ref],
                                   "unresolved symbol reference %s\n",
                                   string_buffer(name));
                        bad = 1;
                }
                symrefToSym[ref] = sym;
        }
        if (bad) {
                MSG(lvl_info, "Symbol resolution failed. Terminating early.\n");
                cleanup();
                exit_program(1);
        }

        RESIZE_GLOBAL_BUFFER(isSymbolExported, symbolCnt);
        for (Symbol sym = 0; sym < symbolCnt; sym++)
                isSymbolExported[sym] = 0;
        for (Export x = 0; x < exportCnt; x++) {
                Symref ref = exportInfo[x].ref;
                Symbol sym = symrefToSym[ref];
                isSymbolExported[sym] = 1;
        }
}

INTERNAL
void resolve_ref_type(Type t)
{
        if (typeInfo[t].isComplete >= 0) {
                /* already processed */
                return;
        }
        if (typeInfo[t].isComplete == -1) {
                MSG(lvl_warn, "Type #%d: cyclic type reference\n", t);
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
                Symbol sym = symrefToSym[typeInfo[t].tRef.ref];
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
        ASSERT(isComplete != unassigned);
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
                ASSERT(typeInfo[t].isComplete == 1 ||
                       typeInfo[t].isComplete == 0);
        }
}

INTERNAL
int is_integral_type(Type t)
{
        return typeInfo[t].kind == TYPE_BASE;
}

INTERNAL
Type referenced_type(Type t)
{
        if (typeInfo[t].kind == TYPE_REFERENCE)
                t = typeInfo[t].tRef.resolvedTp;
        return t;
}

INTERNAL
Type pointer_type(Type t)
{
        // TODO: cache pointer-to version of this type
        Type r = typeCnt++;
        RESIZE_GLOBAL_BUFFER(typeInfo, r);
        typeInfo[r].kind = TYPE_POINTER;
        typeInfo[r].tPointer.tp = t;
        typeInfo[r].isComplete = 0; //XXX?
        return r;
}

INTERNAL
int is_bad_type(Type t)
{
        ASSERT(t >= 0);  // is this really true?
        return ! typeInfo[t].isComplete;
}

INTERNAL
int type_equal(Type a, Type b)
{
        if (!typeInfo[a].isComplete)
                return 0;
        if (!typeInfo[b].isComplete)
                return 0;
        while (typeInfo[a].kind == TYPE_REFERENCE) {
                a = typeInfo[a].tRef.resolvedTp;
                ASSERT(a != -1);
        }
        while (typeInfo[b].kind == TYPE_REFERENCE) {
                b = typeInfo[b].tRef.resolvedTp;
                ASSERT(a != -1);
        }
        return a == b;
}

INTERNAL
Type check_literal_expr_type(Expr x)
{
        //XXX
        Type tp = 0;
        exprType[x] = tp;
        return tp;
}

INTERNAL
Type check_symref_expr_type(Expr x)
{
        Symref ref = exprInfo[x].tSymref.ref;
        // XXX: symbol resolved?
        Symbol sym = symrefToSym[ref];
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
                        tp = symbolInfo[sym].tData.tp;
                        break;
                case SYMBOL_ARRAY:
                        tp = arrayInfo[symbolInfo[sym].tArray].tp;
                        break;
                case SYMBOL_PROC:
                        tp = symbolInfo[sym].tProc.tp;
                        break;
                default:
                        UNHANDLED_CASE();
                        break;
        }
out:
        exprType[x] = tp;
        return tp;
}

INTERNAL
Type check_expr_type(Expr x);

INTERNAL
Type check_unop_expr_type(Expr x)
{
        int op = exprInfo[x].tUnop.kind;
        Expr xx = exprInfo[x].tUnop.expr;
        Type tt = check_expr_type(xx);
        tt = referenced_type(tt);
        Type tp = -1;
        if (tt != -1) {
                switch (op) {
                case UNOP_ADDRESSOF:
                        tp = pointer_type(tt);
                        break;
                case UNOP_DEREF:
                        if (typeInfo[tt].kind == TYPE_POINTER)
                                tp = typeInfo[tt].tPointer.tp;
                        break;
                case UNOP_INVERTBITS:
                case UNOP_NOT:
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
        exprType[x] = tp;
        return tp;
}

INTERNAL
Type check_binop_expr_type(Expr x)
{
        int op = exprInfo[x].tUnop.kind;
        Expr x1 = exprInfo[x].tBinop.expr1;
        Expr x2 = exprInfo[x].tBinop.expr2;
        Type t1 = check_expr_type(x1);
        Type t2 = check_expr_type(x2);
        t1 = referenced_type(t1);
        t2 = referenced_type(t2);
        Type tp = -1;
        if (t1 != -1 && t2 != -1) {
                switch (op) {
                case BINOP_ASSIGN:
                case BINOP_GT:
                case BINOP_LT:
                case BINOP_GE:
                case BINOP_LE:
                case BINOP_EQ:
                case BINOP_NE:
                case BINOP_MINUS:
                case BINOP_PLUS:
                case BINOP_MUL:
                case BINOP_DIV:
                case BINOP_BITAND:
                case BINOP_BITOR:
                case BINOP_BITXOR:
                        if (is_integral_type(t1) && is_integral_type(t2)
                            /* && are_types_compatible(t1 == t2)*/) {
                                tp = t1;
                        }
                        else if (typeInfo[t1].kind == TYPE_POINTER &&
                                 typeInfo[t2].kind == TYPE_POINTER) {
                                if (referenced_type(typeInfo[t1].tPointer.tp) ==
                                    referenced_type(typeInfo[t2].tPointer.tp))
                                        tp = t1;
                        }
                        break;
                default:
                        UNHANDLED_CASE();
                }
        }
        exprType[x] = tp;
        return tp;
}

INTERNAL
Type check_member_expr_type(Expr x)
{
        /*
        Expr xx = exprInfo[x].tMember.expr;
        String name = exprInfo[x].tMember.name;
        Type tt = exprInfo[xx].tMember.expr;
        */
        // TODO: lookup member and infer type
        Type tp = -1;
        exprType[x] = tp;
        return tp;
}

INTERNAL
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
        exprType[x] = tp;
        return tp;
}

INTERNAL
Type check_call_expr_type(Expr x)
{
        Type tp = -1;
        Expr calleeExpr = exprInfo[x].tCall.callee;
        Type calleeTp = check_expr_type(calleeExpr);
        if (calleeTp == -1) {
                LOG_TYPE_ERROR_EXPR(x, "Type of callee is unknown\n");
        }
        else if (typeInfo[calleeTp].kind != TYPE_PROC) {
                LOG_TYPE_ERROR_EXPR(x, "Not a callable\n");
        }
        else {
                // TODO: check arguments
                tp = typeInfo[calleeTp].tProc.rettp;
        }
        return tp;
}

INTERNAL
Type check_expr_type(Expr x)
{
        Type tp = -1;
        switch (exprInfo[x].kind) {
        case EXPR_LITERAL:      tp = check_literal_expr_type(x); break;
        case EXPR_SYMREF:       tp = check_symref_expr_type(x); break;
        case EXPR_UNOP:         tp = check_unop_expr_type(x); break;
        case EXPR_BINOP:        tp = check_binop_expr_type(x); break;
        case EXPR_MEMBER:       tp = check_member_expr_type(x); break;
        case EXPR_SUBSCRIPT:    tp = check_subscript_expr_type(x); break;
        case EXPR_CALL:         tp = check_call_expr_type(x); break;
        default:                UNHANDLED_CASE();
        }
        exprType[x] = tp;
        return tp;
}

void check_types(void)
{
        ASSERT(globalBufferAlloc[BUFFER_exprType].cap == 0);
        RESIZE_GLOBAL_BUFFER(exprType, exprCnt);

        for (Expr x = 0; x < exprCnt; x++)
                check_expr_type(x);
        for (Expr x = 0; x < exprCnt; x++) {
                if (exprType[x] == -1)
                        LOG_TYPE_ERROR_EXPR(
                                x, "Type check of %s expression failed\n",
                                exprKindString[exprInfo[x].kind]);
        }
}
