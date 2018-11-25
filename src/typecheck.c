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

void resolve_symbol_references(void)
{
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
        return typeInfo[t].kind == TYPE_BASE; //XXX
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
                        tp = dataInfo[symbolInfo[sym].tData.optionaldata].tp;
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
                            && t1 == t2)
                                tp = t1;
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
                                x, "Type check of expression failed\n");
        }
}
