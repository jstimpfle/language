#include "defs.h"
#include "api.h"

#define LOG_TYPE_ERROR_EXPR(...) MSG_AT_EXPR(lvl_error, __VA_ARGS__)

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

INTERNAL UNUSEDFUNC
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
                ASSERT(b != -1);
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
                int k = typeInfo[tt].kind;
                switch (op) {
                case UNOP_ADDRESSOF:
                        tp = pointer_type(tt);
                        break;
                case UNOP_DEREF:
                        if (k == TYPE_POINTER)
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
                        if (k == TYPE_BASE)
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
                int k1 = typeInfo[t1].kind;
                int k2 = typeInfo[t2].kind;
                switch (op) {
                case BINOP_ASSIGN:
                        /* XXX: explicit cast needed? */
                        tp = k1;
                        break;
                case BINOP_PLUS:
                case BINOP_MINUS:
                        if (k1 == TYPE_BASE && k2 == TYPE_BASE)
                                /* XXX: widening rules? */
                                tp = t1;
                        else if (k1 == TYPE_POINTER && k2 == TYPE_BASE)
                                tp = t1;
                        else if (k1 == TYPE_BASE && k2 == TYPE_POINTER)
                                tp = t2;
                        break;
                case BINOP_GT:
                case BINOP_LT:
                case BINOP_GE:
                case BINOP_LE:
                case BINOP_EQ:
                case BINOP_NE:
                case BINOP_MUL:
                case BINOP_DIV:
                case BINOP_BITAND:
                case BINOP_BITOR:
                case BINOP_BITXOR:
                        if (k1 == TYPE_BASE && k2 == TYPE_BASE)
                                /* XXX: widening rules? */
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
        t1 = referenced_type(t1);
        t2 = referenced_type(t2);
        if (t1 == -1 || is_bad_type(t1))
                LOG_TYPE_ERROR_EXPR(x,
                        "Cannot typecheck subscript expression: "
                        "bad array type\n");
        else if (t2 == -1 || is_bad_type(t2))
                LOG_TYPE_ERROR_EXPR(x,
                        "Cannot typecheck subscript expression: "
                        "bad index type\n");
        else if (typeInfo[t1].kind != TYPE_ARRAY &&
                 typeInfo[t1].kind != TYPE_POINTER)
                LOG_TYPE_ERROR_EXPR(x,
                        "Subscript expression invalid: "
                        "indexed object is not array type\n");
        else if (typeInfo[t2].kind != TYPE_BASE)
                LOG_TYPE_ERROR_EXPR(x,
                        "Invalid type of index used in subscript expression. "
                        "Need integral type: %d.\n",
                        typeKindString[typeInfo[t2].kind]);
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
