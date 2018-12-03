#include "defs.h"
#include "api.h"

#define LOG_TYPE_ERROR_EXPR(...) MSG_AT_EXPR(lvl_error, __VA_ARGS__)

enum {
        NOTEVALUATED = 0,
        EVALUATED = 1,
};

INTERNAL
int is_lvalue_expression(Expr x)
{
        int kind = exprInfo[x].kind;
        return (kind == EXPR_SYMREF ||
                kind == EXPR_MEMBER ||
                (kind == EXPR_UNOP && exprInfo[x].tUnop.kind == UNOP_DEREF));
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
        (void) x;
        return (Type) 0;  // XXX
}

INTERNAL
Type check_symref_expr_type(Expr x)
{
        Symref ref = exprInfo[x].tSymref.ref;
        // XXX: symbol resolved?
        Symbol sym = symrefToSym[ref];
        if (sym == -1) {
                const char *name = string_buffer(symrefInfo[ref].name);
                LOG_TYPE_ERROR_EXPR(x,
                      "Can't check type: symbol \"%s\" unresolved\n", name);
                return (Type) -1;
        }
        switch (symbolInfo[sym].kind) {
                case SYMBOL_TYPE:
                        // Maybe something like the "type" type?
                        // Or fatal() ?
                        UNHANDLED_CASE();
                        break;
                case SYMBOL_DATA:
                        return symbolInfo[sym].tData.tp;
                case SYMBOL_ARRAY:
                        return arrayInfo[symbolInfo[sym].tArray].tp;
                case SYMBOL_PROC:
                        return symbolInfo[sym].tProc.tp;
                default:
                        UNHANDLED_CASE();
        }
}

INTERNAL
Type check_expr_type(Expr x, int evaluated);

INTERNAL
Type check_unop_expr_type(Expr x)
{
        int op = exprInfo[x].tUnop.kind;
        Expr xx = exprInfo[x].tUnop.expr;
        switch (op) {
        case UNOP_ADDRESSOF: {
                Type tt = check_expr_type(xx, NOTEVALUATED);
                if (!is_lvalue_expression(xx)) {
                        LOG_TYPE_ERROR_EXPR(xx,
                                "Cannot take the address of this expression\n");
                }
                else {
                        if (tt != (Type) -1)
                                return pointer_type(tt);
                }
                break;
        }
        case UNOP_DEREF: {
                Type tt = check_expr_type(xx, EVALUATED);
                if (tt != (Type) -1) {
                        int k = typeInfo[tt].kind;
                        if (k == TYPE_POINTER)
                                return typeInfo[tt].tPointer.tp;
                }
                break;
        }
        case UNOP_INVERTBITS:
        case UNOP_NOT:
        case UNOP_NEGATIVE:
        case UNOP_POSITIVE:
        case UNOP_PREDECREMENT:
        case UNOP_PREINCREMENT:
        case UNOP_POSTDECREMENT:
        case UNOP_POSTINCREMENT: {
                Type tt = check_expr_type(xx, EVALUATED);
                if (tt != (Type) -1) {
                        int k = typeInfo[tt].kind;
                        if (k == TYPE_BASE)
                                return tt;
                }
                break;
        }
        default:
                UNHANDLED_CASE();
        }
        return (Type) -1;
}

INTERNAL
Type check_binop_expr_type(Expr x)
{
        int op = exprInfo[x].tUnop.kind;
        Expr x1 = exprInfo[x].tBinop.expr1;
        Expr x2 = exprInfo[x].tBinop.expr2;
        if (op == BINOP_ASSIGN) {
                /*Type t1 =*/ check_expr_type(x1, NOTEVALUATED);
                Type t2 = check_expr_type(x2, EVALUATED);
                return t2;
        }

        Type t1 = check_expr_type(x1, EVALUATED);
        Type t2 = check_expr_type(x2, EVALUATED);
        if (t1 != -1 && t2 != -1) {
                int k1 = typeInfo[t1].kind;
                int k2 = typeInfo[t2].kind;
                switch (op) {
                case BINOP_ASSIGN:
                        UNHANDLED_CASE(); /* already handled above */
                case BINOP_PLUS:
                case BINOP_MINUS:
                        if (k1 == TYPE_BASE && k2 == TYPE_BASE)
                                /* XXX: widening rules? */
                                return t1;
                        else if (k1 == TYPE_POINTER && k2 == TYPE_BASE)
                                return t1;
                        else if (k1 == TYPE_BASE && k2 == TYPE_POINTER)
                                return t2;
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
                                return t1;
                        break;
                default:
                        UNHANDLED_CASE();
                }
        }
        return -1;
}

INTERNAL
Type check_member_expr_type(Expr x)
{
        check_expr_type(exprInfo[x].tMember.expr, EVALUATED);
        return (Type) -1; //TODO
}

INTERNAL
Type check_subscript_expr_type(Expr x)
{
        Expr x1 = exprInfo[x].tSubscript.expr1;
        Expr x2 = exprInfo[x].tSubscript.expr2;
        Type t1 = check_expr_type(x1, EVALUATED);
        Type t2 = check_expr_type(x2, EVALUATED);
        Type tp = -1;
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
        return tp;
}

INTERNAL
Type check_call_expr_type(Expr x)
{
        Type tp = -1;
        Expr calleeExpr = exprInfo[x].tCall.callee;
        Type calleeTp = check_expr_type(calleeExpr, EVALUATED);
        if (calleeTp == -1) {
                LOG_TYPE_ERROR_EXPR(x, "Type of callee is unknown\n");
        }
        else if (typeInfo[calleeTp].kind != TYPE_PROC) {
                LOG_TYPE_ERROR_EXPR(x, "Not a callable\n");
        }
        else {
                check_expr_type(exprInfo[x].tCall.callee, EVALUATED);
                int first = exprInfo[x].tCall.firstArgIdx;
                int last = first + exprInfo[x].tCall.nargs;
                for (int i = first; i < last; i++) {
                        check_expr_type(callArgInfo[i].argExpr, EVALUATED);
                }
                // TODO: match arguments with function parameters
                tp = typeInfo[calleeTp].tProc.rettp;
        }
        return tp;
}

INTERNAL
Type check_expr_type(Expr x, int evaluated)
{
        Type tp = -1;
        isExprEvaluated[x] = evaluated; // set *before* checking type
        switch (exprInfo[x].kind) {
        case EXPR_LITERAL:      tp = check_literal_expr_type(x); DEBUG("Have %d\n", tp); break;
        case EXPR_SYMREF:       tp = check_symref_expr_type(x); break;
        case EXPR_UNOP:         tp = check_unop_expr_type(x); break;
        case EXPR_BINOP:        tp = check_binop_expr_type(x); break;
        case EXPR_MEMBER:       tp = check_member_expr_type(x); break;
        case EXPR_SUBSCRIPT:    tp = check_subscript_expr_type(x); break;
        case EXPR_CALL:         tp = check_call_expr_type(x); break;
        default:                UNHANDLED_CASE();
        }
        if (tp != (Type) -1) {
                exprType[x] = evaluated ? tp : pointer_type(tp);
                return referenced_type(exprType[x]);
        }
        else {
                exprType[x] = (Type) -1;
                return (Type) -1;  // XXX: is it safe to continue?
        }
}

void check_stmt_types(Stmt a)
{
        switch (stmtInfo[a].kind) {
        case STMT_IF: {
                check_expr_type(stmtInfo[a].tIf.condExpr, EVALUATED);
                check_stmt_types(stmtInfo[a].tIf.ifbody);
                break;
        }
        case STMT_IFELSE: {
                check_expr_type(stmtInfo[a].tIfelse.condExpr, EVALUATED);
                check_stmt_types(stmtInfo[a].tIfelse.ifbody);
                check_stmt_types(stmtInfo[a].tIfelse.elsebody);
                break;
        }
        case STMT_FOR: {
                check_stmt_types(stmtInfo[a].tFor.initStmt);
                check_expr_type(stmtInfo[a].tFor.condExpr, EVALUATED);
                check_stmt_types(stmtInfo[a].tFor.stepStmt);
                check_stmt_types(stmtInfo[a].tFor.forbody);
                break;
        }
        case STMT_WHILE: {
                check_expr_type(stmtInfo[a].tWhile.condExpr, EVALUATED);
                check_stmt_types(stmtInfo[a].tWhile.whilebody);
                break;
        }
        case STMT_RETURN: {
                check_expr_type(stmtInfo[a].tReturn.expr, EVALUATED);
                break;
        }
        case STMT_EXPR: {
                check_expr_type(stmtInfo[a].tExpr.expr, NOTEVALUATED);
                break;
        }
        case STMT_COMPOUND: {
                int first = stmtInfo[a].tCompound.firstChildStmtIdx;
                int c = stmtInfo[a].tCompound.numStatements;
                for (int child = first; child < first + c; child++) {
                        ASSERT(childStmtInfo[child].parent == a);
                        check_stmt_types(childStmtInfo[child].child);
                }
                break;
        }
        case STMT_DATA:
        case STMT_ARRAY:
                // TODO constant expressions need to be checked, too!
                break;
        default:
                UNHANDLED_CASE();
        }
}

void check_types(void)
{
        ASSERT(globalBufferAlloc[BUFFER_exprType].cap == 0);
        RESIZE_GLOBAL_BUFFER(isExprEvaluated, exprCnt);
        RESIZE_GLOBAL_BUFFER(exprType, exprCnt);

        for (Stmt a = 0; a < stmtCnt; a++)
                check_stmt_types(a);

        int bad = 0;
        for (Expr x = 0; x < exprCnt; x++) {
                if (exprType[x] == -1) {
                        LOG_TYPE_ERROR_EXPR(
                                x, "Type check of %s expression failed\n",
                                exprKindString[exprInfo[x].kind]);
                        bad = 1;
                }
        }
        if (bad) {
                FATAL("Type errors detected\n");
        }
}
