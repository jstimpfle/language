#include "defs.h"
#include "api.h"

#define LOG_TYPE_ERROR_EXPR(...) MSG_AT_EXPR(lvl_error, __VA_ARGS__)

INTERNAL
int is_lvalue_expression(Expr x)
{
        int kind = exprInfo[x].exprKind;
        return (kind == EXPR_SYMREF ||
                kind == EXPR_MEMBER ||
                (kind == EXPR_UNOP && exprInfo[x].tUnop.unopKind == UNOP_DEREF));
}

INTERNAL
Type pointer_type(Type t)
{
        // TODO: cache pointer-to version of this type
        Type r = typeCnt++;
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
        typeInfo[r].typeKind = TYPE_POINTER;
        typeInfo[r].tPointer.tp = t;
        typeInfo[r].isComplete = typeInfo[t].isComplete; //XXX?
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
        a = referenced_type(a);
        b = referenced_type(b);
        ASSERT(a != -1);
        ASSERT(b != -1);
        return a == b;
}

INTERNAL
Type check_expr_type(Expr x);

INTERNAL
Type check_literal_expr_type(Expr x)
{
        switch (exprInfo[x].tLiteral.literalKind) {
        case LITERAL_INTEGER:
                return builtinType[BUILTINTYPE_INT];
        case LITERAL_STRING:
                return pointer_type(builtinType[BUILTINTYPE_CHAR]);
        default:
                UNHANDLED_CASE();
        }
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
        switch (symbolInfo[sym].symbolKind) {
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
Type check_unop_expr_type(Expr x)
{
        int op = exprInfo[x].tUnop.unopKind;
        Expr xx = exprInfo[x].tUnop.expr;
        switch (op) {
        case UNOP_ADDRESSOF: {
                Type tt = check_expr_type(xx);
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
                Type tt = check_expr_type(xx);
                if (tt != (Type) -1) {
                        int k = typeInfo[tt].typeKind;
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
                Type tt = check_expr_type(xx);
                if (tt != (Type) -1) {
                        int k = typeInfo[tt].typeKind;
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
        int op = exprInfo[x].tUnop.unopKind;
        Expr x1 = exprInfo[x].tBinop.expr1;
        Expr x2 = exprInfo[x].tBinop.expr2;
        if (op == BINOP_ASSIGN) {
                /*Type t1 =*/ check_expr_type(x1);
                Type t2 = check_expr_type(x2);
                return t2;
        }

        Type t1 = check_expr_type(x1);
        Type t2 = check_expr_type(x2);
        if (t1 != -1 && t2 != -1) {
                int k1 = typeInfo[t1].typeKind;
                int k2 = typeInfo[t2].typeKind;
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
        Expr e = exprInfo[x].tMember.expr;
        String n = exprInfo[x].tMember.name;
        Type t1 = check_expr_type(e);
        Type tp = -1;
        if (t1 == -1) {
                /* bad */
        }
        else if (typeInfo[t1].typeKind != TYPE_STRUCT) {
                LOG_TYPE_ERROR_EXPR(x,
                        "Invalid member expression: "
                        "Operand is not a struct type (it is %s)\n",
                        typeKindString[typeInfo[t1].typeKind]);
        }
        else {
                for (Structmember m = typeInfo[t1].tStruct.firstStructmember;
                     m < structmemberCnt && structmemberInfo[m].structTp == t1;
                     m++) {
                        if (structmemberInfo[m].memberName == n) {
                                tp = structmemberInfo[m].memberTp;
                                break;
                        }
                }
                if (tp == -1) {
                        LOG_TYPE_ERROR_EXPR(x,
                                "struct %s has no member called %s\n",
                                string_buffer(typeInfo[t1].tStruct.name),
                                string_buffer(n));
                }
        }
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
        if (t1 == -1 || is_bad_type(t1))
                LOG_TYPE_ERROR_EXPR(x,
                        "Cannot typecheck subscript expression: "
                        "bad array type\n");
        else if (t2 == -1 || is_bad_type(t2))
                LOG_TYPE_ERROR_EXPR(x,
                        "Cannot typecheck subscript expression: "
                        "bad index type\n");
        else if (typeInfo[t1].typeKind != TYPE_ARRAY &&
                 typeInfo[t1].typeKind != TYPE_POINTER)
                LOG_TYPE_ERROR_EXPR(x,
                        "Subscript expression invalid: "
                        "indexed object is not array type\n");
        else if (typeInfo[t2].typeKind != TYPE_BASE)
                LOG_TYPE_ERROR_EXPR(x,
                        "Invalid type of index used in subscript expression. "
                        "Need integral type: %s.\n",
                        typeKindString[typeInfo[t2].typeKind]);
        else if (typeInfo[t1].typeKind == TYPE_POINTER) {
                tp = typeInfo[t1].tPointer.tp;
        }
        else {
                UNHANDLED_CASE();
        }
        return tp;
}

INTERNAL
Type check_call_expr_type(Expr x)
{
        Expr calleeExpr = exprInfo[x].tCall.callee;
        Type calleeTp = check_expr_type(calleeExpr);
        Type tp = -1;
        if (calleeTp == -1) {
                LOG_TYPE_ERROR_EXPR(x, "Type of callee is unknown\n");
        }
        else if (typeInfo[calleeTp].typeKind != TYPE_PROC) {
                LOG_TYPE_ERROR_EXPR(x, "Not a callable but a %s\n",
                                    typeKindString[typeInfo[calleeTp].typeKind]);
        }
        else {
                check_expr_type(exprInfo[x].tCall.callee);
                int first = exprInfo[x].tCall.firstArgIdx;
                int last = first + exprInfo[x].tCall.nargs;
                for (int i = first; i < last; i++) {
                        check_expr_type(callArgInfo[i].argExpr);
                }
                // TODO: match arguments with function parameters
                tp = typeInfo[calleeTp].tProc.rettp;
        }
        return tp;
}

INTERNAL
Type (*const exprKindToTypecheckFunc[NUM_EXPR_KINDS])(Expr x) = {
#define MAKE(x, y) [x] = &y
        MAKE(  EXPR_LITERAL,    check_literal_expr_type    ),
        MAKE(  EXPR_SYMREF,     check_symref_expr_type     ),
        MAKE(  EXPR_UNOP,       check_unop_expr_type       ),
        MAKE(  EXPR_BINOP,      check_binop_expr_type      ),
        MAKE(  EXPR_MEMBER,     check_member_expr_type     ),
        MAKE(  EXPR_SUBSCRIPT,  check_subscript_expr_type  ),
        MAKE(  EXPR_CALL,       check_call_expr_type       ),
#undef MAKE
};

INTERNAL
Type check_expr_type(Expr x)
{
        ASSERT(0 <= x && x < exprCnt);
        Type tp = -1;

        int kind = exprInfo[x].exprKind;
        ASSERT(0 <= kind && kind < NUM_EXPR_KINDS);
        tp = exprKindToTypecheckFunc [kind] (x);

        exprType[x] = tp;
        return (tp == (Type) -1) ? tp : referenced_type(tp);
}

INTERNAL
void check_stmt_types(Stmt a)
{
        switch (stmtInfo[a].stmtKind) {
        case STMT_IF: {
                check_expr_type(stmtInfo[a].tIf.condExpr);
                check_stmt_types(stmtInfo[a].tIf.ifbody);
                break;
        }
        case STMT_IFELSE: {
                check_expr_type(stmtInfo[a].tIfelse.condExpr);
                check_stmt_types(stmtInfo[a].tIfelse.ifbody);
                check_stmt_types(stmtInfo[a].tIfelse.elsebody);
                break;
        }
        case STMT_FOR: {
                check_stmt_types(stmtInfo[a].tFor.initStmt);
                check_expr_type(stmtInfo[a].tFor.condExpr);
                check_stmt_types(stmtInfo[a].tFor.stepStmt);
                check_stmt_types(stmtInfo[a].tFor.forbody);
                break;
        }
        case STMT_WHILE: {
                check_expr_type(stmtInfo[a].tWhile.condExpr);
                check_stmt_types(stmtInfo[a].tWhile.whilebody);
                break;
        }
        case STMT_RANGE: {
                Expr e1 = stmtInfo[a].tRange.startExpr;
                Expr e2 = stmtInfo[a].tRange.stopExpr;
                Stmt body = stmtInfo[a].tRange.rangebody;
                Type t1 = check_expr_type(e1);
                Type t2 = check_expr_type(e2);
                DEBUG("Types are t1=%d t2=%d\n", t1, t2);
                if (t1 != builtinType[BUILTINTYPE_INT]) {
                        LOG_TYPE_ERROR_EXPR(e1,
                                "Only integer expressions allowed\n");
                        exprType[e1] = (Type) -1;  // XXX
                }
                if (t2 != builtinType[BUILTINTYPE_INT]) {
                        LOG_TYPE_ERROR_EXPR(e2,
                                "Only integer expressions allowed\n");
                        exprType[e2] = (Type) -1;  // XXX
                }
                check_stmt_types(body);
                break;
        }
        case STMT_RETURN: {
                check_expr_type(stmtInfo[a].tReturn.expr);
                break;
        }
        case STMT_EXPR: {
                check_expr_type(stmtInfo[a].tExpr.expr);
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
        RESIZE_GLOBAL_BUFFER(exprType, exprCnt);

        for (Proc p = 0; p < procCnt; p++)
                check_stmt_types(procInfo[p].body);

        int bad = 0;
        for (Expr x = 0; x < exprCnt; x++) {
                if (exprType[x] == -1) {
                        LOG_TYPE_ERROR_EXPR(
                                x, "Type check of %s expression failed\n",
                                exprKindString[exprInfo[x].exprKind]);
                        bad = 1;
                }
        }
        if (bad) {
                FATAL("Type errors detected\n");
        }

        {
                for (Type tp = 0; tp < typeCnt; tp++)
                        if (typeInfo[tp].typeKind == TYPE_STRUCT)
                                typeInfo[tp].tStruct.size = 0;
                for (Structmember m = 0; m < structmemberCnt; m++) {
                        Type tp = structmemberInfo[m].structTp;
                        if (tp != structmemberInfo[m-1].structTp)
                                typeInfo[tp].tStruct.size = 0;
                        int offset = typeInfo[tp].tStruct.size;
                        structmemberInfo[m].offset = offset;
                        int size = get_type_size(structmemberInfo[m].memberTp);
                        /* XXX: better handling and better error message needed
                         *
                         * We could theoretically allow declarations in any
                         * order as long as there are no cycles. But would that
                         * be a good idea too?
                         */
                        if (size == 0) {
                                FATAL("Incomplete type: "
                                      "Size of member %s is not yet known!\n",
                                      string_buffer(structmemberInfo[m].memberName));
                        }
                        offset += size;
                        typeInfo[tp].tStruct.size = offset;
                }
        }
}
