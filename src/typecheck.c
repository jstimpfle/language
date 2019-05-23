#include "defs.h"
#include "api.h"

#define LOG_TYPE_ERROR_EXPR(...) MSG_AT_EXPR(lvl_error, __VA_ARGS__)

INTERNAL Type typecheck_assign(Type dstTp, Expr expr);

INTERNAL int is_lvalue_expression(Expr x)
{
        int kind = exprInfo[x].exprKind;
        return (kind == EXPR_SYMREF ||
                kind == EXPR_MEMBER ||
                kind == EXPR_SUBSCRIPT ||
                (kind == EXPR_UNOP && exprInfo[x].tUnop.unopKind == UNOP_DEREF));
}

/*
 * Check if the argument type is compatible with the parameter type.  They are
 * compatible if they are structurally the same (ignoring levels of
 * TYPE_REFERENCE) or if they are pointer types of the same indirection level
 * and the parameter type has the void type at its base.
 */
INTERNAL int arg_type_matches_param_type(Type argTp, Type paramTp)
{
        int tk;
        int ptr = 0;
        while (argTp != paramTp) {
                argTp = referenced_type(argTp);
                paramTp = referenced_type(paramTp);
                tk = typeInfo[argTp].typeKind;
                if (tk != typeInfo[paramTp].typeKind)
                        break;
                if (tk != TYPE_POINTER)
                        break;
                ptr = 1;
                argTp = typeInfo[argTp].tPointer.tp;
                paramTp = typeInfo[paramTp].tPointer.tp;
        }
        if (argTp == paramTp)
                return 1;
        /* char can be converted to int */
        if (argTp == builtinType[BUILTINTYPE_CHAR] &&
            paramTp == builtinType[BUILTINTYPE_INT])
                return 1;
        /* Any pointer argument is compatible with void pointer parameter */
        if (ptr && paramTp == builtinType[BUILTINTYPE_VOID])
                return 1;
        /* void pointer arguments are compatible with any pointer parameter */
        if (ptr && argTp == builtinType[BUILTINTYPE_VOID])
                return 1;
        /* If the argument type is ^[N]T for some length N and number T, and
         * the parameter type is just a ^T, then that's also fine. */
        if (ptr && typeInfo[argTp].typeKind == TYPE_ARRAY) {
                Type valueTp = typeInfo[argTp].tArray.valueTp;
                if (arg_type_matches_param_type(valueTp, paramTp))
                        return 1;
        }
        return type_equal(argTp, paramTp);
}

INTERNAL Type typecheck_array_expr(Type dstTp, Expr expr)
{
        if (exprInfo[expr].exprKind != EXPR_COMPOUND)
                return (Type) -1;
        int firstLink = exprInfo[expr].tCompound.firstCompoundExprLink;
        int numChilds = exprInfo[expr].tCompound.numChilds;
        Type valueTp = referenced_type(typeInfo[dstTp].tArray.valueTp);
        for (int i = 0; i < numChilds; i++) {
                Expr childExpr = compoundExprLink[firstLink + i].childExpr;
                Type tp = typecheck_assign(valueTp, childExpr);
                if (tp == (Type) -1) {
                        LOG_TYPE_ERROR_EXPR(childExpr,
                                "Wrong type in struct initializer\n");
                        return (Type) -1;
                }
        }
        return dstTp;
}

INTERNAL Type typecheck_struct_expr(Type dstTp, Expr expr)
{
        if (exprInfo[expr].exprKind != EXPR_COMPOUND)
                return (Type) -1;
        int firstLink = exprInfo[expr].tCompound.firstCompoundExprLink;
        int numChilds = exprInfo[expr].tCompound.numChilds;
        int firstMember = typeInfo[dstTp].tStruct.firstStructmember;
        int numMembers = typeInfo[dstTp].tStruct.numMembers;
        if (numChilds > numMembers) {
                LOG_TYPE_ERROR_EXPR(expr,
                        "Too many sub-expressions in complex "
                        "initializer expression\n");
                return (Type) -1;
        }
        for (int i = 0; i < numChilds; i++) {
                ASSERT(firstMember + i < structmemberCnt);
                ASSERT(structmemberInfo[firstMember + i].structTp == dstTp);
                Type memberTp = referenced_type(
                        structmemberInfo[firstMember + i].memberTp);
                Expr parentExpr = compoundExprLink[firstLink + i].parentExpr;
                Expr childExpr = compoundExprLink[firstLink + i].childExpr;
                ASSERT(parentExpr == expr);
                Type tp = typecheck_assign(memberTp, childExpr);
                if (tp == (Type) -1) {
                        LOG_TYPE_ERROR_EXPR(childExpr,
                                "Wrong type in struct initializer!\n");
                        /* TODO: show type mismatch */
                        return (Type) -1;
                }
        }
        // TODO: handling of numChilds < numMembers ?
        return dstTp;  //XXX ??
}

/* typecheck_assign() and arg_type_matches_param_type() probably
 * have basically the same goal. We should unify them */
INTERNAL Type typecheck_assign(Type dstTp, Expr expr)
{
        dstTp = referenced_type(dstTp);
        if (typeInfo[dstTp].typeKind == TYPE_ARRAY)
                return typecheck_array_expr(dstTp, expr);
        if (typeInfo[dstTp].typeKind == TYPE_STRUCT)
                return typecheck_struct_expr(dstTp, expr);
        if (exprInfo[expr].exprKind == EXPR_COMPOUND)
                /* dstTp is neither array nor struct */
                return (Type) -1;
        Type tp = check_expr_type(expr);
        if (! arg_type_matches_param_type(tp, dstTp)) // XXX
                return (Type) -1;
        return tp;
}

INTERNAL Type check_sizeof_compilercall_expr_type(Expr x)
{
        Expr y = exprInfo[x].tCompilercall.expr;
        check_expr_type(y);
        if (exprType[y] == (Type) -1)
                return (Type) -1;
        return builtinType[BUILTINTYPE_INT];
}

INTERNAL Type check_lengthof_compilercall_expr_type(Expr x)
{
        Expr y = exprInfo[x].tCompilercall.expr;
        check_expr_type(y);
        Type tp = exprType[y];
        if (tp == (Type) -1)
                return (Type) -1;
        if (typeInfo[tp].typeKind != TYPE_ARRAY) {
                LOG_TYPE_ERROR_EXPR(x, "#lengthof expression can only "
                                    "be applied to static arrays\n");
                return (Type) -1;
        }
        return builtinType[BUILTINTYPE_INT];
}

INTERNAL Type check_stringify_compilercall_expr_type(Expr x)
{
        (void) x;
        return string_type();
}

INTERNAL Type check_literal_expr_type(Expr x)
{
        switch (exprInfo[x].tLiteral.literalKind) {
        case LITERAL_INTEGER:
        case LITERAL_CHARACTER:
                return builtinType[BUILTINTYPE_INT];
        case LITERAL_FLOAT:
                return builtinType[BUILTINTYPE_FLOAT];
        case LITERAL_STRING:
                return string_type();
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL Type check_symref_expr_type(Expr x)
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
        case SYMBOL_PROC:
                return symbolInfo[sym].tProc.tp;
        case SYMBOL_MACRO:
                FATAL("TODO: We cannot check the type of a (call to a) macro. We need to implement some kind of cloning/instancing of the macro expression subtree first, and then replace the EXPR_CALL invocation of the macro by that new subtree.\n");
        case SYMBOL_CONSTANT: {
                Constant constant = symbolInfo[sym].tConstant;
                ASSERT(constant != (Constant) -1);
                int constantKind = constantInfo[constant].constantKind;
                if (constantKind == CONSTANT_INTEGER)
                        return builtinType[BUILTINTYPE_INT];
                else if (constantKind == CONSTANT_EXPRESSION) {
                        Expr expr = constantInfo[constant].tExpr;
                        if (exprType[expr] == -1) {
                                LOG_TYPE_ERROR_EXPR(expr,
                                        "Type of constant %s not yet known. "
                                        "Try changing the order of "
                                        "declarations\n",
                                        SS(constantInfo[constant].symbol));
                                return (Type) -1;
                        }
                        return exprType[expr];
                }
                else {
                        UNHANDLED_CASE();
                }
        }
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL Type check_unop_expr_type(Expr x)
{
        int op = exprInfo[x].tUnop.unopKind;
        Expr xx = exprInfo[x].tUnop.expr;
        switch (op) {
        case UNOP_ADDRESSOF: {
                Type tt = check_expr_type(xx);
                if (tt != (Type) -1) {
                        if (is_lvalue_expression(xx))
                                return pointer_type(tt);
                        LOG_TYPE_ERROR_EXPR(xx,
                            "Cannot take the address of this expression\n");
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
        case UNOP_NOT: {
                Type tt = check_expr_type(xx);
                if (tt != (Type) -1) {
                        int k = typeInfo[tt].typeKind;
                        if (k == TYPE_BASE || k == TYPE_POINTER)
                                return builtinType[BUILTINTYPE_INT];
                }
                break;
        }
        case UNOP_BITWISENOT:
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

INTERNAL Type check_binop_expr_type(Expr x)
{
        int op = exprInfo[x].tBinop.binopKind;
        Expr x1 = exprInfo[x].tBinop.expr1;
        Expr x2 = exprInfo[x].tBinop.expr2;
        if (op == BINOP_ASSIGN) {
                Type t1 = check_expr_type(x1);
                return typecheck_assign(t1, x2);
        }

        Type t1 = check_expr_type(x1);
        Type t2 = check_expr_type(x2);
        if (t1 != (Type) -1 && t2 != (Type) -1) {
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

INTERNAL Type check_member_expr_type(Expr x)
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
                if (tp == (Type) -1) {
                        LOG_TYPE_ERROR_EXPR(x,
                                "struct %s has no member called %s\n",
                                string_buffer(typeInfo[t1].tStruct.name),
                                string_buffer(n));
                }
        }
        return tp;
}

INTERNAL Type check_subscript_expr_type(Expr x)
{
        Expr x1 = exprInfo[x].tSubscript.expr1;
        Expr x2 = exprInfo[x].tSubscript.expr2;
        Type t1 = check_expr_type(x1);
        Type t2 = check_expr_type(x2);
        Type tp = (Type) -1;
        if (t1 == (Type) -1)
                LOG_TYPE_ERROR_EXPR(x,
                        "Cannot typecheck subscript expression: "
                        "invalid or incomplete array type\n");
        else if (t2 == (Type) -1)
                LOG_TYPE_ERROR_EXPR(x,
                        "Cannot typecheck subscript expression: "
                        "invalid or incomplete index type\n");
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
        else if (typeInfo[t1].typeKind == TYPE_ARRAY) {
                tp = typeInfo[t1].tArray.valueTp;
        }
        else {
                UNHANDLED_CASE();
        }
        return tp;
}

INTERNAL Type check_call_expr_type(Expr x)
{
        Expr calleeExpr = exprInfo[x].tCall.callee;
        Type calleeTp = check_expr_type(calleeExpr);
        if (calleeTp == (Type) -1) {
                LOG_TYPE_ERROR_EXPR(x, "Type of callee is unknown\n");
                return (Type) -1;
        }
        else if (typeInfo[calleeTp].typeKind != TYPE_PROC) {
                LOG_TYPE_ERROR_EXPR(x, "Not a callable but a %s\n",
                                    typeKindString[typeInfo[calleeTp].typeKind]);
                return (Type) -1;
        }

        int first = exprInfo[x].tCall.firstArgIdx;
        int nargs = exprInfo[x].tCall.nargs;
        for (int i = 0; i < nargs; i++) {
                Type argTp = check_expr_type(callArgInfo[first + i].argExpr);
                if (argTp == (Type) -1)
                        return (Type) -1;
        }

        /* XXX: string_buffer() does not return a stable address currently.
         * We must take care not to allocate new strings as long as procName
         * is in use */
        const char *procName = (exprInfo[calleeExpr].exprKind == EXPR_SYMREF)
                ? string_buffer(symrefInfo[exprInfo[calleeExpr].tSymref.ref].name)
                : "<complex proc expr>";

        Param firstParam = firstProctypeParam[calleeTp];
        int nparams = typeInfo[calleeTp].tProc.nparams;
        if (nparams != nargs) {
                LOG_TYPE_ERROR_EXPR(x,
                        "In call to %s(): Wrong number of arguments. "
                        "Need %d, but got %d\n", procName, nparams, nargs);
                return (Type) -1;
        }

        for (int i = 0; i < nargs; i++) {
                Type argTp = exprType[callArgInfo[first + i].argExpr];
                Type paramTp = paramInfo[firstParam + i].tp;
                if (! arg_type_matches_param_type(argTp, paramTp)) {
                        LOG_TYPE_ERROR_EXPR(x,
                                "In call to %s(): Argument #%d doesn't match "
                                "type of function parameter\n", procName, i+1);
                        outf("Found:    ");  print_type(argTp); outf("\n");
                        outf("Expected: "); print_type(paramTp); outf("\n");
                        return (Type) -1;
                }
        }
        return typeInfo[calleeTp].tProc.rettp;
}

INTERNAL Type check_compound_expr_type(Expr x)
{
        /*XXX*/
        (void) x;
        return builtinType[BUILTINTYPE_COMPOUND];
}

INTERNAL Type check_compilervalue_expr_type(Expr x)
{
        int cvkind = exprInfo[x].tCompilervalue.compilervalueKind;
        switch (cvkind) {
        case COMPILERVALUE_FILE: return string_type();
        case COMPILERVALUE_LINE: return builtinType[BUILTINTYPE_INT];
        case COMPILERVALUE_PROCNAME: return string_type();
        default: UNHANDLED_CASE();
        }
}

INTERNAL Type (*const compilercallKindToTypecheckFunc[NUM_COMPILERCALL_KINDS])(Expr x) = {
        [COMPILERCALL_STRINGIFY] = check_stringify_compilercall_expr_type,
        [COMPILERCALL_LENGTHOF]  = check_lengthof_compilercall_expr_type,
        [COMPILERCALL_SIZEOF]    = check_sizeof_compilercall_expr_type,
};

INTERNAL Type check_compilercall_expr_type(Expr x)
{
        int kind = exprInfo[x].tCompilercall.compilercallKind;
        ASSERT( compilercallKindToTypecheckFunc [kind] );
        Type tp = compilercallKindToTypecheckFunc [kind] (x);
        exprType[x] = tp;
        return tp;
}

INTERNAL Type (*const exprKindToTypecheckFunc[NUM_EXPR_KINDS])(Expr x) = {
        [EXPR_LITERAL]    = check_literal_expr_type,
        [EXPR_SYMREF]     = check_symref_expr_type,
        [EXPR_UNOP]       = check_unop_expr_type,
        [EXPR_BINOP]      = check_binop_expr_type,
        [EXPR_MEMBER]     = check_member_expr_type,
        [EXPR_SUBSCRIPT]  = check_subscript_expr_type,
        [EXPR_CALL]       = check_call_expr_type,
        [EXPR_COMPOUND]   = check_compound_expr_type,
        [EXPR_COMPILERVALUE] = check_compilervalue_expr_type,
        [EXPR_COMPILERCALL] = check_compilercall_expr_type,
};

Type check_expr_type(Expr x)
{
        ASSERT(0 <= x && x < exprCnt);
        DEBUG("Check type of %s expression=%d...\n",
              exprKindString[exprInfo[x].exprKind], x);

        /* Catch redundant type checks. They indicate a structural problem */
        if (exprType[x] != (Type) -1) {
                print_type(exprType[x]); outs("\n");
                MSG_AT_EXPR(lvl_info, x,
                            "type of %s expression=%d is %d\n",
                            exprKindString[exprInfo[x].exprKind],
                            x, exprType[x]);
                ASSERT(0);
        }

        int kind = exprInfo[x].exprKind;
        ASSERT(0 <= kind && kind < NUM_EXPR_KINDS);
        ASSERT( exprKindToTypecheckFunc [kind] );

        Type tp = exprKindToTypecheckFunc [kind] (x);
        if (tp == (Type) -1) {
                LOG_TYPE_ERROR_EXPR(
                        x, "Type check of %s expression failed\n",
                        exprKindString[exprInfo[x].exprKind]);
                FATAL_ERROR_AT_EXPR(x, "Type errors detected\n");
        }
        DEBUG("type of expr=%d successfully checked as %d\n", x, tp);

        exprType[x] = tp;
        return referenced_type(tp);
}

void check_stmt_types(Stmt a)
{
        DEBUG("Check types of statement %d\n", a);
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
        case STMT_DATA: {
                Data data = stmtInfo[a].tData.data;
                Type dataTp = dataInfo[data].tp;
                Expr expr = stmtInfo[a].tData.optionalInitializerExpr;
                if (expr != (Expr) -1) {
                        Type tp = typecheck_assign(dataTp, expr);
                        if (tp == (Type) -1) { // XXX what to do?
                                LOG_TYPE_ERROR_EXPR(expr, "Typecheck failed\n");
                                exprType[expr] = (Type) -1; // XXX
                                FATAL_ERROR_AT_EXPR(expr, "Type errors detected\n"); //XXX
                        }
                }
                break;
        }
        case STMT_MACRO:
                // TODO constant expressions need to be checked, too!
                break;
        case STMT_IGNORE:
                check_stmt_types(stmtInfo[a].tIgnore);
                break;
        default:
                UNHANDLED_CASE();
        }
}
