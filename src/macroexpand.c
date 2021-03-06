#include "defs.h"
#include "api.h"

INTERNAL
Expr lookup_macro_param(MacroParam macroParam)
{
        for (int i = 0; i < macroBoundArgCnt; i++)
                if (macroParam == macroBoundArg[i].macroParam)
                        return macroBoundArg[i].expr;
        /* the symbol resolution phase should have worked out this error */
        UNREACHABLE();
}

INTERNAL Proc CURRENTLY_EXPANDED_PROC;
#define EXPAND(x) ((x) = expand_expr(x))

/*
 * XXX: this is important to prevent a bug arising from undefined behaviour:
 * undefined order of evaluation in expressions like
 *
 * exprInfo[x].fooExpr = expand_expr(exprInfo[x].fooExpr);
 *
 * If the left side is evaluated first (as an lvalue address), and evaluating
 * the right side triggers a realloc of exprInfo, then the address assigned to
 * is invalid.
 */
#define CAREFUL_EXPAND(x) { Expr _new_ = expand_expr(x); x = _new_; }


INTERNAL void push_macro_invocation(Expr expr)
{
        /* Debug code 2019/03
        File file; int offset;
        find_expr_position(expr, &file, &offset);
        String filepath = fileInfo[file].filepath;
        MSG(lvl_info, "pushing macro invocation at %s offset %d\n", string_buffer(filepath), offset);
        */
        int idx = macroInvocationStackSize++;
        RESIZE_GLOBAL_BUFFER(macroInvocationStack, macroInvocationStackSize);
        macroInvocationStack[idx] = expr;
}

INTERNAL void pop_macro_invocation(void)
{
        ASSERT(macroInvocationStackSize > 0);
        macroInvocationStackSize--;
}

INTERNAL
Expr expand_expr(Expr x)
{
        if (exprInfo[x].exprKind == EXPR_SYMREF) {

                /* Expressions that reference a MacroParam are somewhat special since
                 * here we don't expand the reference but the bound expression */
                Symref ref = exprInfo[x].tSymref.ref;
                Symbol sym = symrefToSym[ref];
                ASSERT(sym != (Symbol) -1);

                if (symbolInfo[sym].symbolKind == SYMBOL_MACROPARAM) {
                        Expr expr = lookup_macro_param(symbolInfo[sym].tMacroParam);
                        return expand_expr(expr);
                }
                else if (symbolInfo[sym].symbolKind == SYMBOL_MACRO) {
                        Macro macro = symbolInfo[sym].tMacro;
                        int macroKind = macroInfo[macro].macroKind;
                        if (macroInfo[macro].macroKind != MACRO_VALUE) {
                                FATAL("Invalid use of %s macro. Expected %s macro\n",
                                      macroKindString[macroKind],
                                      macroKindString[MACRO_VALUE]);
                        }
                        push_macro_invocation(x);
                        Expr expanded = expand_expr(macroInfo[macro].expr);
                        pop_macro_invocation();
                        return expanded;
                }
                else {
                        goto isnotamacro;
                }
        }

        else if (exprInfo[x].exprKind == EXPR_CALL) {

                Expr callee = exprInfo[x].tCall.callee;
                if (exprInfo[callee].exprKind != EXPR_SYMREF)
                        goto isnotamacro;

                Symref ref = exprInfo[callee].tSymref.ref;
                Symbol sym = symrefToSym[ref];
                ASSERT(sym != (Symbol) -1);
                if (symbolInfo[sym].symbolKind != SYMBOL_MACRO)
                        goto isnotamacro;

                Macro macro = symbolInfo[sym].tMacro;
                int macroKind = macroInfo[macro].macroKind;
                if (macroInfo[macro].macroKind != MACRO_FUNCTION)
                        FATAL("Cannot call a %s macro.\n",
                              macroKindString[macroKind]);

                MacroParam firstMacroParam = macroInfo[macro].tFunction.firstMacroParam;
                int nparams = macroInfo[macro].tFunction.nparams;
                int nargs = exprInfo[x].tCall.nargs;
                if (nparams != nargs)
                        FATAL("Bad invocation: macro %s has %d parameters "
                              "but invocation provides %d arguments (%s)\n",
                              SS(macroInfo[macro].symbol), nparams, nargs,
                              nargs < nparams ? "too few" : "too many");

                int firstBoundArg = macroBoundArgCnt;
                int firstArgIdx = exprInfo[x].tCall.firstArgIdx;
                macroBoundArgCnt += nargs;
                RESIZE_GLOBAL_BUFFER(macroBoundArg, macroBoundArgCnt);
                for (int i = 0; i < nargs; i++) {
                        MacroParam macroParam = firstMacroParam + i;
                        int callArg = firstArgIdx + i;
                        ASSERT(callArgInfo[callArg].callExpr == x);
                        Expr expr = callArgInfo[callArg].argExpr;
                        /* Bind the argument (expression) to the formal macro
                         * parameter */
                        macroBoundArg[firstBoundArg + i].macroParam = macroParam;
                        macroBoundArg[firstBoundArg + i].expr = expr;
                        DEBUG("bind macroparam=%d expr=%d\n", macroParam, expr);
                }
                DEBUG("PATCH expression, expand macro=%d!\n", macro);

                push_macro_invocation(x);
                Expr y = expand_expr(macroInfo[macro].expr);
                pop_macro_invocation();

                macroBoundArgCnt -= nargs;
                return y;
        }

isnotamacro:
        {
                Expr y = exprCnt++;
                RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
                exprInfo[y] = exprInfo[x];

                // the proc member should probably be removed at some point.
                // Currently we need to override it because some expressions are
                // defined in a macro so they don't have the right proc set yet.
                ASSERT(exprInfo[y].proc == (Proc) -1 ||
                       exprInfo[y].proc == CURRENTLY_EXPANDED_PROC);
                exprInfo[y].proc = CURRENTLY_EXPANDED_PROC;

                switch (exprInfo[x].exprKind) {
                case EXPR_LITERAL:
                        break;
                case EXPR_SYMREF: {
                        Symref ref = exprInfo[x].tSymref.ref;
                        Symbol sym = symrefToSym[ref];
                        ASSERT(sym != (Symbol) -1);
                        ASSERT(symbolInfo[sym].symbolKind != SYMBOL_MACROPARAM);
                        /* don't change anything */
                        break;
                }
                case EXPR_UNOP:
                        CAREFUL_EXPAND(exprInfo[y].tUnop.expr);
                        break;
                case EXPR_BINOP:
                        CAREFUL_EXPAND(exprInfo[y].tBinop.expr1);
                        CAREFUL_EXPAND(exprInfo[y].tBinop.expr2);
                        break;
                case EXPR_MEMBER:
                        CAREFUL_EXPAND(exprInfo[y].tMember.expr);
                        break;
                case EXPR_SUBSCRIPT:
                        CAREFUL_EXPAND(exprInfo[y].tSubscript.expr1);
                        CAREFUL_EXPAND(exprInfo[y].tSubscript.expr2);
                        break;
                case EXPR_CALL: {
                        /* Not a macro invocation! We covered that case above */
                        CAREFUL_EXPAND(exprInfo[y].tCall.callee);
                        /* To make the arguments sequential without sorting,
                         * allocate
                           them in one big chunk */
                        int nargs = exprInfo[x].tCall.nargs;
                        int first = callArgCnt;
                        int firstOld = exprInfo[x].tCall.firstArgIdx;
                        callArgCnt += nargs;
                        RESIZE_GLOBAL_BUFFER(callArgInfo, callArgCnt);
                        exprInfo[y].tCall.firstArgIdx = first;
                        exprInfo[y].tCall.nargs = nargs;
                        for (int i = 0; i < nargs; i++) {
                                Expr oldArgExpr = callArgInfo[firstOld + i].argExpr;
                                Expr argExpr = expand_expr(oldArgExpr);
                                callArgInfo[first + i].callExpr = y;
                                callArgInfo[first + i].argExpr = argExpr;
                        }
                        break;
                }
                case EXPR_COMPOUND: {
                        int first = exprInfo[x].tCompound.firstCompoundExprLink;
                        int numChilds = exprInfo[x].tCompound.numChilds;
                        /* XXX here we don't allocate new compoundExprLinks
                         * but simply overwrite the old one. Should we create
                         * new ones? */
                        exprInfo[y].tCompound.firstCompoundExprLink = first;
                        exprInfo[y].tCompound.numChilds = numChilds;
                        for (int i = 0; i < numChilds; i++) {
                                Expr childExpr = compoundExprLink[first + i].childExpr;
                                Expr newChild = expand_expr(childExpr);
                                compoundExprLink[first + i].parentExpr = y;
                                compoundExprLink[first + i].childExpr = newChild;
                        }
                        break;
                }
                case EXPR_COMPILERVALUE:
                        /* XXX: compiler values may depend on the location of
                        the source token. That is a problem if the compiler
                        value expression is part of a macro body, when we
                        really wanted to get information related to the place
                        of invocation of the macro.
                        For now we will hack in the first token of the macro
                        invocation. Not sure if we will ever find a better
                        solution. */
                        if (macroInvocationStackSize > 0) {
                                Expr mx = macroInvocationStack[macroInvocationStackSize - 1];
                                exprInfo[y].tCompilervalue.hashtok = find_expr_token(mx);
                        }
                        break;
                case EXPR_COMPILERCALL:
                        CAREFUL_EXPAND(exprInfo[y].tCompilercall.expr);
                        break;
                default:
                        UNHANDLED_CASE();
                }
                return y;
        }
}

INTERNAL
void expand_stmt_exprs(Stmt a)
{
        switch (stmtInfo[a].stmtKind) {
        case STMT_IF: {
                EXPAND(stmtInfo[a].tIf.condExpr);
                expand_stmt_exprs(stmtInfo[a].tIf.ifbody);
                break;
        }
        case STMT_IFELSE: {
                EXPAND(stmtInfo[a].tIfelse.condExpr);
                expand_stmt_exprs(stmtInfo[a].tIfelse.ifbody);
                expand_stmt_exprs(stmtInfo[a].tIfelse.elsebody);
                break;
        }
        case STMT_FOR: {
                expand_stmt_exprs(stmtInfo[a].tFor.initStmt);
                EXPAND(stmtInfo[a].tFor.condExpr);
                expand_stmt_exprs(stmtInfo[a].tFor.stepStmt);
                expand_stmt_exprs(stmtInfo[a].tFor.forbody);
                break;
        }
        case STMT_WHILE: {
                EXPAND(stmtInfo[a].tWhile.condExpr);
                expand_stmt_exprs(stmtInfo[a].tWhile.whilebody);
                break;
        }
        case STMT_RANGE: {
                EXPAND(stmtInfo[a].tRange.startExpr);
                EXPAND(stmtInfo[a].tRange.stopExpr);
                expand_stmt_exprs(stmtInfo[a].tRange.rangebody);
                break;
        }
        case STMT_RETURN: {
                EXPAND(stmtInfo[a].tReturn.expr);
                break;
        }
        case STMT_EXPR: {
                EXPAND(stmtInfo[a].tExpr.expr);
                break;
        }
        case STMT_COMPOUND: {
                int first = stmtInfo[a].tCompound.firstChildStmtIdx;
                int c = stmtInfo[a].tCompound.numStatements;
                for (int child = first; child < first + c; child++) {
                        ASSERT(childStmtInfo[child].parent == a);
                        expand_stmt_exprs(childStmtInfo[child].child);
                }
                break;
        }
        case STMT_DATA:
                if (stmtInfo[a].tData.optionalInitializerExpr != (Expr) -1)
                        EXPAND(stmtInfo[a].tData.optionalInitializerExpr);
                break;
        case STMT_MACRO:
                /* ignore */
                break;
        case STMT_IGNORE: {
                /* don't ignore :-). We actually want to expand so we can
                 * typecheck. That's the point! */
                expand_stmt_exprs(stmtInfo[a].tIgnore);
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

void expand_macros(void)
{
        for (Proc p = 0; p < procCnt; p++) {
                CURRENTLY_EXPANDED_PROC = p;
                expand_stmt_exprs(procInfo[p].body);
        }
        for (Constant constant = 0; constant < constantCnt; constant++) {
                if (constantInfo[constant].constantKind != CONSTANT_EXPRESSION)
                        continue;
                CURRENTLY_EXPANDED_PROC = (Proc) -1;
                /*
                DEBUG("macroexpand constant %s constant-expr=%d\n",
                        SS(constantInfo[constant].symbol),
                        constantInfo[constant].tExpr);
                        */
                EXPAND(constantInfo[constant].tExpr);
        }
}
