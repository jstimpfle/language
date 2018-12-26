#include "defs.h"
#include "api.h"

INTERNAL
Expr lookup_macro_param(MacroParam macroParam)
{
        for (int i = 0; i < macroBoundArgCnt; i++)
                if (macroParam == macroBoundArg[i].macroParam)
                        return macroBoundArg[i].expr;
        /* the symbol resolution phase should have worked out this error */
        ASSERT(0);
}

INTERNAL
Expr clone_expr(Expr src, Proc origProc)
{
        /* The origProc argument is needed because we associate Expr's with the
         * proc they "execute in". Since macros can be defined outside procs and
         * be used in multiple distinct procs, we need to fixup this information
         * when cloning the macro bodies. */

        /* Expressions that reference a MacroParam are somewhat special since
         * here we don't clone the reference but the bound expression */
        if (exprInfo[src].exprKind == EXPR_SYMREF) {
                Symref ref = exprInfo[src].tSymref.ref;
                Symbol sym = symrefToSym[ref];
                ASSERT(sym != (Symbol) -1);
                if (symbolInfo[sym].symbolKind == SYMBOL_MACROPARAM) {
                        Expr expr = lookup_macro_param(
                                                symbolInfo[sym].tMacroParam);
                        return clone_expr(expr, origProc);
                }
        }

        Expr dst = exprCnt++;
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        exprInfo[dst] = exprInfo[src];  /* HACK copy the whole struct no matter
                                           what its kind is */
        exprInfo[dst].proc = origProc;  /* ... but fix the proc */

        switch (exprInfo[src].exprKind) {
        case EXPR_LITERAL:
                break;
        case EXPR_SYMREF:
                /* since the test above failed, the symref does not reference a
                   macro parameter and we are fine */
                break;
        case EXPR_UNOP:
                exprInfo[dst].tUnop.expr = clone_expr(exprInfo[src].tUnop.expr, origProc);
                break;
        case EXPR_BINOP:
                exprInfo[dst].tBinop.expr1 = clone_expr(exprInfo[src].tBinop.expr1, origProc);
                exprInfo[dst].tBinop.expr2 = clone_expr(exprInfo[src].tBinop.expr2, origProc);
                break;
        case EXPR_MEMBER:
                exprInfo[dst].tMember.expr = clone_expr(exprInfo[src].tMember.expr, origProc);
                break;
        case EXPR_SUBSCRIPT:
                exprInfo[dst].tSubscript.expr1 = clone_expr(exprInfo[src].tSubscript.expr1, origProc);
                exprInfo[dst].tSubscript.expr2 = clone_expr(exprInfo[src].tSubscript.expr2, origProc);
                break;
        case EXPR_CALL: {
                exprInfo[dst].tCall.callee = clone_expr(exprInfo[src].tCall.callee, origProc);
                /* To make the arguments sequential without sorting, allocate
                   them in one big chunk */
                int nargs = exprInfo[src].tCall.nargs;
                int first = callArgCnt;
                int last = first + nargs;
                int firstOld = exprInfo[src].tCall.firstArgIdx;
                int lastOld = first + nargs;
                callArgCnt += nargs;
                RESIZE_GLOBAL_BUFFER(callArgInfo, callArgCnt);
                exprInfo[dst].tCall.firstArgIdx = first;
                exprInfo[dst].tCall.nargs = nargs;
                for (int i = 0; i < nargs; i++) {
                        Expr oldArgExpr = callArgInfo[firstOld + i].argExpr;
                        callArgInfo[first + i].callExpr = dst;
                        callArgInfo[first + i].argExpr = clone_expr(oldArgExpr, origProc);
                }
                break;
        }
        default:
                UNHANDLED_CASE();
        }
        return dst;
}

void expand_macros(void)
{
        /* TODO: Macros that call other macros do not work yet. We need
         * to figure out the call dependencies, make sure they form a DAG,
         * and then maybe expand the emacro bodies themselves first. */
        for (Expr x = 0; x < exprCnt; x++) {
                if (exprInfo[x].exprKind != EXPR_CALL)
                        continue;
                Expr callee = exprInfo[x].tCall.callee;
                if (exprInfo[callee].exprKind != EXPR_SYMREF)
                        continue;
                Symref ref = exprInfo[callee].tSymref.ref;
                Symbol sym = symrefToSym[ref];
                ASSERT(sym != (Symbol) -1);
                if (symbolInfo[sym].symbolKind != SYMBOL_MACRO)
                        continue;
                Macro macro = symbolInfo[sym].tMacro;
                MacroParam firstMacroParam = macroInfo[macro].firstMacroParam;
                int nparams = macroInfo[macro].nparams;
                int firstArgIdx = exprInfo[x].tCall.firstArgIdx;
                int nargs = exprInfo[x].tCall.nargs;
                if (macroInfo[macro].nparams != nargs) {
                        FATAL("Bad macro invocation: macro %s has %d "
                              "parameters, but invocation provides %d\n",
                              SS(macroInfo[macro].symbol), nparams, nargs);
                }
                macroBoundArgCnt = nargs;
                RESIZE_GLOBAL_BUFFER(macroBoundArg, macroBoundArgCnt);
                for (int i = 0; i < nargs; i++) {
                        MacroParam macroParam = firstMacroParam + i;
                        int callArg = firstArgIdx + i;
                        ASSERT(callArgInfo[callArg].callExpr == x);
                        Expr expr = callArgInfo[callArg].argExpr;
                        Token paramToken = macroParamInfo[macroParam].token;
                        /* Bind the argument (expression) to the formal macro
                         * parameter */
                        macroBoundArg[i].macroParam = macroParam;
                        macroBoundArg[i].expr = expr;
                }
                DEBUG("PATCH expression, expand macro=%d!\n", macro);
                Proc origProc = exprInfo[x].proc;
                Expr xpanded = clone_expr(macroInfo[macro].expr, origProc);
                // XXX: ugly hack. patch the expression
                exprInfo[x] = exprInfo[xpanded];
        }
}
