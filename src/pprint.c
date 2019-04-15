#include "defs.h"
#include "api.h"

static int indentSize;


INTERNAL void pp_expr(Expr expr);
INTERNAL void pp_stmt(Stmt stmt, int suppressnewline);


INTERNAL
void add_indent(void)
{
        indentSize += 4;
}

INTERNAL
void remove_indent(void)
{
        indentSize -= 4;
}

INTERNAL
void pp_newline(void)
{
        outs("\n");
        for (int i = 0; i < indentSize; i++)
                outs(" ");
}

INTERNAL
void pp_type(Type tp)
{
        if (tp < 0) {
                outs("(Bad type)");
                return;
        }
        switch (typeInfo[tp].typeKind) {
        case TYPE_BASE:
                outs(string_buffer(typeInfo[tp].tBase.name));
                break;
        case TYPE_ARRAY:
                outs("(array type)");
                break;
        case TYPE_POINTER:
                pp_type(typeInfo[tp].tPointer.tp);
                outs("*");
                break;
        case TYPE_PROC:
                pp_type(typeInfo[tp].tProc.rettp); //XXX
                break;
        case TYPE_REFERENCE:
                outs(SRS(typeInfo[tp].tRef.ref));
                break;
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL
void pp_data(Data d)
{
        pp_newline();
        outs("data ");
        pp_type(dataInfo[d].tp);
        outs(" ");
        outs(SS(dataInfo[d].sym));
        outs(";");
}

INTERNAL
void pp_literal_expr(Expr expr)
{
        switch (exprInfo[expr].tLiteral.literalKind) {
        case LITERAL_INTEGER: {
                Token tok = exprInfo[expr].tLiteral.tok;
                outf("%lld", tokenInfo[tok].tInteger);
                break;
        }
        case LITERAL_STRING: {
                String s = exprInfo[expr].tLiteral.tString;
                outf("\"%s\"", string_buffer(s));
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL
void pp_symref_expr(Expr expr)
{
        String s = symrefInfo[exprInfo[expr].tSymref.ref].name;
        outs(string_buffer(s));
}

INTERNAL
void pp_unop_expr(Expr expr)
{
        int unop = exprInfo[expr].tUnop.unopKind;
        int isprefix = unopIsPrefix[unop];
        const char *str = unopString[unop];
        if (isprefix)
                outs(str);
        pp_expr(exprInfo[expr].tUnop.expr);
        if (!isprefix)
                outs(str);
}

INTERNAL
void pp_binop_expr(Expr expr)
{
        pp_expr(exprInfo[expr].tBinop.expr1);
        int binop = exprInfo[expr].tBinop.binopKind;
        outs(" ");
        outs(binopString[binop]);
        outs(" ");
        pp_expr(exprInfo[expr].tBinop.expr2);
}

INTERNAL
void pp_member_expr(Expr expr)
{
        pp_expr(exprInfo[expr].tMember.expr);
        outs(".");
        outs(string_buffer(exprInfo[expr].tMember.name));
}

INTERNAL
void pp_subscript_expr(Expr expr)
{
        pp_expr(exprInfo[expr].tSubscript.expr1);
        outs("[");
        pp_expr(exprInfo[expr].tSubscript.expr2);
        outs("]");
}

INTERNAL
void pp_call_expr(Expr expr)
{
        Expr callee = exprInfo[expr].tCall.callee;
        int first = exprInfo[expr].tCall.firstArgIdx;
        int last = first + exprInfo[expr].tCall.nargs;
        pp_expr(callee);
        outs("(");
        for (int i = first; i < last; i++) {
                if (i > first)
                        outs(", ");
                pp_expr(callArgInfo[i].argExpr);
        }
        outs(")");
}

INTERNAL
void pp_compound_expr(Expr expr)
{
        int first = exprInfo[expr].tCompound.firstCompoundExprLink;
        int numChilds = exprInfo[expr].tCompound.numChilds;
        outs("{");
        for (int i = 0; i < numChilds; i++) {
                if (i > 0)
                        outs(",");
                outs(" ");
                pp_expr(compoundExprLink[first + i].childExpr);
        }
        outs(" }");
}

INTERNAL void pp_compilervalue_expr(Expr expr)
{
        int kind = exprInfo[expr].tCompilervalue.compilervalueKind;
        outs("#");
        outs(compilervalueKindString[kind]);
}

INTERNAL void pp_compilercall_expr(Expr expr)
{
        int kind = exprInfo[expr].tCompilercall.compilercallKind;
        outs("#");
        outs(compilercallKindString[kind]);
        outs("(");
        pp_expr(expr);
        outs(")");
}

INTERNAL void pp_childstmt(Stmt stmt)  /* helper */
{
        if (stmtInfo[stmt].stmtKind == STMT_COMPOUND) {
                outs(" ");
                pp_stmt(stmt, 0);
        }
        else {
                add_indent();
                pp_stmt(stmt, 0);
                remove_indent();
        }
}

INTERNAL void pp_expr_stmt(Stmt stmt)
{
        pp_expr(stmtInfo[stmt].tExpr.expr);
        outs(";");
}

INTERNAL void pp_if_stmt(Stmt stmt)
{
        Stmt ifbody = stmtInfo[stmt].tIf.ifbody;
        outs("if (");
        pp_expr(stmtInfo[stmt].tIf.condExpr);
        outs(")");
        pp_childstmt(ifbody);
}

INTERNAL void pp_ifelse_stmt(Stmt stmt)
{
        outs("if (");
        pp_expr(stmtInfo[stmt].tIfelse.condExpr);
        outs(")");
        pp_childstmt(stmtInfo[stmt].tIfelse.ifbody);
        pp_newline();
        outs("else");
        pp_childstmt(stmtInfo[stmt].tIfelse.elsebody);
}

INTERNAL void pp_for_stmt(Stmt stmt)
{
        outs("for (");
        pp_stmt(stmtInfo[stmt].tFor.initStmt, 0);
        outs("; ");
        pp_expr(stmtInfo[stmt].tFor.condExpr);
        outs("; ");
        pp_expr_stmt(stmtInfo[stmt].tFor.stepStmt);
        outs(")");
        pp_childstmt(stmtInfo[stmt].tFor.forbody);
}

INTERNAL void pp_range_stmt(Stmt stmt)
{
        Symbol sym = dataInfo[stmtInfo[stmt].tRange.variable].sym;
        outs("for ");
        outs(SS(sym));
        outs(" from ");
        pp_expr(stmtInfo[stmt].tRange.startExpr);
        outs(" to ");
        pp_expr(stmtInfo[stmt].tRange.stopExpr);
        outs(" do ");
        pp_childstmt(stmtInfo[stmt].tRange.rangebody);
}

INTERNAL void pp_while_stmt(Stmt stmt)
{
        outs("while (");
        pp_expr(stmtInfo[stmt].tWhile.condExpr);
        outs(")");
        pp_childstmt(stmtInfo[stmt].tWhile.whilebody);
}

INTERNAL void pp_return_stmt(Stmt stmt)
{
        outs("return ");
        pp_expr(stmtInfo[stmt].tReturn.expr);
        outs(";");
}

INTERNAL void pp_compound_stmt(Stmt stmt)
{
        outs("{");
        add_indent();
        for (int i = stmtInfo[stmt].tCompound.firstChildStmtIdx;
             i < childStmtCnt && childStmtInfo[i].parent == stmt; i++) {
                pp_stmt(childStmtInfo[i].child, 0);
        }
        remove_indent();
        pp_newline();
        outs("}");
}

INTERNAL void pp_data_stmt(Stmt stmt)
{
        Data data = stmtInfo[stmt].tData.data;
        Expr expr = stmtInfo[stmt].tData.optionalInitializerExpr;
        pp_newline();
        outs("data ");
        outs(SS(dataInfo[data].sym));
        outs(" ");
        pp_type(dataInfo[data].tp);
        if (expr != (Expr) -1) {
                outs(" = ");
                pp_expr(expr);
        }
        outs(";");
}

INTERNAL void pp_macro_stmt(Stmt stmt)
{
        Macro macro = stmtInfo[stmt].tMacro;
        pp_newline();
        outs("macro ");
        outs(SS(macroInfo[macro].symbol));
        outs(" => ");
        pp_expr(macroInfo[macro].expr);
        outs(";");
}

INTERNAL void (*const exprKindToPrintFunc[NUM_EXPR_KINDS])(Expr expr) = {
        [EXPR_LITERAL]   = pp_literal_expr,
        [EXPR_SYMREF]    = pp_symref_expr,
        [EXPR_UNOP]      = pp_unop_expr,
        [EXPR_BINOP]     = pp_binop_expr,
        [EXPR_MEMBER]    = pp_member_expr,
        [EXPR_SUBSCRIPT] = pp_subscript_expr,
        [EXPR_CALL]      = pp_call_expr,
        [EXPR_COMPOUND]  = pp_compound_expr,
        [EXPR_COMPILERVALUE] = pp_compilervalue_expr,
        [EXPR_COMPILERCALL] = pp_compilercall_expr,
};

INTERNAL void (*const stmtKindToPrintFunc[NUM_STMT_KINDS])(Stmt stmt) = {
        [STMT_EXPR]     = pp_expr_stmt,
        [STMT_IF]       = pp_if_stmt,
        [STMT_IFELSE]   = pp_ifelse_stmt,
        [STMT_FOR]      = pp_for_stmt,
        [STMT_RANGE]    = pp_range_stmt,
        [STMT_WHILE]    = pp_while_stmt,
        [STMT_RETURN]   = pp_return_stmt,
        [STMT_COMPOUND] = pp_compound_stmt,
        [STMT_DATA]     = pp_data_stmt,
        [STMT_MACRO]    = pp_macro_stmt,
};

INTERNAL
void pp_expr(Expr expr)
{
        int exprKind = exprInfo[expr].exprKind;
        ASSERT(0 <= exprKind && exprKind < NUM_EXPR_KINDS);
        ASSERT(exprKindToPrintFunc[exprKind]);
        exprKindToPrintFunc[exprKind](expr);
}

INTERNAL
void pp_stmt(Stmt stmt, int suppressnewline)
{
        if (! suppressnewline)
                pp_newline();
        int stmtKind = stmtInfo[stmt].stmtKind;
        ASSERT(0 <= stmtKind && stmtKind < NUM_STMT_KINDS);
        ASSERT(stmtKindToPrintFunc[stmtKind]);
        stmtKindToPrintFunc[stmtKind](stmt);
}

INTERNAL
void pp_proc(Proc p)
{
        outs("\n");
        outs("proc ");
        pp_type(procToType[p]);
        outs(" ");
        outs(SS(procInfo[p].sym));
        outs("(");
        int firstParam = firstProctypeParam[procToType[p]];
        int nparams = typeInfo[procToType[p]].tProc.nparams;
        for (int i = 0; i < nparams; i++) {
                if (i > 0)
                        outs(", ");
                pp_type(paramInfo[firstParam+i].tp);
                outs(" ");
                outs(SS(paramInfo[firstParam+i].sym));
        }
        outs(")");
        pp_newline();
        pp_compound_stmt(procInfo[p].body);
        outs("\n");
}

INTERNAL
void pp_export(Export x)
{
        Symref ref = exportInfo[x].ref;
        String name = symrefInfo[ref].name;
        outf("export %s;\n", string_buffer(name));
}

void prettyprint(void)
{
        pp_newline();
        for (Data i = 0; i < dataCnt; i++)
                if (scopeInfo[dataInfo[i].scope].scopeKind == SCOPE_GLOBAL)
                        pp_data(i);
        pp_newline();
        for (Proc i = 0; i < procCnt; i++)
                pp_proc(i);
        pp_newline();
        for (Export x = 0; x < exportCnt; x++)
                pp_export(x);
        pp_newline();
}
