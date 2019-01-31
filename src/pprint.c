#include "defs.h"
#include "api.h"

static int indentSize;

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
        case TYPE_ENTITY:
                outs(string_buffer(typeInfo[tp].tEntity.name));
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
void pp_entity(Type t)
{
        pp_newline();
        outs("entity ");
        pp_type(typeInfo[t].tEntity.tp);
        outs(" ");
        outs(string_buffer(typeInfo[t].tEntity.name));
        outs(";");
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
void pp_expr(Expr expr)
{
        switch (exprInfo[expr].exprKind) {
                case EXPR_SYMREF: {
                        String s = symrefInfo[exprInfo[expr].tSymref.ref].name;
                        outs(string_buffer(s));
                        break;
                }
                case EXPR_LITERAL: {
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
                        break;
                }
                case EXPR_UNOP: {
                        int unop = exprInfo[expr].tUnop.unopKind;
                        int isprefix = unopIsPrefix[unop];
                        const char *str = unopString[unop];
                        if (isprefix)
                                outs(str);
                        pp_expr(exprInfo[expr].tUnop.expr);
                        if (!isprefix)
                                outs(str);
                        break;
                }
                case EXPR_BINOP: {
                        pp_expr(exprInfo[expr].tBinop.expr1);
                        int binop = exprInfo[expr].tBinop.binopKind;
                        outs(" ");
                        outs(binopString[binop]);
                        outs(" ");
                        pp_expr(exprInfo[expr].tBinop.expr2);
                        break;
                }
                case EXPR_MEMBER: {
                        pp_expr(exprInfo[expr].tMember.expr);
                        outs(".");
                        outs(string_buffer(exprInfo[expr].tMember.name));
                        break;
                }
                case EXPR_SUBSCRIPT: {
                        pp_expr(exprInfo[expr].tSubscript.expr1);
                        outs("[");
                        pp_expr(exprInfo[expr].tSubscript.expr2);
                        outs("]");
                        break;
                }
                case EXPR_CALL: {
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
                        break;
                }
                case EXPR_COMPOUND: {
                        int first = exprInfo[expr].tCompound.firstChildLink;
                        int numChilds = exprInfo[expr].tCompound.numChilds;
                        outs("{");
                        for (int i = 0; i < numChilds; i++) {
                                if (i > 0)
                                        outs(",");
                                outs(" ");
                                pp_expr(compoundExprLink[first + i].childExpr);
                        }
                        outs(" }");
                        break;
                }
                default:
                        UNHANDLED_CASE();
        }
}

INTERNAL
void pp_expr_stmt(Stmt stmt)
{
        pp_expr(stmtInfo[stmt].tExpr.expr);
        outs(";");
}

INTERNAL void pp_stmt(Stmt stmt, int suppressnewline);

INTERNAL
void pp_compound_stmt(Stmt stmt)
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

INTERNAL
void pp_data_stmt(Stmt stmt)
{
        Data data = stmtInfo[stmt].tData.data;
        Expr expr = stmtInfo[stmt].tData.optionalInitializerExpr;
        pp_newline();
        outs("data ");
        pp_type(dataInfo[data].tp);
        outs(" ");
        outs(SS(dataInfo[data].sym));
        if (expr != (Expr) -1) {
                outs(" = ");
                pp_expr(expr);
        }
        outs(";");

}

INTERNAL
void pp_array_stmt(Stmt stmt)
{
        /* TODO */
        (void) stmt;
}

INTERNAL
void pp_childstmt(Stmt stmt)
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

INTERNAL
void pp_stmt(Stmt stmt, int suppressnewline)
{
        if (! suppressnewline)
                pp_newline();
        switch (stmtInfo[stmt].stmtKind) {
        case STMT_IF: {
                Stmt ifbody = stmtInfo[stmt].tIf.ifbody;
                outs("if (");
                pp_expr(stmtInfo[stmt].tIf.condExpr);
                outs(")");
                pp_childstmt(ifbody);
                break;
        }
        case STMT_IFELSE: {
                outs("if (");
                pp_expr(stmtInfo[stmt].tIfelse.condExpr);
                outs(")");
                pp_childstmt(stmtInfo[stmt].tIfelse.ifbody);
                pp_newline();
                outs("else");
                pp_childstmt(stmtInfo[stmt].tIfelse.elsebody);
                break;
        }
        case STMT_FOR: {
                outs("for (");
                pp_stmt(stmtInfo[stmt].tFor.initStmt, 0);
                outs("; ");
                pp_expr(stmtInfo[stmt].tFor.condExpr);
                outs("; ");
                pp_expr_stmt(stmtInfo[stmt].tFor.stepStmt);
                outs(")");
                pp_childstmt(stmtInfo[stmt].tFor.forbody);
                break;
        }
        case STMT_RANGE: {
                Symbol sym = dataInfo[stmtInfo[stmt].tRange.variable].sym;
                outs("for ");
                outs(SS(sym));
                outs(" from ");
                pp_expr(stmtInfo[stmt].tRange.startExpr);
                outs(" to ");
                pp_expr(stmtInfo[stmt].tRange.stopExpr);
                outs(" do ");
                pp_childstmt(stmtInfo[stmt].tRange.rangebody);
                break;
        }
        case STMT_WHILE:
                outs("while (");
                pp_expr(stmtInfo[stmt].tWhile.condExpr);
                outs(")");
                pp_childstmt(stmtInfo[stmt].tWhile.whilebody);
                break;
        case STMT_RETURN:
                outs("return ");
                pp_expr(stmtInfo[stmt].tReturn.expr);
                outs(";");
                break;
        case STMT_EXPR:
                pp_expr_stmt(stmt);
                break;
        case STMT_COMPOUND:
                pp_compound_stmt(stmt);
                break;
        case STMT_DATA:
                pp_data_stmt(stmt);
                break;
        case STMT_ARRAY:
                pp_array_stmt(stmt);
                break;
        case STMT_MACRO:
                UNHANDLED_CASE(); // TODO
                break;
        case STMT_IGNORE:
                outs("#ignore ");
                pp_stmt(stmtInfo[stmt].tIgnore, 1);
                break;
        default:
                UNHANDLED_CASE();
        }
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
        for (Type i = 0; i < typeCnt; i++)
                if (typeInfo[i].typeKind == TYPE_ENTITY)
                        pp_entity(i);
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
