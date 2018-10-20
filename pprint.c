#include "defs.h"
#include "api.h"

static int indentSize;

void add_indent(void)
{
        indentSize += 4;
}

void remove_indent(void)
{
        indentSize -= 4;
}

void pprint_newline(void)
{
        msg("\n");
        for (int i = 0; i < indentSize; i++)
                msg(" ");
}

void pprint_type(Type tp)
{
        msg("%s", SS(typeInfo[tp].sym));
}

void pprint_data(Data d)
{
        msg("data ");
        pprint_type(dataInfo[d].tp);
        msg(" ");
        msg("%s", SS(dataInfo[d].sym));
        msg(";");
        pprint_newline();
}

void pprint_expr(Expr expr)
{
        switch (exprInfo[expr].kind) {
                case EXPR_SYMREF: {
                        String s = exprInfo[expr].tSymref.name;
                        msg("%s", string_buffer(s));
                        break;
                }
                case EXPR_LITERAL: {
                        Token tok = exprInfo[expr].tLiteral.tok;
                        msg("%lld", tokenInfo[tok].integer);
                        break;
                }
                case EXPR_UNOP: {
                        int unop = exprInfo[expr].tUnop.kind;
                        int isprefix = unopInfo[unop].isprefix;
                        const char *str = unopInfo[unop].str;
                        if (isprefix)
                                msg("%s", str);
                        pprint_expr(exprInfo[expr].tUnop.expr);
                        if (!isprefix)
                                msg("%s", str);
                        break;
                }
                case EXPR_BINOP: {
                        pprint_expr(exprInfo[expr].tBinop.expr1);
                        int binop = exprInfo[expr].tBinop.kind;
                        msg(" %s ", binopInfo[binop].str);
                        pprint_expr(exprInfo[expr].tBinop.expr2);
                        break;
                }
                case EXPR_CALL: {
                        msg("CALL()");
                        break;
                }
                default:
                        assert(0 && "Unhandled!\n");
        }
}

void pprint_expr_stmt(Stmt stmt)
{
        pprint_expr(stmtInfo[stmt].tExpr.expr);
        msg(";");
        pprint_newline();
}

void pprint_stmt(Stmt stmt);

void pprint_compound_stmt(Stmt stmt)
{
        msg("{");
        add_indent();
        pprint_newline();
        for (int i = stmtInfo[stmt].tCompound.firstChildStmtIdx;
             childStmtInfo[i].parent == stmt; i++) {
                pprint_stmt(childStmtInfo[i].child);
        }
        remove_indent();
        pprint_newline();
        msg("}");
}

void pprint_data_stmt(Stmt stmt)
{
        pprint_data(stmtInfo[stmt].tData.data);
}

void pprint_stmt(Stmt stmt)
{
        switch (stmtInfo[stmt].kind) {
        case STMT_IF: {
                Stmt child = stmtInfo[stmt].tIf.childStmt;
                int isCompound = stmtInfo[child].kind == STMT_COMPOUND;
                msg("if (");
                pprint_expr(stmtInfo[stmt].tIf.condExpr);
                msg(")");
                if (isCompound) {
                        msg(" ");
                        pprint_stmt(child);
                }
                else {
                        add_indent();
                        pprint_newline();
                        pprint_stmt(child);
                        remove_indent();
                }
                pprint_newline();
                break;
        }
        case STMT_FOR:
                msg("for (");
                pprint_stmt(stmtInfo[stmt].tFor.initStmt);
                msg("; ");
                pprint_expr(stmtInfo[stmt].tFor.condExpr);
                msg("; ");
                pprint_expr_stmt(stmtInfo[stmt].tFor.stepStmt);
                msg(")");
                add_indent();
                pprint_newline();
                pprint_stmt(stmtInfo[stmt].tFor.childStmt);
                remove_indent();
                pprint_newline();
                break;
        case STMT_WHILE:
                msg("if (");
                pprint_expr(stmtInfo[stmt].tWhile.condExpr);
                msg(")");
                add_indent();
                pprint_newline();
                pprint_stmt(stmtInfo[stmt].tWhile.childStmt);
                remove_indent();
                pprint_newline();
                break;
        case STMT_EXPR:
                pprint_expr_stmt(stmt);
                break;
        case STMT_COMPOUND:
                pprint_compound_stmt(stmt);
                break;
        case STMT_DATA:
                pprint_data_stmt(stmt);
                break;
        default:
                assert(0 && "Unhandled!\n");
        }
}

void pprint_proc(Proc p)
{
        msg("proc ");
        pprint_type(procInfo[p].tp);
        msg(" ");
        msg("%s", SS(procInfo[p].sym));
        msg("(");
        int firstArg = procInfo[p].firstArg;
        for (int i = 0; i < procInfo[p].nargs; i++) {
                if (i > 0)
                        msg(", ");
                pprint_type(procArgInfo[firstArg+i].tp);
                msg(" %s", SS(procArgInfo[firstArg+i].sym));
        }
        msg(")");
        pprint_newline();
        pprint_compound_stmt(procInfo[p].body);
}

void prettyprint(void)
{
        for (Data i = 0; i < dataCnt; i++) {
                //if (scopeInfo[dataInfo[i].scope].kind == global_scope) {
                if (i[dataInfo].scope[scopeInfo].kind == global_scope) {
                        pprint_data(i);
                }
        }
        for (Proc i = 0; i < procCnt; i++) {
                pprint_proc(i);
        }
}
