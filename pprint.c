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
        if (tp < 0) {
                msg("(Bad type)");
                return;
        }
        switch (typeInfo[tp].kind) {
        case TYPE_BASE:
                msg("%s", string_buffer(typeInfo[tp].tBase.name));
                break;
        case TYPE_ENTITY:
                msg("%s", string_buffer(typeInfo[tp].tEntity.name));
                break;
        case TYPE_ARRAY:
                msg("(array type)");
                break;
        case TYPE_PROC:
                msg("(proc type)");
                break;
        case TYPE_REFERENCE:
                msg("%s", SRS(typeInfo[tp].tRef.ref));
                break;
        default:
                UNHANDLED_CASE();
        }
}

void pprint_entity(Type t)
{
        pprint_newline();
        msg("entity ");
        pprint_type(typeInfo[t].tEntity.tp);
        msg(" ");
        msg("%s", SS(typeInfo[t].tEntity.name));
        msg(";");
}

void pprint_data(Data d)
{
        pprint_newline();
        msg("data ");
        pprint_type(dataInfo[d].tp);
        msg(" ");
        msg("%s", SS(dataInfo[d].sym));
        msg(";");
}

void pprint_array(Array a)
{
        Type t = arrayInfo[a].tp;
        pprint_newline();
        msg("array ");
        pprint_type(typeInfo[t].tArray.valuetp);
        msg(" ");
        msg("%s", SS(arrayInfo[a].sym));
        msg("[");
        pprint_type(typeInfo[t].tArray.idxtp);
        msg("]");
        msg(";");
}

void pprint_expr(Expr expr)
{
        switch (exprInfo[expr].kind) {
                case EXPR_SYMREF: {
                        String s = symrefInfo[exprInfo[expr].tSymref.ref].name;
                        msg("%s", string_buffer(s));
                        break;
                }
                case EXPR_LITERAL: {
                        Token tok = exprInfo[expr].tLiteral.tok;
                        msg("%lld", tokenInfo[tok].tInteger);
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
                case EXPR_MEMBER: {
                        pprint_expr(exprInfo[expr].tMember.expr);
                        msg(".");
                        msg("%s", string_buffer(exprInfo[expr].tMember.name));
                        break;
                }
                case EXPR_SUBSCRIPT: {
                        pprint_expr(exprInfo[expr].tSubscript.expr1);
                        msg("[");
                        pprint_expr(exprInfo[expr].tSubscript.expr2);
                        msg("]");
                        break;
                }
                case EXPR_CALL: {
                        Expr callee = exprInfo[expr].tCall.callee;
                        int first = exprInfo[expr].tCall.firstArgIdx;
                        int last = first + exprInfo[expr].tCall.nargs;
                        pprint_expr(callee);
                        msg("(");
                        for (int i = first; i < last; i++) {
                                if (i > first)
                                        msg(", ");
                                pprint_expr(callArgInfo[i].argExpr);
                        }
                        msg(")");
                        break;
                }
                default:
                        assert(0 && "Unhandled!\n");
        }
}

void pprint_expr_stmt(Stmt stmt)
{
        pprint_newline();
        pprint_expr(stmtInfo[stmt].tExpr.expr);
        msg(";");
}

void pprint_stmt(Stmt stmt);

void pprint_compound_stmt(Stmt stmt)
{
        msg("{");
        add_indent();
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
        pprint_data(stmtInfo[stmt].tData);
}

void pprint_array_stmt(Stmt stmt)
{
        pprint_array(stmtInfo[stmt].tArray);
}

void pprint_stmt(Stmt stmt)
{
        switch (stmtInfo[stmt].kind) {
        case STMT_IF: {
                Stmt child = stmtInfo[stmt].tIf.childStmt;
                int isCompound = stmtInfo[child].kind == STMT_COMPOUND;
                pprint_newline();
                msg("if (");
                pprint_expr(stmtInfo[stmt].tIf.condExpr);
                msg(")");
                if (isCompound) {
                        msg(" ");
                        pprint_stmt(child);
                }
                else {
                        add_indent();
                        pprint_stmt(child);
                        remove_indent();
                }
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
        case STMT_RETURN:
                pprint_newline();
                msg("return ");
                pprint_expr(stmtInfo[stmt].tReturn.expr);
                msg(";");
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
        case STMT_ARRAY:
                pprint_array_stmt(stmt);
                break;
        default:
                assert(0 && "Unhandled!\n");
        }
}

void pprint_proc(Proc p)
{
        msg("\n");
        msg("proc ");
        pprint_type(procInfo[p].tp);
        msg(" ");
        msg("%s", SS(procInfo[p].sym));
        msg("(");
        int firstParam = procInfo[p].firstParam;
        for (int i = 0; i < procInfo[p].nparams; i++) {
                if (i > 0)
                        msg(", ");
                pprint_type(paramInfo[firstParam+i].tp);
                msg(" %s", SS(paramInfo[firstParam+i].sym));
        }
        msg(")");
        pprint_newline();
        pprint_compound_stmt(procInfo[p].body);
        msg("\n");
}

void prettyprint(void)
{
        for (Type i = 0; i < typeCnt; i++)
                if (typeInfo[i].kind == TYPE_ENTITY)
                        pprint_entity(i);
        pprint_newline();
        for (Data i = 0; i < dataCnt; i++)
                if (scopeInfo[dataInfo[i].scope].kind == globalScope)
                        pprint_data(i);
        pprint_newline();
        for (Array i = 0; i < arrayCnt; i++)
                if (scopeInfo[arrayInfo[i].scope].kind == globalScope)
                        pprint_array(i);
        pprint_newline();
        for (Proc i = 0; i < procCnt; i++)
                pprint_proc(i);
        pprint_newline();
}
