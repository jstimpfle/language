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
        switch (typeInfo[tp].kind) {
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
void pp_array(Array a)
{
        Type t = arrayInfo[a].tp;
        pp_newline();
        outs("array ");
        pp_type(typeInfo[t].tArray.valuetp);
        outs(" ");
        outs(SS(arrayInfo[a].sym));
        outs("[");
        pp_type(typeInfo[t].tArray.idxtp);
        outs("]");
        outs(";");
}

INTERNAL
void pp_expr(Expr expr)
{
        switch (exprInfo[expr].kind) {
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
                        int unop = exprInfo[expr].tUnop.kind;
                        int isprefix = unopInfo[unop].isprefix;
                        const char *str = unopInfo[unop].str;
                        if (isprefix)
                                outs(str);
                        pp_expr(exprInfo[expr].tUnop.expr);
                        if (!isprefix)
                                outs(str);
                        break;
                }
                case EXPR_BINOP: {
                        pp_expr(exprInfo[expr].tBinop.expr1);
                        int binop = exprInfo[expr].tBinop.kind;
                        outs(" ");
                        outs(binopInfo[binop].str);
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
                default:
                        UNHANDLED_CASE();
        }
}

INTERNAL
void pp_expr_stmt(Stmt stmt)
{
        pp_newline();
        pp_expr(stmtInfo[stmt].tExpr.expr);
        outs(";");
}

INTERNAL void pp_stmt(Stmt stmt);

INTERNAL
void pp_compound_stmt(Stmt stmt)
{
        outs("{");
        add_indent();
        for (int i = stmtInfo[stmt].tCompound.firstChildStmtIdx;
             i < childStmtCnt && childStmtInfo[i].parent == stmt; i++) {
                pp_stmt(childStmtInfo[i].child);
        }
        remove_indent();
        pp_newline();
        outs("}");
}

INTERNAL
void pp_data_stmt(Stmt stmt)
{
        pp_data(stmtInfo[stmt].tData);
}

INTERNAL
void pp_array_stmt(Stmt stmt)
{
        pp_array(stmtInfo[stmt].tArray);
}

INTERNAL
void pp_childstmt(Stmt stmt)
{
        if (stmtInfo[stmt].kind == STMT_COMPOUND) {
                outs(" ");
                pp_stmt(stmt);
        }
        else {
                add_indent();
                pp_stmt(stmt);
                remove_indent();
        }
}

INTERNAL
void pp_stmt(Stmt stmt)
{
        switch (stmtInfo[stmt].kind) {
        case STMT_IF: {
                Stmt ifbody = stmtInfo[stmt].tIf.ifbody;
                pp_newline();
                outs("if (");
                pp_expr(stmtInfo[stmt].tIf.condExpr);
                outs(")");
                pp_childstmt(ifbody);
                break;
        }
        case STMT_IFELSE: {
                pp_newline();
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
                pp_newline();
                outs("for (");
                pp_stmt(stmtInfo[stmt].tFor.initStmt);
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
                pp_newline();
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
                pp_newline();
                outs("while (");
                pp_expr(stmtInfo[stmt].tWhile.condExpr);
                outs(")");
                pp_childstmt(stmtInfo[stmt].tWhile.whilebody);
                break;
        case STMT_RETURN:
                pp_newline();
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
        default:
                ASSERT(0 && "Unhandled!\n");
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
        for (int i = 0; i < procInfo[p].nparams; i++) {
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

void prettyprint(void)
{
        for (Type i = 0; i < typeCnt; i++)
                if (typeInfo[i].kind == TYPE_ENTITY)
                        pp_entity(i);
        pp_newline();
        for (Data i = 0; i < dataCnt; i++)
                if (scopeInfo[dataInfo[i].scope].scopeKind == globalScope)
                        pp_data(i);
        pp_newline();
        for (Array i = 0; i < arrayCnt; i++)
                if (scopeInfo[arrayInfo[i].scope].scopeKind == globalScope)
                        pp_array(i);
        pp_newline();
        for (Proc i = 0; i < procCnt; i++)
                pp_proc(i);
        pp_newline();
}
