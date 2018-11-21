#include "defs.h"
#include "api.h"

void compile_expr(Expr x)
{
        Proc proc = exprInfo[x].proc;
        switch (exprInfo[x].kind) {
        case EXPR_LITERAL: {
                IrStmt y = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[y].proc = proc;
                irStmtInfo[y].kind = IRSTMT_LOADCONSTANT;
                irStmtInfo[y].tLoadConstant.constval = tokenInfo[exprInfo[x].tLiteral.tok].tInteger.value; //XXX
                irStmtInfo[y].tLoadConstant.tgtreg = exprToIrReg[x];
                break;
        }
        case EXPR_BINOP: {
                Expr e1 = exprInfo[x].tBinop.expr1;
                Expr e2 = exprInfo[x].tBinop.expr2;
                if (exprInfo[x].tBinop.kind == BINOP_ASSIGN) {
                        /* need special handling: we do not compute the lefthand
                         * value but assign to it. This is lvalue vs rvalue
                         * here, TODO: think of a cleaner way to do assignments,
                         * or at least structure the code better. */
                        assert(exprInfo[e1].kind == EXPR_SYMREF);
                        Symref ref = exprInfo[e1].tSymref.ref;
                        Symbol sym = symrefInfo[ref].sym;
                        if (symbolInfo[sym].kind == SYMBOL_DATA &&
                            symbolInfo[sym].scope == SCOPE_PROC) {
                                Data data = symbolInfo[sym].tData;
                                DEBUG("compile e2...\n");
                                compile_expr(e2);
                                DEBUG("ok\n");
                                IrStmt y = irStmtCnt++;
                                IrReg srcreg = exprToIrReg[e2];
                                IrReg tgtreg = dataToIrReg[data];
                                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                                irStmtInfo[y].proc = proc;
                                irStmtInfo[y].kind = IRSTMT_REGREG;
                                irStmtInfo[y].tRegreg.srcreg = srcreg;
                                irStmtInfo[y].tRegreg.tgtreg = tgtreg;
                                // TODO: move to exprToIrReg[x] ?
                        }
                        else {
                                FATAL("Can't assign to non-local variables yet.\n");
                        }
                }
                else {
                        IrCallArg arg1 = irCallArgCnt++;
                        IrCallArg arg2 = irCallArgCnt++;
                        IrCallResult ret = irCallResultCnt++;
                        IrStmt y = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irCallArgInfo, irCallArgCnt);
                        RESIZE_GLOBAL_BUFFER(irCallResultInfo, irCallResultCnt);
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irCallArgInfo[arg1].srcreg = exprToIrReg[e1];
                        irCallArgInfo[arg2].srcreg = exprToIrReg[e2];
                        irCallArgInfo[arg1].callStmt = y;
                        irCallArgInfo[arg2].callStmt = y;
                        irCallResultInfo[ret].callStmt = y;
                        irCallResultInfo[ret].tgtreg = exprToIrReg[x];
                        // XXX: We "call" the binop operation. This is very inefficient.
                        irStmtInfo[y].proc = proc;
                        irStmtInfo[y].kind = IRSTMT_CALL;
                        irStmtInfo[y].tCall.calleeReg = exprToIrReg[x];
                        irStmtInfo[y].tCall.firstIrCallArg = arg1;
                        irStmtInfo[y].tCall.firstIrCallResult = ret;
                }
                break;
        }
        case EXPR_SYMREF: {
                Type symTp = exprType[x];
                Symref ref = exprInfo[x].tSymref.ref;
                Symbol sym = symrefInfo[ref].sym;
                assert(sym >= 0);
                if (symbolInfo[sym].kind == SYMBOL_DATA &&
                    symbolInfo[sym].scope == SCOPE_PROC) {
                        Data data = symbolInfo[sym].tData;
                        IrStmt y = irStmtCnt++;
                        IrReg srcreg = dataToIrReg[data];
                        IrReg tgtreg = exprToIrReg[x];
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[y].proc = proc;
                        irStmtInfo[y].kind = IRSTMT_REGREG;
                        irStmtInfo[y].tRegreg.srcreg = srcreg;
                        irStmtInfo[y].tRegreg.tgtreg = tgtreg;
                }
                else {
                        IrReg reg = irRegCnt++;
                        IrStmt s = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irRegInfo[reg].proc = proc;
                        irRegInfo[reg].name = intern_cstring("(reg)"); //XXX
                        irRegInfo[reg].sym = sym; //XXX
                        irRegInfo[reg].tp = symTp;
                        irStmtInfo[s].proc = proc;
                        irStmtInfo[s].kind = IRSTMT_LOADSYMBOLADDR;
                        irStmtInfo[s].tLoadSymbolAddr.sym = sym;
                        irStmtInfo[s].tLoadSymbolAddr.tgtreg = exprToIrReg[x];
                }
                break;
        }
        case EXPR_CALL: {
                /*
                 * Evaluate function arguments
                 */
                int firstCallArg = exprInfo[x].tCall.firstArgIdx;
                int lastCallArg = firstCallArg + exprInfo[x].tCall.nargs;
                for (int i = firstCallArg; i < lastCallArg; i++)
                        compile_expr(callArgInfo[i].argExpr);
                /*
                 * Evaluate function to call
                 */
                Expr calleeExpr = exprInfo[x].tCall.callee;
                compile_expr(calleeExpr);
                /*
                 * Emit calling code
                 */
                IrStmt y = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[y].proc = proc;
                irStmtInfo[y].kind = IRSTMT_CALL;
                irStmtInfo[y].tCall.calleeReg = exprToIrReg[calleeExpr];
                irStmtInfo[y].tCall.firstIrCallArg = irCallArgCnt; // XXX: Achtung!
                irStmtInfo[y].tCall.firstIrCallResult = irCallResultCnt; // XXX: Achtung!
                /*
                 * Fix up routing information for arguments
                 */
                for (int i = firstCallArg; i < lastCallArg; i++) {
                        Expr argExpr = callArgInfo[i].argExpr;
                        IrCallArg arg = irCallArgCnt++;
                        RESIZE_GLOBAL_BUFFER(irCallArgInfo, irCallArgCnt);
                        irCallArgInfo[arg].callStmt = y;
                        irCallArgInfo[arg].srcreg = exprToIrReg[argExpr];
                }
                /*
                 * Fix up routing information for result
                 */
                IrCallResult ret = irCallResultCnt++;
                RESIZE_GLOBAL_BUFFER(irCallResultInfo, irCallResultCnt);
                irCallResultInfo[ret].callStmt = y;
                irCallResultInfo[ret].tgtreg = exprToIrReg[x];
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL
void compile_stmt(IrProc irp, Stmt stmt)
{
        switch (stmtInfo[stmt].kind) {
        case STMT_DATA: {
                Data data = stmtInfo[stmt].tData;
                IrReg reg = irRegCnt++;
                RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
                irRegInfo[reg].proc = irp;
                irRegInfo[reg].name = 0; //XXX
                irRegInfo[reg].sym = dataInfo[data].sym;
                irRegInfo[reg].tp = dataInfo[data].tp;
                break;
        }
        case STMT_ARRAY: {
                break;
        }
        case STMT_EXPR: {
                compile_expr(stmtInfo[stmt].tExpr.expr);
                break;
        }
        case STMT_COMPOUND: {
                Stmt first = stmtInfo[stmt].tCompound.firstChildStmtIdx;
                Stmt last = first + stmtInfo[stmt].tCompound.numStatements;
                for (int cld = first; cld < last; cld++)
                        compile_stmt(irp, childStmtInfo[cld].child);
                break;
        }
        case STMT_IF: {
                Expr condExpr = stmtInfo[stmt].tIf.condExpr;
                Stmt cldStmt = stmtInfo[stmt].tIf.childStmt;
                compile_expr(condExpr);
                IrStmt irs = irStmtCnt++;
                compile_stmt(irp, cldStmt);
                IrStmt stmtAfterBlock = irStmtCnt;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[irs].proc = irp;
                irStmtInfo[irs].kind = IRSTMT_CONDGOTO;
                irStmtInfo[irs].tCondGoto.condreg = exprToIrReg[condExpr];
                irStmtInfo[irs].tCondGoto.tgtstmt = stmtAfterBlock;
                irStmtInfo[irs].tCondGoto.isNeg = 1;
                break;
        }
        case STMT_WHILE: {
                Expr condExpr = stmtInfo[stmt].tIf.condExpr;
                Stmt cldStmt = stmtInfo[stmt].tIf.childStmt;
                compile_expr(condExpr);
                IrStmt irs = irStmtCnt++;
                IrStmt firstStmtInBlock = irStmtCnt;
                compile_stmt(irp, cldStmt);
                IrStmt backJmp = irStmtCnt++;
                IrStmt stmtAfterBlock = irStmtCnt;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[irs].proc = irp;
                irStmtInfo[irs].kind = IRSTMT_CONDGOTO;
                irStmtInfo[irs].tCondGoto.condreg = exprToIrReg[condExpr];
                irStmtInfo[irs].tCondGoto.tgtstmt = stmtAfterBlock;
                irStmtInfo[irs].tCondGoto.isNeg = 1;
                irStmtInfo[backJmp].proc = irp;
                irStmtInfo[backJmp].kind = IRSTMT_GOTO;
                irStmtInfo[backJmp].tGoto.tgtstmt = firstStmtInBlock;
                break;
        }
        case STMT_RETURN: {
                Expr resultExpr = stmtInfo[stmt].tReturn.expr;
                compile_expr(resultExpr);
                IrStmt irs = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[irs].proc = irp;
                irStmtInfo[irs].kind = IRSTMT_RETURN;
                irStmtInfo[irs].tReturn.firstResult = irReturnResultCnt;
                /**/
                IrReturnResult ret = irReturnResultCnt++;
                RESIZE_GLOBAL_BUFFER(irReturnResultInfo, irReturnResultCnt);
                irReturnResultInfo[ret].returnStmt = irs;
                irReturnResultInfo[ret].resultReg = exprToIrReg[resultExpr];
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

void compile_to_IR(void)
{
        DEBUG("For each expression find its proc.\n");
        DEBUG("(TODO: think about adding this data already when parsing?\n");
        DEBUG("For each Proc make an IrProc\n");
        RESIZE_GLOBAL_BUFFER(procToIrProc, procCnt);
        for (Proc x = 0; x < procCnt; x++) {
                IrProc ip = irProcCnt++;
                procToIrProc[x] = ip;
                RESIZE_GLOBAL_BUFFER(irProcInfo, irProcCnt);
                irProcInfo[ip].symbol = procInfo[x].sym;
                irProcInfo[ip].firstIrStmt = 0;
                irProcInfo[ip].firstIrReg = 0;
        }

        DEBUG("For each local variable make an IrReg to hold it\n");
        RESIZE_GLOBAL_BUFFER(dataToIrReg, dataCnt);
        for (Data x = 0; x < dataCnt; x++)
                dataToIrReg[x] = (IrReg) -1;
        for (Data x = 0; x < dataCnt; x++) {
                Scope s = dataInfo[x].scope;
                if (scopeInfo[s].kind == SCOPE_PROC) {
                        Proc p = scopeInfo[s].tProc.proc;
                        IrProc irp = procToIrProc[p];
                        IrReg r = irRegCnt++;
                        dataToIrReg[x] = r;
                        RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
                        irRegInfo[r].proc = irp;
                        irRegInfo[r].name = symbolInfo[dataInfo[x].sym].name;
                        irRegInfo[r].sym = dataInfo[x].sym;
                        irRegInfo[r].tp = dataInfo[x].tp;
                }
        }

        DEBUG("For each expression make an IrReg to hold the result\n");
        RESIZE_GLOBAL_BUFFER(exprToIrReg, exprCnt);
        for (Expr x = 0; x < exprCnt; x++) {
                Proc p = exprInfo[x].proc;
                IrReg r = irRegCnt++;
                exprToIrReg[x] = r;
                RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
                irRegInfo[r].proc = procToIrProc[p];
                irRegInfo[r].name = intern_cstring("(tmp)") /*XXX*/;
                irRegInfo[r].sym = -1; // registers from Expr's are unnamed
                irRegInfo[r].tp = exprType[x]; // for now
        }

        DEBUG("For each proc add appropriate IrStmts\n");
        for (Proc p = 0; p < procCnt; p++) {
                DEBUG("Compile proc #%d %s\n", p, SS(procInfo[p].sym));
                IrProc irp = procToIrProc[p];
                irProcInfo[irp].symbol = procInfo[p].sym;
                compile_stmt(irp, procInfo[p].body);
        }
}
