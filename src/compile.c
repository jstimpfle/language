/*
 * Compile IR from parsed syntax
 */

#include "defs.h"
#include "api.h"

INTERNAL
void compile_expr(Expr x);

INTERNAL
void compile_literal_expr(Expr x)
{
        IrStmt y = irStmtCnt++;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        switch (exprInfo[x].tLiteral.kind) {
        case LITERAL_INTEGER: {
                Token tok = exprInfo[x].tLiteral.tok;
                long long constval = tokenInfo[tok].tInteger.value;
                irStmtInfo[y].proc = exprInfo[x].proc;
                irStmtInfo[y].kind = IRSTMT_LOADCONSTANT;
                irStmtInfo[y].tLoadConstant.kind = IRCONSTANT_INTEGER;
                irStmtInfo[y].tLoadConstant.tInteger = constval;
                irStmtInfo[y].tLoadConstant.tgtreg = exprToIrReg[x];
                break;
        }
        case LITERAL_STRING: {
                String s = exprInfo[x].tLiteral.tString;
                irStmtInfo[y].proc = exprInfo[x].proc;
                irStmtInfo[y].kind = IRSTMT_LOADCONSTANT;
                irStmtInfo[y].tLoadConstant.kind = IRCONSTANT_STRING;
                irStmtInfo[y].tLoadConstant.tString = s;
                irStmtInfo[y].tLoadConstant.tgtreg = exprToIrReg[x];
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL
void compile_unop_expr(Expr x)
{
        Expr e1 = exprInfo[x].tUnop.expr;
        switch (exprInfo[x].tUnop.kind) {
        case UNOP_ADDRESSOF: {
                compile_expr(e1);
                exprToIrReg[x] = exprToIrReg[e1]; //XXX
                break;
        }
        case UNOP_DEREF: {
                compile_expr(e1);
                if (!isExprEvaluated[x]) {
                        exprToIrReg[x] = exprToIrReg[e1]; //XXX
                }
                else {
                        IrStmt y = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[y].proc = exprInfo[x].proc;
                        irStmtInfo[y].kind = IRSTMT_LOAD;
                        irStmtInfo[y].tLoad.srcaddrreg = exprToIrReg[e1];
                        irStmtInfo[y].tLoad.tgtreg = exprToIrReg[x];
                }
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL
void compile_binop_expr(Expr x)
{
        int binopKind = exprInfo[x].tBinop.kind;
        Expr e1 = exprInfo[x].tBinop.expr1;
        Expr e2 = exprInfo[x].tBinop.expr2;
        if (binopKind == BINOP_ASSIGN) {
                compile_expr(e1); // could optimize this for simple identifier assignments
                compile_expr(e2);
                IrStmt y = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[y].proc = exprInfo[x].proc;
                irStmtInfo[y].kind = IRSTMT_STORE;
                irStmtInfo[y].tStore.srcreg = exprToIrReg[e2];
                irStmtInfo[y].tStore.tgtaddrreg = exprToIrReg[e1];
                exprToIrReg[x] = exprToIrReg[e2]; //XXX
        }
        else if (binopKind == BINOP_PLUS ||
                 binopKind == BINOP_MINUS ||
                 binopKind == BINOP_MUL ||
                 binopKind == BINOP_DIV) {
                compile_expr(e1);
                compile_expr(e2);
                int kind;
                switch (exprInfo[x].tBinop.kind) {
                case BINOP_PLUS:  kind = IROP2_ADD; break;
                case BINOP_MINUS: kind = IROP2_SUB; break;
                case BINOP_MUL:   kind = IROP2_MUL; break;
                case BINOP_DIV:   kind = IROP2_DIV; break;
                default: ASSERT(0);
                }
                IrStmt y = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[y].proc = exprInfo[x].proc;
                irStmtInfo[y].kind = IRSTMT_OP2;
                irStmtInfo[y].tOp2.kind = kind;
                irStmtInfo[y].tOp2.reg1 = exprToIrReg[e1];
                irStmtInfo[y].tOp2.reg2 = exprToIrReg[e2];
                irStmtInfo[y].tOp2.tgtreg = exprToIrReg[x];
        }
        else {
                compile_expr(e1);
                compile_expr(e2);
                int kind;
                switch (exprInfo[x].tBinop.kind) {
                case BINOP_LT: kind = IRCMP_LT; break;
                case BINOP_GT: kind = IRCMP_GT; break;
                case BINOP_LE: kind = IRCMP_LE; break;
                case BINOP_GE: kind = IRCMP_GE; break;
                case BINOP_EQ: kind = IRCMP_EQ; break;
                case BINOP_NE: kind = IRCMP_NE; break;
                default:
                        MSG(lvl_error, "Can't handle %s!\n",
                            binopInfo[exprInfo[x].tBinop.kind].str);
                        UNHANDLED_CASE();
                }
                IrStmt y = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[y].proc = exprInfo[x].proc;
                irStmtInfo[y].kind = IRSTMT_CMP;
                irStmtInfo[y].tCmp.kind = kind;
                irStmtInfo[y].tCmp.reg1 = exprToIrReg[e1];
                irStmtInfo[y].tCmp.reg2 = exprToIrReg[e2];
                irStmtInfo[y].tCmp.tgtreg = exprToIrReg[x];
        }
}

INTERNAL
void compile_symref_expr(Expr x)
{
        Symref ref = exprInfo[x].tSymref.ref;
        Symbol sym = symrefToSym[ref];
        ASSERT(sym >= 0);
        if (symbolInfo[sym].kind == SYMBOL_DATA &&
            scopeInfo[symbolInfo[sym].scope].kind == SCOPE_PROC) {
                Data data = symbolInfo[sym].tData.optionaldata;
                ASSERT(data != (Data) -1);  // proc-local data must exist
                if (isExprEvaluated[x]) {
                        IrStmt y = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[y].proc = exprInfo[x].proc;
                        irStmtInfo[y].kind = IRSTMT_REGREG;
                        irStmtInfo[y].tRegreg.srcreg = dataToIrReg[data];
                        irStmtInfo[y].tRegreg.tgtreg = exprToIrReg[x];
                }
                else {
                        IrStmt y = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[y].proc = exprInfo[x].proc;
                        irStmtInfo[y].kind = IRSTMT_LOADREGADDR;
                        irStmtInfo[y].tLoadRegAddr.reg = dataToIrReg[data];
                        irStmtInfo[y].tLoadRegAddr.tgtreg = exprToIrReg[x];
                }
        }
        else {
                IrStmt s0 = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[s0].proc = exprInfo[x].proc;
                irStmtInfo[s0].kind = IRSTMT_LOADSYMBOLADDR;
                irStmtInfo[s0].tLoadSymbolAddr.sym = sym;
                irStmtInfo[s0].tLoadSymbolAddr.tgtreg = exprToIrReg[x];
                if (isExprEvaluated[x]) {
                        IrStmt s1 = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[s1].proc = exprInfo[x].proc;
                        irStmtInfo[s1].kind = IRSTMT_LOAD;
                        irStmtInfo[s1].tLoad.srcaddrreg = exprToIrReg[x];
                        irStmtInfo[s1].tLoad.tgtreg = exprToIrReg[x];
                }
        }
}

INTERNAL
void compile_call_expr(Expr x)
{
        /*
         * Evaluate function arguments
         */
        int firstCallArg = exprInfo[x].tCall.firstArgIdx;
        int lastCallArg = firstCallArg + exprInfo[x].tCall.nargs;
        for (int i = firstCallArg; i < lastCallArg; i++)
                compile_expr(callArgInfo[i].argExpr);
        /*
         * Evaluate function to call. The usual case is also a special
         * case: If we call an EXPR_SYMREF, that symbol's value is not
         * loaded, but only its address.
         */
        Expr calleeExpr = exprInfo[x].tCall.callee;
        if (exprInfo[calleeExpr].kind == EXPR_SYMREF) {
                Symref ref = exprInfo[calleeExpr].tSymref.ref;
                Symbol sym = symrefToSym[ref];
                ASSERT(sym != (Symbol) -1);
                IrStmt s0 = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[s0].proc = exprInfo[x].proc;
                irStmtInfo[s0].kind = IRSTMT_LOADSYMBOLADDR;
                irStmtInfo[s0].tLoadSymbolAddr.sym = sym;
                irStmtInfo[s0].tLoadSymbolAddr.tgtreg = exprToIrReg[calleeExpr];
        }
        else {
                compile_expr(calleeExpr);
        }
        /*
         * Emit calling code
         */
        IrStmt y = irStmtCnt++;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        irStmtInfo[y].proc = exprInfo[x].proc;
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
}

INTERNAL
void compile_subscript_expr(Expr x)
{
        Expr e1 = exprInfo[x].tSubscript.expr1;
        Expr e2 = exprInfo[x].tSubscript.expr2;
        compile_expr(e1);
        compile_expr(e2);
        /* offset pointer */
        IrStmt y = irStmtCnt++;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        irStmtInfo[y].proc = exprInfo[x].proc;
        irStmtInfo[y].kind = IRSTMT_OP2;
        irStmtInfo[y].tOp2.kind = IROP2_ADD;
        irStmtInfo[y].tOp2.reg1 = exprToIrReg[e1];
        irStmtInfo[y].tOp2.reg2 = exprToIrReg[e2];
        irStmtInfo[y].tOp2.tgtreg = exprToIrReg[x];
        if (isExprEvaluated[x]) {
                IrStmt z = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[z].proc = exprInfo[x].proc;
                irStmtInfo[z].kind = IRSTMT_LOAD;
                irStmtInfo[z].tLoad.srcaddrreg = exprToIrReg[x];
                irStmtInfo[z].tLoad.tgtreg = exprToIrReg[x];
        }
}

INTERNAL
void compile_expr(Expr x)
{
        if (! isExprEvaluated[x] &&
            !( exprInfo[x].kind == EXPR_SYMREF ) &&
            !( exprInfo[x].kind == EXPR_MEMBER ) &&
            !( exprInfo[x].kind == EXPR_SUBSCRIPT ) &&
            !( exprInfo[x].kind == EXPR_UNOP &&
               exprInfo[x].tUnop.kind == UNOP_DEREF)) {
                /* Expression cannot be an lvalue. This condition should have
                 * been caught during type checking. */
                UNREACHABLE();
        }
        switch (exprInfo[x].kind) {
        case EXPR_LITERAL:    compile_literal_expr(x); break;
        case EXPR_UNOP:       compile_unop_expr(x); break;
        case EXPR_BINOP:      compile_binop_expr(x); break;
        case EXPR_SYMREF:     compile_symref_expr(x); break;
        case EXPR_CALL:       compile_call_expr(x); break;
        case EXPR_SUBSCRIPT:  compile_subscript_expr(x); break;
        default: UNHANDLED_CASE();
        }
}

INTERNAL
void compile_stmt(IrProc irp, Stmt stmt)
{
        switch (stmtInfo[stmt].kind) {
        case STMT_DATA: {
                /* We have allocated a register in compile_to_IR() */
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
                Stmt ifbody = stmtInfo[stmt].tIf.ifbody;
                compile_expr(condExpr);
                IrStmt irs = irStmtCnt++;
                compile_stmt(irp, ifbody);
                IrStmt stmtAfterBlock = irStmtCnt;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[irs].proc = irp;
                irStmtInfo[irs].kind = IRSTMT_CONDGOTO;
                irStmtInfo[irs].tCondGoto.condreg = exprToIrReg[condExpr];
                irStmtInfo[irs].tCondGoto.tgtstmt = stmtAfterBlock;
                irStmtInfo[irs].tCondGoto.isNeg = 1;
                break;
        }
        case STMT_IFELSE: {
                Expr condExpr = stmtInfo[stmt].tIfelse.condExpr;
                Stmt ifbody = stmtInfo[stmt].tIfelse.ifbody;
                Stmt elsebody = stmtInfo[stmt].tIfelse.elsebody;
                compile_expr(condExpr);
                IrStmt j0 = irStmtCnt++;
                compile_stmt(irp, ifbody);
                IrStmt j1 = irStmtCnt++;
                IrStmt firstinelse = irStmtCnt;
                compile_stmt(irp, elsebody);
                IrStmt firstafterelse = irStmtCnt;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[j0].proc = irp;
                irStmtInfo[j0].kind = IRSTMT_CONDGOTO;
                irStmtInfo[j0].tCondGoto.condreg = exprToIrReg[condExpr];
                irStmtInfo[j0].tCondGoto.tgtstmt = firstinelse;
                irStmtInfo[j0].tCondGoto.isNeg = 1;
                irStmtInfo[j1].proc = irp;
                irStmtInfo[j1].kind = IRSTMT_GOTO;
                irStmtInfo[j1].tGoto.tgtstmt = firstafterelse;
                break;
        }
        case STMT_WHILE: {
                Expr condExpr = stmtInfo[stmt].tWhile.condExpr;
                Stmt whilebody = stmtInfo[stmt].tWhile.whilebody;
                IrStmt condStmt = irStmtCnt;
                compile_expr(condExpr);
                IrStmt irs = irStmtCnt++;
                compile_stmt(irp, whilebody);
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
                irStmtInfo[backJmp].tGoto.tgtstmt = condStmt;
                break;
        }
        case STMT_RETURN: {
                Expr resultExpr = stmtInfo[stmt].tReturn.expr;
                compile_expr(resultExpr);
                IrStmt irs = irStmtCnt++;
                IrReturnval ret = irReturnvalCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                RESIZE_GLOBAL_BUFFER(irReturnvalInfo, irReturnvalCnt);
                irStmtInfo[irs].proc = irp;
                irStmtInfo[irs].kind = IRSTMT_RETURN;
                irStmtInfo[irs].tReturn.firstResult = ret;
                irReturnvalInfo[ret].returnStmt = irs;
                irReturnvalInfo[ret].resultReg = exprToIrReg[resultExpr];
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

void compile_to_IR(void)
{
        DEBUG("For each expression find its proc.\n");
        DEBUG("(TODO: think about adding this data already when parsing)?\n");
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
                irRegInfo[r].tp = exprType[x];
        }

        DEBUG("For each proc add appropriate IrStmts\n");
        for (Proc p = 0; p < procCnt; p++) {
                DEBUG("Compile proc #%d %s\n", p, SS(procInfo[p].sym));
                IrProc irp = procToIrProc[p];
                irProcInfo[irp].symbol = procInfo[p].sym;
                compile_stmt(irp, procInfo[p].body);
        }
        for (IrStmt i = irStmtCnt; i --> 0;)
                irProcInfo[irStmtInfo[i].proc].firstIrStmt = i;

        DEBUG("Look for jump targets and sources and sort them by target)\n");
        for (IrStmt stmt = 0; stmt < irStmtCnt; stmt++) {
                Stmt tgtstmt;
                switch (irStmtInfo[stmt].kind) {
                case IRSTMT_CONDGOTO:
                        tgtstmt = irStmtInfo[stmt].tCondGoto.tgtstmt;
                        break;
                case IRSTMT_GOTO:
                        tgtstmt = irStmtInfo[stmt].tGoto.tgtstmt;
                        break;
                default:
                        continue;
                }
                int x = irOriginCnt++;
                RESIZE_GLOBAL_BUFFER(irOrigin, irOriginCnt);
                irOrigin[x].stmt = tgtstmt;
                irOrigin[x].originStmt = stmt;
                break;
        }
}
