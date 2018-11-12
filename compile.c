#include "defs.h"
#include "api.h"

INTERNAL
Symbol find_symbol_in_scope(String name, Scope scope)
{
        //DEBUG("RESOLVE %s\n", string_buffer(name));
        for (; scope != -1; scope = scopeInfo[scope].parentScope) {
                Symbol first = scopeInfo[scope].firstSymbol;
                Symbol last = first + scopeInfo[scope].numSymbols;
                for (Symbol i = first; i < last; i++) {
                        if (symbolInfo[i].name == name) {
                                //DEBUG("FOUND symbol %s\n", string_buffer(name));
                                return i;
                        }
                }
        }
        return -1;
}

void resolve_symbol_references(void)
{
        for (Symref ref = 0; ref < symrefCnt; ref++) {
                String name = symrefInfo[ref].name;
                Scope refScope = symrefInfo[ref].refScope;
                Symbol sym = find_symbol_in_scope(name, refScope);
                if (sym < 0) {
                        MSG_AT_TOK("ERROR", symrefInfo[ref].tok,
                                   "unresolved symbol reference %s\n",
                                   string_buffer(name));
                }
                symrefInfo[ref].sym = sym;
        }
}

INTERNAL
void resolve_ref_type(Type t)
{
        if (typeInfo[t].isComplete >= 0) {
                /* already processed */
                return;
        }
        if (typeInfo[t].isComplete == -1) {
                MSG(lvl_warn, "Type #%d: cyclic type reference\n", t);
                typeInfo[t].isComplete = 0;
                return;
        }

        const int unassigned = -42;

        int isComplete = unassigned;

        switch (typeInfo[t].kind) {
        case TYPE_BASE:
                isComplete = 1;
                break;
        case TYPE_ENTITY:
                resolve_ref_type(typeInfo[t].tEntity.tp);
                isComplete = typeInfo[typeInfo[t].tEntity.tp].isComplete;
                break;
        case TYPE_ARRAY:
                resolve_ref_type(typeInfo[t].tArray.idxtp);
                resolve_ref_type(typeInfo[t].tArray.valuetp);
                isComplete =
                        typeInfo[typeInfo[t].tArray.idxtp].isComplete &&
                        typeInfo[typeInfo[t].tArray.valuetp].isComplete;
                break;
        case TYPE_POINTER:
                resolve_ref_type(typeInfo[t].tPointer.tp);
                isComplete = typeInfo[typeInfo[t].tPointer.tp].isComplete;
                break;
        case TYPE_PROC:
                isComplete = 0;
                // TODO
                break;
        case TYPE_REFERENCE: {
                isComplete = 0;
                typeInfo[t].isComplete = -1;
                Symbol sym = symrefInfo[typeInfo[t].tRef.ref].sym;
                if (sym != -1 && symbolInfo[sym].kind == SYMBOL_TYPE) {
                        Type symtp = symbolInfo[sym].tType;
                        if (symtp != -1) {
                                resolve_ref_type(symtp);
                                isComplete = typeInfo[symtp].isComplete;
                        }
                        typeInfo[t].tRef.resolvedTp = symtp;
                }
                break;
        }
        default:
                UNHANDLED_CASE();
        }
        assert(isComplete != unassigned);
        typeInfo[t].isComplete = isComplete;
}

void resolve_type_references(void)
{
        /* isComplete -2 means "TO DO" */
        /* isComplete -1 means "currently resolving" */
        for (Type t = 0; t < typeCnt; t++)
                typeInfo[t].isComplete = -2;
        for (Type t = 0; t < typeCnt; t++)
                if (typeInfo[t].kind == TYPE_REFERENCE)
                        typeInfo[t].tRef.resolvedTp = -1;
        for (Type t = 0; t < typeCnt; t++)
                if (typeInfo[t].isComplete == -2)
                        resolve_ref_type(t);
        for (Type t = 0; t < typeCnt; t++) {
                assert(typeInfo[t].isComplete == 1 ||
                       typeInfo[t].isComplete == 0);
        }
}

INTERNAL
int is_integral_type(Type t)
{
        return typeInfo[t].kind == TYPE_BASE; //XXX
}

INTERNAL
int is_bad_type(Type t)
{
        assert(t >= 0);  // is this really true?
        return ! typeInfo[t].isComplete;
}

INTERNAL
int type_equal(Type a, Type b)
{
        if (!typeInfo[a].isComplete)
                return 0;
        if (!typeInfo[b].isComplete)
                return 0;
        while (typeInfo[a].kind == TYPE_REFERENCE) {
                a = typeInfo[a].tRef.resolvedTp;
                assert(a != -1);
        }
        while (typeInfo[b].kind == TYPE_REFERENCE) {
                b = typeInfo[b].tRef.resolvedTp;
                assert(a != -1);
        }
        return a == b;
}

INTERNAL
Type check_literal_expr_type(Expr x)
{
        //XXX
        Type tp = 0;
        exprInfo[x].tp = tp;
        return tp;
}

INTERNAL
Type check_symref_expr_type(Expr x)
{
        Symref ref = exprInfo[x].tSymref.ref;
        // XXX: symbol resolved?
        Symbol sym = symrefInfo[ref].sym;
        Type tp = -1;
        if (sym == -1) {
                const char *name = string_buffer(symrefInfo[ref].name);
                LOG_TYPE_ERROR_EXPR(x,
                      "Can't check type: symbol \"%s\" unresolved\n", name);
                goto out;
        }
        switch (symbolInfo[sym].kind) {
                case SYMBOL_TYPE:
                        // Maybe something like the "type" type?
                        // Or fatal() ?
                        UNHANDLED_CASE();
                        break;
                case SYMBOL_DATA:
                        tp = dataInfo[symbolInfo[sym].tData].tp;
                        break;
                case SYMBOL_ARRAY:
                        tp = arrayInfo[symbolInfo[sym].tArray].tp;
                        break;
                case SYMBOL_PROC:
                        tp = procInfo[symbolInfo[sym].tProc].tp;
                        break;
                case SYMBOL_PARAM:
                        tp = paramInfo[symbolInfo[sym].tParam].tp;
                        break;
                default:
                        UNHANDLED_CASE();
                        break;
        }
out:
        exprInfo[x].tp = tp;
        return tp;
}

INTERNAL
Type check_expr_type(Expr x);

INTERNAL
Type check_unop_expr_type(Expr x)
{
        int op = exprInfo[x].tUnop.kind;
        Expr xx = exprInfo[x].tUnop.expr;
        Type tt = check_expr_type(xx);
        Type tp = -1;
        if (tt != -1) {
                switch (op) {
                case UNOP_INVERTBITS:
                case UNOP_NOT:
                case UNOP_ADDRESSOF:
                case UNOP_DEREF:
                case UNOP_NEGATIVE:
                case UNOP_POSITIVE:
                case UNOP_PREDECREMENT:
                case UNOP_PREINCREMENT:
                case UNOP_POSTDECREMENT:
                case UNOP_POSTINCREMENT:
                        if (is_integral_type(tt))
                                tp = tt;
                        break;
                default:
                        UNHANDLED_CASE();
                }
        }
        exprInfo[x].tp = tp;
        return tp;
}

INTERNAL
Type check_binop_expr_type(Expr x)
{
        int op = exprInfo[x].tUnop.kind;
        Expr x1 = exprInfo[x].tBinop.expr1;
        Expr x2 = exprInfo[x].tBinop.expr2;
        Type t1 = check_expr_type(x1);
        Type t2 = check_expr_type(x2);
        Type tp = -1;
        if (t1 != -1 && t2 != -1) {
                switch (op) {
                case BINOP_ASSIGN:
                case BINOP_EQUALS:
                case BINOP_MINUS:
                case BINOP_PLUS:
                case BINOP_MUL:
                case BINOP_DIV:
                case BINOP_BITAND:
                case BINOP_BITOR:
                case BINOP_BITXOR:
                        if (is_integral_type(t1) && is_integral_type(t2)
                            && t1 == t2)
                                tp = t1;
                        break;
                default:
                        UNHANDLED_CASE();
                }
        }
        exprInfo[x].tp = tp;
        return tp;
}

INTERNAL
Type check_member_expr_type(Expr x)
{
        /*
        Expr xx = exprInfo[x].tMember.expr;
        String name = exprInfo[x].tMember.name;
        Type tt = exprInfo[xx].tMember.expr;
        */
        // TODO: lookup member and infer type
        Type tp = -1;
        exprInfo[x].tp = tp;
        return tp;
}

INTERNAL
Type check_subscript_expr_type(Expr x)
{
        Expr x1 = exprInfo[x].tSubscript.expr1;
        Expr x2 = exprInfo[x].tSubscript.expr2;
        Type t1 = check_expr_type(x1);
        Type t2 = check_expr_type(x2);
        Type tp = -1;

        if (is_bad_type(t1) || is_bad_type(t2)) {
                LOG_TYPE_ERROR_EXPR(x,
                        "Cannot typecheck subscript expression: "
                        "bad array or index type\n");
        }
        else if (typeInfo[t1].kind != TYPE_ARRAY)
                LOG_TYPE_ERROR_EXPR(x,
                            "Subscript expression invalid: "
                            "indexed object is not array type\n");
        else if (! type_equal(t2, typeInfo[t1].tArray.idxtp))
                LOG_TYPE_ERROR_EXPR(x,
                            "Incompatible type of index "
                            "in subscript expression\n");
        else
                tp = typeInfo[t1].tArray.valuetp;
        /*
        DEBUG("t1=%d, t2=%d, idxtp=%d, valuetp=%d, tp=%d\n",
            t1, t2, typeInfo[t1].tArray.idxtp, typeInfo[t1].tArray.valuetp, tp);
            */
        exprInfo[x].tp = tp;
        return tp;
}

INTERNAL
Type check_call_expr_type(Expr x)
{
        //XXX total mess and incomplete and wrong
        Expr callee = exprInfo[x].tCall.callee;
        Type calleeTp = check_expr_type(callee);
        if (calleeTp == -1)
                return -1;
        int calleeTpKind = typeInfo[calleeTp].kind;
        if (calleeTpKind != TYPE_PROC)
                LOG_TYPE_ERROR_EXPR(callee,
                    "Called expression: Expected proc type but found %s\n",
                    typeKindString[calleeTpKind]);
        int first = exprInfo[x].tCall.firstArgIdx;
        int last = first + exprInfo[x].tCall.nargs;
        for (int i = first; i < last; i++) {
                Expr argx = callArgInfo[i].argExpr;
                check_expr_type(argx);
                // TODO: check that argument type matches param of called proc
        }
        return -1;
}

INTERNAL
Type check_expr_type(Expr x)
{
        Type tp = -1;
        switch (exprInfo[x].kind) {
        case EXPR_LITERAL:      tp = check_literal_expr_type(x); break;
        case EXPR_SYMREF:       tp = check_symref_expr_type(x); break;
        case EXPR_UNOP:         tp = check_unop_expr_type(x); break;
        case EXPR_BINOP:        tp = check_binop_expr_type(x); break;
        case EXPR_MEMBER:       tp = check_member_expr_type(x); break;
        case EXPR_SUBSCRIPT:    tp = check_subscript_expr_type(x); break;
        case EXPR_CALL:         tp = check_call_expr_type(x); break;
        default:                UNHANDLED_CASE();
        }
        exprInfo[x].tp = tp;
        return tp;
}

void check_types(void)
{
        for (Expr x = 0; x < exprCnt; x++)
                check_expr_type(x);
        for (Expr x = 0; x < exprCnt; x++) {
                if (exprInfo[x].tp == -1)
                        LOG_TYPE_ERROR_EXPR(
                                x, "Type check of expression failed\n");
        }
}

void compile_expr(Expr x)
{
        switch (exprInfo[x].kind) {
        case EXPR_LITERAL: {
                IrStmt y = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[y].proc = exprToProc[x];
                irStmtInfo[y].kind = IRSTMT_LOADCONSTANT;
                irStmtInfo[y].tLoadConstant.constval = tokenInfo[exprInfo[x].tLiteral.tok].tInteger.value; //XXX
                irStmtInfo[y].tLoadConstant.tgtreg = exprToIrReg[x];
                break;
        }
        case EXPR_BINOP: {
                Expr e1 = exprInfo[x].tBinop.expr1;
                Expr e2 = exprInfo[x].tBinop.expr2;
                compile_expr(e1);
                compile_expr(e2);
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
                irStmtInfo[y].proc = exprToProc[x];
                // XXX: We "call" the binop operation. This is very inefficient.
                irStmtInfo[y].kind = IRSTMT_CALL;
                irStmtInfo[y].tCall.callee = 0;  //XXX TODO: need register holding address of binop operation
                irStmtInfo[y].tCall.firstIrCallArg = arg1;
                irStmtInfo[y].tCall.firstIrCallResult = ret;
                break;
        }
        case EXPR_SYMREF: {
                Symref ref = exprInfo[x].tSymref.ref;
                Symbol sym = symrefInfo[ref].sym;
                assert(sym >= 0);
                IrReg addr = irRegCnt++;
                RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
                IrStmt s0 = irStmtCnt++;
                IrStmt s1 = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irRegInfo[addr].proc = exprToProc[x];
                irRegInfo[addr].name = intern_cstring("(addr)"); //XXX
                irRegInfo[addr].sym = sym; //XXX
                irRegInfo[addr].tp = -1; //XXX
                irStmtInfo[s0].proc = exprToProc[x];
                irStmtInfo[s0].kind = IRSTMT_LOADSYMBOLADDR;
                irStmtInfo[s0].tLoadSymbolAddr.sym = sym;
                irStmtInfo[s0].tLoadSymbolAddr.tgtreg = addr;
                irStmtInfo[s1].proc = exprToProc[x];
                irStmtInfo[s1].kind = IRSTMT_LOAD;
                irStmtInfo[s1].tLoad.srcaddrreg = addr;
                irStmtInfo[s1].tLoad.tgtreg = exprToIrReg[x];
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
                irStmtInfo[y].proc = exprToProc[x];
                irStmtInfo[y].kind = IRSTMT_CALL;
                irStmtInfo[y].tCall.callee = exprToIrReg[calleeExpr];
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
                // UNHANDLED_CASE();
                break;
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
        RESIZE_GLOBAL_BUFFER(exprToProc, exprCnt);
        for (Expr x = 0; x < exprCnt; x++) {
                exprToProc[x] = (Proc)0;  //XXX
        }

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
        for (Data x = 0; x < dataCnt; x++) {
                Scope s = dataInfo[x].scope;
                if (scopeInfo[s].kind == SCOPE_PROC) {
                        Proc p = scopeInfo[s].tProc.proc;
                        IrProc irp = procToIrProc[p];
                        IrReg r = irRegCnt++;
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
                Proc p = exprToProc[x];
                IrReg r = irRegCnt++;
                exprToIrReg[x] = r;
                RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
                irRegInfo[r].proc = procToIrProc[p];
                irRegInfo[r].name = intern_cstring("(tmp)") /*XXX*/;
                irRegInfo[r].sym = -1; // registers from Expr's are unnamed
                irRegInfo[r].tp = exprInfo[x].tp; // for now
        }

        DEBUG("For each proc add appropriate IrStmts\n");
        for (Proc p = 0; p < procCnt; p++) {
                DEBUG("Compile proc #%d %s\n", p, SS(procInfo[p].sym));
                IrProc irp = procToIrProc[p];
                irProcInfo[irp].symbol = procInfo[p].sym;
                compile_stmt(irp, procInfo[p].body);
        }
}
