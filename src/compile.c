/*
 * Compile IR from parsed syntax
 */

#include "defs.h"
#include "api.h"

enum {
        NOT_USED_AS_LVALUE = 0,
        USED_AS_LVALUE = 1,
};

INTERNAL
int find_struct_offset(Type tp, String memberName)
{
        if (typeInfo[tp].typeKind == TYPE_REFERENCE) //XXX
                tp = typeInfo[tp].tRef.resolvedTp;
        ASSERT(typeInfo[tp].typeKind == TYPE_STRUCT);
        for (Structmember m = typeInfo[tp].tStruct.firstStructmember;
             m < structmemberCnt && structmemberInfo[m].structTp == tp;
             m++) {
                if (structmemberInfo[m].memberName == memberName) {
                        return structmemberInfo[m].offset;
                }
        }
        UNREACHABLE();
}

INTERNAL
int is_local_variable(Expr e, Data *out)
{
        if (exprInfo[e].exprKind != EXPR_SYMREF)
                return 0;
        Symref ref = exprInfo[e].tSymref.ref;
        Symbol sym = symrefToSym[ref];
        if (symbolInfo[sym].symbolKind != SYMBOL_DATA)
                return 0;
        Scope scope = symbolInfo[sym].scope;
        if (scopeInfo[scope].scopeKind != SCOPE_PROC)
                return 0;
        Data data = symbolInfo[sym].tData.optionaldata;
        if (data == (Data) -1)
                return 0;
        *out = data;
        return 1;
}

INTERNAL
void compile_expr(Expr x, int usedAsLvalue);

INTERNAL
void compile_literal_expr(Expr x, UNUSED int usedAsLvalue)
{
        IrStmt y = irStmtCnt++;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        switch (exprInfo[x].tLiteral.literalKind) {
        case LITERAL_INTEGER: {
                Token tok = exprInfo[x].tLiteral.tok;
                long long constval = tokenInfo[tok].tInteger.value;
                irStmtInfo[y].proc = exprInfo[x].proc;
                irStmtInfo[y].irStmtKind = IRSTMT_LOADCONSTANT;
                irStmtInfo[y].tLoadConstant.irConstantKind = IRCONSTANT_INTEGER;
                irStmtInfo[y].tLoadConstant.tInteger = constval;
                irStmtInfo[y].tLoadConstant.tgtreg = exprToIrReg[x];
                break;
        }
        case LITERAL_STRING: {
                String s = exprInfo[x].tLiteral.tString;
                irStmtInfo[y].proc = exprInfo[x].proc;
                irStmtInfo[y].irStmtKind = IRSTMT_LOADCONSTANT;
                irStmtInfo[y].tLoadConstant.irConstantKind = IRCONSTANT_STRING;
                irStmtInfo[y].tLoadConstant.tString = s;
                irStmtInfo[y].tLoadConstant.tgtreg = exprToIrReg[x];
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL
void compile_unop_expr(Expr x, int usedAsLvalue)
{
        IrProc irp = procToIrProc[exprInfo[x].proc];
        Expr e1 = exprInfo[x].tUnop.expr;
        switch (exprInfo[x].tUnop.unopKind) {
        case UNOP_ADDRESSOF: {
                compile_expr(e1, USED_AS_LVALUE);
                exprToIrReg[x] = exprToIrReg[e1]; //XXX
                break;
        }
        case UNOP_DEREF: {
                compile_expr(e1, NOT_USED_AS_LVALUE);
                if (usedAsLvalue) {
                        exprToIrReg[x] = exprToIrReg[e1]; //XXX
                }
                else {
                        IrStmt y = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[y].proc = exprInfo[x].proc;
                        irStmtInfo[y].irStmtKind = IRSTMT_LOAD;
                        irStmtInfo[y].tLoad.srcaddrreg = exprToIrReg[e1];
                        irStmtInfo[y].tLoad.tgtreg = exprToIrReg[x];
                }
                break;
        }

        {
                int opkind;
                IrReg incReg;
                IrReg loadReg;
                IrReg tmpReg;
        case UNOP_PREINCREMENT:   tmpReg = irRegCnt++;  loadReg = tmpReg;  incReg = exprToIrReg[x];  opkind = IROP1_INC; goto doit;
        case UNOP_PREDECREMENT:   tmpReg = irRegCnt++;  loadReg = tmpReg;  incReg = exprToIrReg[x];  opkind = IROP1_DEC; goto doit;
        case UNOP_POSTINCREMENT:  tmpReg = irRegCnt++;  loadReg = exprToIrReg[x];  incReg = tmpReg;  opkind = IROP1_INC; goto doit;
        case UNOP_POSTDECREMENT:  tmpReg = irRegCnt++;  loadReg = exprToIrReg[x];  incReg = tmpReg;  opkind = IROP1_DEC; goto doit;
        doit:
                compile_expr(e1, USED_AS_LVALUE);
                IrStmt loadStmt = irStmtCnt++;
                IrStmt incStmt = irStmtCnt++;
                IrStmt storeStmt = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irRegInfo[tmpReg].proc = irp;
                irRegInfo[tmpReg].name = -1;
                irRegInfo[tmpReg].sym = -1;
                irRegInfo[tmpReg].tp = builtinType[BUILTINTYPE_INT];
                irStmtInfo[loadStmt].proc = irp;
                irStmtInfo[loadStmt].irStmtKind = IRSTMT_LOAD;
                irStmtInfo[loadStmt].tLoad.srcaddrreg = exprToIrReg[e1];
                irStmtInfo[loadStmt].tLoad.tgtreg = loadReg;
                irStmtInfo[incStmt].proc = irp;
                irStmtInfo[incStmt].irStmtKind = IRSTMT_OP1;
                irStmtInfo[incStmt].tOp1.irOp1Kind = opkind;
                irStmtInfo[incStmt].tOp1.reg = loadReg;
                irStmtInfo[incStmt].tOp1.tgtreg = incReg;
                irStmtInfo[storeStmt].proc = irp;
                irStmtInfo[storeStmt].irStmtKind = IRSTMT_STORE;
                irStmtInfo[storeStmt].tStore.srcreg = incReg;
                irStmtInfo[storeStmt].tStore.tgtaddrreg = exprToIrReg[e1];
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL
void compile_binop_expr(Expr x, UNUSED int usedAsLvalue)
{
        int binopKind = exprInfo[x].tBinop.binopKind;
        Expr e1 = exprInfo[x].tBinop.expr1;
        Expr e2 = exprInfo[x].tBinop.expr2;
        if (binopKind == BINOP_ASSIGN) {
                // Compile the right hand expression first. In general this is
                // the sane thing to do: Evaluate the assignment target only
                // after the sideeffects of the righthand side have been done.
                compile_expr(e2, NOT_USED_AS_LVALUE);

                Data data;
                if (is_local_variable(e1, &data)) {
                        /* optimization for simple identifier assignments */
                        IrStmt y = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[y].proc = exprInfo[x].proc;
                        irStmtInfo[y].irStmtKind = IRSTMT_REGREG;
                        irStmtInfo[y].tRegreg.srcreg = exprToIrReg[e2];
                        irStmtInfo[y].tRegreg.tgtreg = dataToIrReg[data];
                }
                else {
                        /* no optimization: more complex expression */
                        compile_expr(e1, USED_AS_LVALUE); // could optimize this for simple identifier assignments
                        IrStmt y = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[y].proc = exprInfo[x].proc;
                        irStmtInfo[y].irStmtKind = IRSTMT_STORE;
                        irStmtInfo[y].tStore.srcreg = exprToIrReg[e2];
                        irStmtInfo[y].tStore.tgtaddrreg = exprToIrReg[e1];
                }
                exprToIrReg[x] = exprToIrReg[e2]; //XXX
        }
        else if (binopKind == BINOP_PLUS ||
                 binopKind == BINOP_MINUS ||
                 binopKind == BINOP_MUL ||
                 binopKind == BINOP_DIV) {
                compile_expr(e1, NOT_USED_AS_LVALUE);
                compile_expr(e2, NOT_USED_AS_LVALUE);
                int irOp2Kind;
                switch (exprInfo[x].tBinop.binopKind) {
                case BINOP_PLUS:  irOp2Kind = IROP2_ADD; break;
                case BINOP_MINUS: irOp2Kind = IROP2_SUB; break;
                case BINOP_MUL:   irOp2Kind = IROP2_MUL; break;
                case BINOP_DIV:   irOp2Kind = IROP2_DIV; break;
                default: ASSERT(0);
                }
                IrStmt y = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[y].proc = exprInfo[x].proc;
                irStmtInfo[y].irStmtKind = IRSTMT_OP2;
                irStmtInfo[y].tOp2.irOp2Kind = irOp2Kind;
                irStmtInfo[y].tOp2.reg1 = exprToIrReg[e1];
                irStmtInfo[y].tOp2.reg2 = exprToIrReg[e2];
                irStmtInfo[y].tOp2.tgtreg = exprToIrReg[x];
        }
        else {
                compile_expr(e1, NOT_USED_AS_LVALUE);
                compile_expr(e2, NOT_USED_AS_LVALUE);
                int irCmpKind;
                switch (exprInfo[x].tBinop.binopKind) {
                case BINOP_LT: irCmpKind = IRCMP_LT; break;
                case BINOP_GT: irCmpKind = IRCMP_GT; break;
                case BINOP_LE: irCmpKind = IRCMP_LE; break;
                case BINOP_GE: irCmpKind = IRCMP_GE; break;
                case BINOP_EQ: irCmpKind = IRCMP_EQ; break;
                case BINOP_NE: irCmpKind = IRCMP_NE; break;
                default:
                        MSG(lvl_error, "Can't handle %s!\n",
                            binopInfo[exprInfo[x].tBinop.binopKind].str);
                        UNHANDLED_CASE();
                }
                IrStmt y = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[y].proc = exprInfo[x].proc;
                irStmtInfo[y].irStmtKind = IRSTMT_CMP;
                irStmtInfo[y].tCmp.irCmpKind = irCmpKind;
                irStmtInfo[y].tCmp.reg1 = exprToIrReg[e1];
                irStmtInfo[y].tCmp.reg2 = exprToIrReg[e2];
                irStmtInfo[y].tCmp.tgtreg = exprToIrReg[x];
        }
}

INTERNAL
void compile_member_expr(Expr x, int usedAsLvalue)
{
        Expr e = exprInfo[x].tMember.expr;
        String memberName = exprInfo[x].tMember.name;

        compile_expr(e, USED_AS_LVALUE);
        /* XXX: The offset should not be here! struct layout should be done
         * in the backend. */
        int offset = find_struct_offset(exprType[e], memberName);

        IrReg offsetReg = irRegCnt++;
        IrReg addrReg = irRegCnt++;
        IrStmt loadStmt = irStmtCnt++;
        IrStmt addStmt = irStmtCnt++;
        RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);

        irRegInfo[offsetReg].proc = procToIrProc[exprInfo[x].proc];
        irRegInfo[offsetReg].name = -1;
        irRegInfo[offsetReg].sym = -1;
        irRegInfo[offsetReg].tp = builtinType[BUILTINTYPE_INT];

        irRegInfo[addrReg].proc = procToIrProc[exprInfo[x].proc];
        irRegInfo[addrReg].name = -1;
        irRegInfo[addrReg].sym = -1;
        irRegInfo[addrReg].tp = irRegInfo[exprToIrReg[e]].tp;

        irStmtInfo[loadStmt].proc = exprInfo[x].proc;
        irStmtInfo[loadStmt].irStmtKind = IRSTMT_LOADCONSTANT;
        irStmtInfo[loadStmt].tLoadConstant.irConstantKind = IRCONSTANT_INTEGER;
        irStmtInfo[loadStmt].tLoadConstant.tInteger = offset;
        irStmtInfo[loadStmt].tLoadConstant.tgtreg = offsetReg;

        irStmtInfo[addStmt].proc = exprInfo[x].proc;
        irStmtInfo[addStmt].irStmtKind = IRSTMT_OP2;
        irStmtInfo[addStmt].tOp2.irOp2Kind = IROP2_ADD;
        irStmtInfo[addStmt].tOp2.reg1 = exprToIrReg[e];
        irStmtInfo[addStmt].tOp2.reg2 = offsetReg;
        irStmtInfo[addStmt].tOp2.tgtreg = addrReg;

        if (usedAsLvalue) {
                exprToIrReg[x] = addrReg;
        }
        else {
                IrStmt y = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[y].proc = exprInfo[x].proc;
                irStmtInfo[y].irStmtKind = IRSTMT_LOAD;
                irStmtInfo[y].tLoad.srcaddrreg = addrReg;
                irStmtInfo[y].tLoad.tgtreg = exprToIrReg[x];
        }
}

INTERNAL
void compile_symref_expr(Expr x, int usedAsLvalue)
{
        Symref ref = exprInfo[x].tSymref.ref;
        Symbol sym = symrefToSym[ref];
        ASSERT(sym >= 0);
        if (symbolInfo[sym].symbolKind == SYMBOL_DATA &&
            scopeInfo[symbolInfo[sym].scope].scopeKind == SCOPE_PROC) {
                Data data = symbolInfo[sym].tData.optionaldata;
                ASSERT(data != (Data) -1);  // proc-local data must exist
                if (! usedAsLvalue) {
                        IrStmt y = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[y].proc = exprInfo[x].proc;
                        irStmtInfo[y].irStmtKind = IRSTMT_REGREG;
                        irStmtInfo[y].tRegreg.srcreg = dataToIrReg[data];
                        irStmtInfo[y].tRegreg.tgtreg = exprToIrReg[x];
                }
                else {
                        IrStmt y = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[y].proc = exprInfo[x].proc;
                        irStmtInfo[y].irStmtKind = IRSTMT_LOADREGADDR;
                        irStmtInfo[y].tLoadRegAddr.reg = dataToIrReg[data];
                        irStmtInfo[y].tLoadRegAddr.tgtreg = exprToIrReg[x];
                }
        }
        else {
                IrStmt s0 = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[s0].proc = exprInfo[x].proc;
                irStmtInfo[s0].irStmtKind = IRSTMT_LOADSYMBOLADDR;
                irStmtInfo[s0].tLoadSymbolAddr.sym = sym;
                irStmtInfo[s0].tLoadSymbolAddr.tgtreg = exprToIrReg[x];
                if (! usedAsLvalue) {
                        IrStmt s1 = irStmtCnt++;
                        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                        irStmtInfo[s1].proc = exprInfo[x].proc;
                        irStmtInfo[s1].irStmtKind = IRSTMT_LOAD;
                        irStmtInfo[s1].tLoad.srcaddrreg = exprToIrReg[x];
                        irStmtInfo[s1].tLoad.tgtreg = exprToIrReg[x];
                }
        }
}

INTERNAL
void compile_call_expr(Expr x, UNUSED int usedAsLvalue)
{
        /* Evaluate function arguments */
        int firstCallArg = exprInfo[x].tCall.firstArgIdx;
        int lastCallArg = firstCallArg + exprInfo[x].tCall.nargs;
        for (int i = firstCallArg; i < lastCallArg; i++)
                compile_expr(callArgInfo[i].argExpr, NOT_USED_AS_LVALUE);

        /* Evaluate what function to call. TODO: should callees be pointers or
         * values? */
        Expr calleeExpr = exprInfo[x].tCall.callee;
        if (exprInfo[calleeExpr].exprKind == EXPR_SYMREF)
                compile_expr(calleeExpr, USED_AS_LVALUE);
        else
                compile_expr(calleeExpr, NOT_USED_AS_LVALUE);

        /* Do the call */
        IrStmt y = irStmtCnt++;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        irStmtInfo[y].proc = exprInfo[x].proc;
        irStmtInfo[y].irStmtKind = IRSTMT_CALL;
        irStmtInfo[y].tCall.calleeReg = exprToIrReg[calleeExpr];
        irStmtInfo[y].tCall.firstIrCallArg = irCallArgCnt; // XXX: Achtung!
        irStmtInfo[y].tCall.firstIrCallResult = irCallResultCnt; // XXX: Achtung!
        /* Plumbing of call args */
        for (int i = firstCallArg; i < lastCallArg; i++) {
                Expr argExpr = callArgInfo[i].argExpr;
                IrCallArg arg = irCallArgCnt++;
                RESIZE_GLOBAL_BUFFER(irCallArgInfo, irCallArgCnt);
                irCallArgInfo[arg].callStmt = y;
                irCallArgInfo[arg].srcreg = exprToIrReg[argExpr];
        }
        /* Plumbing of result values */
        IrCallResult ret = irCallResultCnt++;
        RESIZE_GLOBAL_BUFFER(irCallResultInfo, irCallResultCnt);
        irCallResultInfo[ret].callStmt = y;
        irCallResultInfo[ret].tgtreg = exprToIrReg[x];
}

INTERNAL
void compile_subscript_expr(Expr x, int usedAsLvalue)
{
        Expr e1 = exprInfo[x].tSubscript.expr1;
        Expr e2 = exprInfo[x].tSubscript.expr2;
        compile_expr(e1, NOT_USED_AS_LVALUE);
        compile_expr(e2, NOT_USED_AS_LVALUE);

        ASSERT(typeInfo[exprType[e1]].typeKind == TYPE_POINTER);
        int scale = get_type_size(typeInfo[exprType[e1]].tPointer.tp);

        /* offset pointer */
        IrReg scaleReg = irRegCnt++;
        IrReg offsetReg = irRegCnt++;
        IrReg addrReg = irRegCnt++;
        IrStmt loadStmt = irStmtCnt++;
        IrStmt mulStmt = irStmtCnt++;
        IrStmt addStmt = irStmtCnt++;

        RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);

        irRegInfo[scaleReg].proc = procToIrProc[exprInfo[x].proc];
        irRegInfo[scaleReg].name = -1;
        irRegInfo[scaleReg].sym = -1;
        irRegInfo[scaleReg].tp = builtinType[BUILTINTYPE_INT];

        irRegInfo[offsetReg].proc = procToIrProc[exprInfo[x].proc];
        irRegInfo[offsetReg].name = -1;
        irRegInfo[offsetReg].sym = -1;
        irRegInfo[offsetReg].tp = builtinType[BUILTINTYPE_INT];

        irRegInfo[addrReg].proc = procToIrProc[exprInfo[x].proc];
        irRegInfo[addrReg].name = -1;
        irRegInfo[addrReg].sym = -1;
        irRegInfo[addrReg].tp = builtinType[BUILTINTYPE_INT];

        irStmtInfo[loadStmt].proc = exprInfo[x].proc;
        irStmtInfo[loadStmt].irStmtKind = IRSTMT_LOADCONSTANT;
        irStmtInfo[loadStmt].tLoadConstant.irConstantKind = IRCONSTANT_INTEGER;
        irStmtInfo[loadStmt].tLoadConstant.tInteger = scale;
        irStmtInfo[loadStmt].tLoadConstant.tgtreg = scaleReg;

        irStmtInfo[mulStmt].proc = exprInfo[x].proc;
        irStmtInfo[mulStmt].irStmtKind = IRSTMT_OP2;
        irStmtInfo[mulStmt].tOp2.irOp2Kind = IROP2_MUL;
        irStmtInfo[mulStmt].tOp2.reg1 = scaleReg;
        irStmtInfo[mulStmt].tOp2.reg2 = exprToIrReg[e2];
        irStmtInfo[mulStmt].tOp2.tgtreg = offsetReg;

        irStmtInfo[addStmt].proc = exprInfo[x].proc;
        irStmtInfo[addStmt].irStmtKind = IRSTMT_OP2;
        irStmtInfo[addStmt].tOp2.irOp2Kind = IROP2_ADD;
        irStmtInfo[addStmt].tOp2.reg1 = exprToIrReg[e1];
        irStmtInfo[addStmt].tOp2.reg2 = offsetReg;
        irStmtInfo[addStmt].tOp2.tgtreg = addrReg;

        if (usedAsLvalue) {
                exprToIrReg[x] = addrReg;
        }
        else {
                IrStmt z = irStmtCnt++;
                RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
                irStmtInfo[z].proc = exprInfo[x].proc;
                irStmtInfo[z].irStmtKind = IRSTMT_LOAD;
                irStmtInfo[z].tLoad.srcaddrreg = addrReg;
                irStmtInfo[z].tLoad.tgtreg = exprToIrReg[x];
        }
}

INTERNAL
void compile_sizeof_expr(Expr x, int usedAsLvalue)
{
        /* Expression cannot be an lvalue. This condition should have been
         * caught during type checking. */
        ASSERT(! usedAsLvalue);  // caught during type checking

        Expr y = exprInfo[x].tSizeof.expr;
        Type tp = exprType[y];
        IrStmt irs = irStmtCnt++;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        irStmtInfo[irs].proc = exprInfo[x].proc;
        irStmtInfo[irs].irStmtKind = IRSTMT_LOADCONSTANT;
        irStmtInfo[irs].tLoadConstant.irConstantKind = IRCONSTANT_INTEGER;
        irStmtInfo[irs].tLoadConstant.tInteger = get_type_size(tp);
        irStmtInfo[irs].tLoadConstant.tgtreg = exprToIrReg[x];
}

INTERNAL
void (*const exprKindToCompileFunc[NUM_EXPR_KINDS])(Expr x, int usedAsLvalue) = {
#define MAKE(x, y) [x] = &y
        MAKE( EXPR_LITERAL,    compile_literal_expr   ),
        MAKE( EXPR_UNOP,       compile_unop_expr      ),
        MAKE( EXPR_BINOP,      compile_binop_expr     ),
        MAKE( EXPR_MEMBER,     compile_member_expr    ),
        MAKE( EXPR_SUBSCRIPT,  compile_subscript_expr ),
        MAKE( EXPR_SYMREF,     compile_symref_expr    ),
        MAKE( EXPR_CALL,       compile_call_expr      ),
        MAKE( EXPR_SIZEOF,     compile_sizeof_expr    ),
#undef MAKE
};

INTERNAL
void compile_expr(Expr x, int usedAsLvalue)
{
        if (usedAsLvalue &&
            !( exprInfo[x].exprKind == EXPR_SYMREF ) &&
            !( exprInfo[x].exprKind == EXPR_MEMBER ) &&
            !( exprInfo[x].exprKind == EXPR_SUBSCRIPT ) &&
            !( exprInfo[x].exprKind == EXPR_UNOP &&
               exprInfo[x].tUnop.unopKind == UNOP_DEREF)) {
                /* Expression cannot be an lvalue. This condition should have
                 * been caught during type checking. */
                DEBUG("Expression kind: %s\n", exprKindString[exprInfo[x].exprKind]);
                MSG_AT_EXPR(lvl_error, x, "This error should not happen!\n");
                UNREACHABLE();
        }

        /* Allocate IrReg for this expr */
        IrReg r = irRegCnt++;
        RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
        irRegInfo[r].proc = procToIrProc[exprInfo[x].proc]; // XXX
        irRegInfo[r].name = -1;  // register from Expr's are unnamed
        irRegInfo[r].sym = -1; // registers from Expr's are unnamed
        irRegInfo[r].tp = exprType[x];
        exprToIrReg[x] = r;

        int kind = exprInfo[x].exprKind;
        ASSERT(0 <= kind && kind < NUM_EXPR_KINDS);
        exprKindToCompileFunc [kind] (x, usedAsLvalue);
}

INTERNAL
void compile_stmt(IrProc irp, Stmt stmt);

INTERNAL
void compile_data_stmt(IrProc irp, Stmt stmt)
{
        (void) irp;
        (void) stmt;
        /* We have allocated a register in compile_to_IR() */
}

INTERNAL
void compile_array_stmt(IrProc irp, Stmt stmt)
{
        (void) irp;
        (void) stmt;
        UNHANDLED_CASE();
}

INTERNAL
void compile_macro_stmt(IrProc irp, Stmt stmt)
{
        // nothing to compile right now
        (void) irp;
        (void) stmt;
}

INTERNAL
void compile_expr_stmt(IrProc irp, Stmt stmt)
{
        (void) irp;
        compile_expr(stmtInfo[stmt].tExpr.expr, NOT_USED_AS_LVALUE);
}

INTERNAL
void compile_compound_stmt(IrProc irp, Stmt stmt)
{
        Stmt first = stmtInfo[stmt].tCompound.firstChildStmtIdx;
        Stmt last = first + stmtInfo[stmt].tCompound.numStatements;
        for (int cld = first; cld < last; cld++)
                compile_stmt(irp, childStmtInfo[cld].child);
}

INTERNAL
void compile_if_stmt(IrProc irp, Stmt stmt)
{
        Expr condExpr = stmtInfo[stmt].tIf.condExpr;
        Stmt ifbody = stmtInfo[stmt].tIf.ifbody;
        compile_expr(condExpr, NOT_USED_AS_LVALUE);
        IrStmt irs = irStmtCnt++;
        compile_stmt(irp, ifbody);
        IrStmt stmtAfterBlock = irStmtCnt;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        irStmtInfo[irs].proc = irp;
        irStmtInfo[irs].irStmtKind = IRSTMT_CONDGOTO;
        irStmtInfo[irs].tCondGoto.condreg = exprToIrReg[condExpr];
        irStmtInfo[irs].tCondGoto.tgtstmt = stmtAfterBlock;
        irStmtInfo[irs].tCondGoto.isNeg = 1;
}

INTERNAL
void compile_ifelse_stmt(IrProc irp, Stmt stmt)
{
        Expr condExpr = stmtInfo[stmt].tIfelse.condExpr;
        Stmt ifbody = stmtInfo[stmt].tIfelse.ifbody;
        Stmt elsebody = stmtInfo[stmt].tIfelse.elsebody;
        compile_expr(condExpr, NOT_USED_AS_LVALUE);
        IrStmt j0 = irStmtCnt++;
        compile_stmt(irp, ifbody);
        IrStmt j1 = irStmtCnt++;
        IrStmt firstinelse = irStmtCnt;
        compile_stmt(irp, elsebody);
        IrStmt firstafterelse = irStmtCnt;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        irStmtInfo[j0].proc = irp;
        irStmtInfo[j0].irStmtKind = IRSTMT_CONDGOTO;
        irStmtInfo[j0].tCondGoto.condreg = exprToIrReg[condExpr];
        irStmtInfo[j0].tCondGoto.tgtstmt = firstinelse;
        irStmtInfo[j0].tCondGoto.isNeg = 1;
        irStmtInfo[j1].proc = irp;
        irStmtInfo[j1].irStmtKind = IRSTMT_GOTO;
        irStmtInfo[j1].tGoto.tgtstmt = firstafterelse;
}

INTERNAL
void compile_while_stmt(IrProc irp, Stmt stmt)
{
        Expr condExpr = stmtInfo[stmt].tWhile.condExpr;
        Stmt whilebody = stmtInfo[stmt].tWhile.whilebody;
        IrStmt condStmt = irStmtCnt;
        compile_expr(condExpr, NOT_USED_AS_LVALUE);
        IrStmt irs = irStmtCnt++;
        compile_stmt(irp, whilebody);
        IrStmt backJmp = irStmtCnt++;
        IrStmt stmtAfterBlock = irStmtCnt;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        irStmtInfo[irs].proc = irp;
        irStmtInfo[irs].irStmtKind = IRSTMT_CONDGOTO;
        irStmtInfo[irs].tCondGoto.condreg = exprToIrReg[condExpr];
        irStmtInfo[irs].tCondGoto.tgtstmt = stmtAfterBlock;
        irStmtInfo[irs].tCondGoto.isNeg = 1;
        irStmtInfo[backJmp].proc = irp;
        irStmtInfo[backJmp].irStmtKind = IRSTMT_GOTO;
        irStmtInfo[backJmp].tGoto.tgtstmt = condStmt;
}

INTERNAL
void compile_for_stmt(IrProc irp, Stmt stmt)
{
        Stmt initStmt = stmtInfo[stmt].tFor.initStmt;
        Expr condExpr = stmtInfo[stmt].tFor.condExpr;
        Stmt stepStmt = stmtInfo[stmt].tFor.stepStmt;
        Stmt forbody = stmtInfo[stmt].tFor.forbody;
        compile_stmt(irp, initStmt);
        IrStmt cond = irStmtCnt;
        compile_expr(condExpr, NOT_USED_AS_LVALUE);
        IrStmt condJmp = irStmtCnt++;
        compile_stmt(irp, forbody);
        compile_stmt(irp, stepStmt);
        IrStmt backJmp = irStmtCnt++;
        IrStmt stmtAfterBlock = irStmtCnt;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        irStmtInfo[condJmp].proc = irp;
        irStmtInfo[condJmp].irStmtKind = IRSTMT_CONDGOTO;
        irStmtInfo[condJmp].tCondGoto.condreg = exprToIrReg[condExpr];
        irStmtInfo[condJmp].tCondGoto.tgtstmt = stmtAfterBlock;
        irStmtInfo[condJmp].tCondGoto.isNeg = 1;
        irStmtInfo[backJmp].proc = irp;
        irStmtInfo[backJmp].irStmtKind = IRSTMT_GOTO;
        irStmtInfo[backJmp].tGoto.tgtstmt = cond;
}

INTERNAL
void compile_range_stmt(IrProc irp, Stmt stmt)
{
        Data variable = stmtInfo[stmt].tRange.variable;
        Expr e1 = stmtInfo[stmt].tRange.startExpr;
        Expr e2 = stmtInfo[stmt].tRange.stopExpr;
        Stmt rangebody = stmtInfo[stmt].tRange.rangebody;

        /* XXX Override: place start value directly in the IrReg of the
         * iteration variable, instead of placing it in the automatically
         * generated IrReg first. This must come before compile_expr(e1,...) */
        exprToIrReg[e1] = dataToIrReg[variable];

        compile_expr(e1, NOT_USED_AS_LVALUE);
        compile_expr(e2, NOT_USED_AS_LVALUE);

        IrReg varReg = dataToIrReg[variable];
        IrReg stopvalueReg = exprToIrReg[e2];
        IrReg cmpReg = irRegCnt++;

        IrStmt checkStmt = irStmtCnt++;
        IrStmt breakStmt = irStmtCnt++;
        compile_stmt(irp, rangebody);
        IrStmt stepStmt = irStmtCnt++;
        IrStmt jumpStmt = irStmtCnt++;
        IrStmt stmtAfterBlock = irStmtCnt;

        RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        irRegInfo[cmpReg].proc = irp;
        irRegInfo[cmpReg].name = -1;
        irRegInfo[cmpReg].sym = -1;
        irRegInfo[cmpReg].tp = builtinType[BUILTINTYPE_INT];
        irStmtInfo[checkStmt].proc = irp;
        irStmtInfo[checkStmt].irStmtKind = IRSTMT_CMP;
        irStmtInfo[checkStmt].tCmp.irCmpKind = IRCMP_GE;
        irStmtInfo[checkStmt].tCmp.reg1 = varReg;
        irStmtInfo[checkStmt].tCmp.reg2 = stopvalueReg;
        irStmtInfo[checkStmt].tCmp.tgtreg = cmpReg;
        irStmtInfo[breakStmt].proc = irp;
        irStmtInfo[breakStmt].irStmtKind = IRSTMT_CONDGOTO;
        irStmtInfo[breakStmt].tCondGoto.condreg = cmpReg;
        irStmtInfo[breakStmt].tCondGoto.tgtstmt = stmtAfterBlock;
        irStmtInfo[breakStmt].tCondGoto.isNeg = 0;
        irStmtInfo[stepStmt].proc = irp;
        irStmtInfo[stepStmt].irStmtKind = IRSTMT_OP1;
        irStmtInfo[stepStmt].tOp1.irOp1Kind = IROP1_INC;
        irStmtInfo[stepStmt].tOp1.reg = varReg;
        irStmtInfo[stepStmt].tOp1.tgtreg = varReg;
        irStmtInfo[jumpStmt].proc = irp;
        irStmtInfo[jumpStmt].irStmtKind = IRSTMT_GOTO;
        irStmtInfo[jumpStmt].tGoto.tgtstmt = checkStmt;
}

INTERNAL
void compile_return_stmt(IrProc irp, Stmt stmt)
{
        Expr resultExpr = stmtInfo[stmt].tReturn.expr;
        compile_expr(resultExpr, NOT_USED_AS_LVALUE);
        IrStmt irs = irStmtCnt++;
        IrReturnval ret = irReturnvalCnt++;
        RESIZE_GLOBAL_BUFFER(irStmtInfo, irStmtCnt);
        RESIZE_GLOBAL_BUFFER(irReturnvalInfo, irReturnvalCnt);
        irStmtInfo[irs].proc = irp;
        irStmtInfo[irs].irStmtKind = IRSTMT_RETURN;
        irStmtInfo[irs].tReturn.firstResult = ret;
        irReturnvalInfo[ret].returnStmt = irs;
        irReturnvalInfo[ret].resultReg = exprToIrReg[resultExpr];
}

INTERNAL
void (*const stmtKindToCompileFunc[NUM_STMT_KINDS])(IrProc irp, Stmt stmt) = {
#define MAKE(x, y) [x] = &y
        MAKE( STMT_DATA,      compile_data_stmt     ),
        MAKE( STMT_ARRAY,     compile_array_stmt    ),
        MAKE( STMT_MACRO,     compile_macro_stmt    ),
        MAKE( STMT_EXPR,      compile_expr_stmt     ),
        MAKE( STMT_COMPOUND,  compile_compound_stmt ),
        MAKE( STMT_IF,        compile_if_stmt       ),
        MAKE( STMT_IFELSE,    compile_ifelse_stmt   ),
        MAKE( STMT_WHILE,     compile_while_stmt    ),
        MAKE( STMT_FOR,       compile_for_stmt      ),
        MAKE( STMT_RANGE,     compile_range_stmt    ),
        MAKE( STMT_RETURN,    compile_return_stmt   ),
#undef MAKE
};

INTERNAL
void compile_stmt(IrProc irp, Stmt stmt)
{
        int kind = stmtInfo[stmt].stmtKind;
        ASSERT(0 <= kind && kind < NUM_STMT_KINDS);
        ASSERT(stmtKindToCompileFunc[kind] != 0);
        stmtKindToCompileFunc [kind] (irp, stmt);
}

INTERNAL
void compile_proc(Proc p)
{
        /* make ir registers for all of the proc's local data declarations */
        for (Data d = firstDataOfProc[p]; d < dataCnt; d++) {
                Scope s = dataInfo[d].scope;
                if (scopeInfo[s].scopeKind != SCOPE_PROC)
                        break;
                if (scopeInfo[s].tProc != p)
                        break;
                IrProc irp = procToIrProc[p];
                IrReg r = irRegCnt++;
                dataToIrReg[d] = r;
                RESIZE_GLOBAL_BUFFER(irRegInfo, irRegCnt);
                irRegInfo[r].proc = irp;
                irRegInfo[r].name = symbolInfo[dataInfo[d].sym].name;
                irRegInfo[r].sym = dataInfo[d].sym;
                irRegInfo[r].tp = dataInfo[d].tp;
        }

        DEBUG("Compile proc #%d %s\n", p, SS(procInfo[p].sym));
        IrProc irp = procToIrProc[p];
        irProcInfo[irp].symbol = procInfo[p].sym;
        compile_stmt(irp, procInfo[p].body);
}

void compile_to_IR(void)
{
        RESIZE_GLOBAL_BUFFER(procToIrProc, procCnt);
        RESIZE_GLOBAL_BUFFER(firstDataOfProc, procCnt);
        //RESIZE_GLOBAL_BUFFER(firstExprOfProc, procCnt);
        RESIZE_GLOBAL_BUFFER(exprToIrReg, exprCnt);
                for (Expr x = 0; x < exprCnt; x++)
                        exprToIrReg[x] = -1; // really necessary? at least for debugging
        RESIZE_GLOBAL_BUFFER(dataToIrReg, dataCnt);

        DEBUG("For each local variable start without associated IrReg\n");
        for (Data x = 0; x < dataCnt; x++)
                dataToIrReg[x] = (IrReg) -1;

        DEBUG("For each expression find its proc.\n");
        DEBUG("(TODO: think about adding this data already when parsing)?\n");
        DEBUG("For each Proc make an IrProc\n");
        for (Proc p = 0; p < procCnt; p++) {
                IrProc ip = irProcCnt++;
                procToIrProc[p] = ip;
                RESIZE_GLOBAL_BUFFER(irProcInfo, irProcCnt);
                irProcInfo[ip].symbol = procInfo[p].sym;
                irProcInfo[ip].firstIrStmt = 0;
                irProcInfo[ip].firstIrReg = 0;
        }

        DEBUG("For each proc find its first local variable\n");
        for (Proc p = 0; p < procCnt; p++)
                firstDataOfProc[p] = dataCnt;
        for (Data d = dataCnt; d --> 0;) {
                Scope s = dataInfo[d].scope;
                if (scopeInfo[s].scopeKind == SCOPE_PROC)
                        firstDataOfProc[scopeInfo[s].tProc] = d;
        }
        for (Data d = 0; d < dataCnt; d++) {
                Scope s = dataInfo[d].scope;
                if (scopeInfo[s].scopeKind == SCOPE_PROC) {
                        DEBUG("data=%d its proc=%d its first data=%d\n",
                              d, scopeInfo[s].tProc,
                              firstDataOfProc[scopeInfo[s].tProc]);
                }
        }
        DEBUG("OK\n");

        DEBUG("For each proc add appropriate IrStmts\n");
        for (Proc p = 0; p < procCnt; p++) {
                DEBUG("compile_proc(%d)\n", p);
                compile_proc(p);
        }
        DEBUG("OK\n");
        for (IrStmt i = irStmtCnt; i --> 0;)
                irProcInfo[irStmtInfo[i].proc].firstIrStmt = i;

        for (Data d = 0; d < dataCnt; d++)
                DEBUG("data=%d irReg=%d\n", d, dataToIrReg[d]);

        DEBUG("Look for jump targets and sources and sort them by target)\n");
        for (IrStmt stmt = 0; stmt < irStmtCnt; stmt++) {
                Stmt tgtstmt;
                switch (irStmtInfo[stmt].irStmtKind) {
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
