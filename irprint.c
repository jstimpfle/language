#include "defs.h"
#include "api.h"

INTERNAL
void irp_constant(long long x)
{
        outf("%lld", x);
}

INTERNAL
void irp_symbol(Symbol sym)
{
        outf("@%s", SS(sym));
}

INTERNAL
void irp_reg(IrReg v)
{
        if (irRegInfo[v].sym >= 0)
                outf("%s", SS(irRegInfo[v].sym));
        else
                outf("%%%d", v);
}

INTERNAL
void irp_proc(IrProc p)
{
        outf("PROC #%d \"%s\":\n", p, SS(irProcInfo[p].symbol));
        for (IrReg reg = irProcInfo[p].firstIrReg;
             reg < irRegCnt && irRegInfo[reg].proc == p;
             reg++) {
                outs("  Reg:  ");
                irp_reg(reg);
                outs("\n");
        }
        for (IrStmt i = irProcInfo[p].firstIrStmt;
             i < irStmtCnt && irStmtInfo[i].proc == p;
             i++) {
                outf("%5d   ", i);
                switch (irStmtInfo[i].kind) {
                case IRSTMT_LOADCONSTANT:
                        outs("LDC   ");
                        irp_constant(irStmtInfo[i].tLoadConstant.constval);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tLoadConstant.tgtreg);
                        break;
                case IRSTMT_LOADSYMBOLADDR:
                        outs("LDA   ");
                        irp_symbol(irStmtInfo[i].tLoadSymbolAddr.sym);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tLoadSymbolAddr.tgtreg);
                        break;
                case IRSTMT_LOAD:
                        outs("LD    ");
                        irp_reg(irStmtInfo[i].tLoad.srcaddrreg);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tLoad.tgtreg);
                        break;
                case IRSTMT_STORE:
                        outs("STR   ");
                        irp_reg(irStmtInfo[i].tStore.srcreg);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tStore.tgtaddrreg);
                        break;
                case IRSTMT_CALL: {
                        outs("CALL  ");
                        irp_reg(irStmtInfo[i].tCall.callee);
                        outs(", ");
                        outs("(");
                        int arg = irStmtInfo[i].tCall.firstIrCallArg;
                        for (;;) {
                                irp_reg(irCallArgInfo[arg].srcreg);
                                arg++;
                                if (arg == irCallArgCnt)
                                        break;
                                if (irCallArgInfo[arg].callStmt != i)
                                        break;
                                outs(", ");
                        }
                        outs(")");
                        outs(", ");
                        outs("(");
                        int ret = irStmtInfo[i].tCall.firstIrCallResult;
                        for (;;) {
                                /*DEBUG("\nret=%d tgtreg=%d, irCallResultCnt=%d\n",
                                        ret, irCallResultInfo[ret].tgtreg, irCallResultCnt);
                                        */
                                irp_reg(irCallResultInfo[ret].tgtreg);
                                ret++;
                                if (ret == irCallResultCnt)
                                        break;
                                if (irCallResultInfo[ret].callStmt != i)
                                        break;
                                outs(", ");
                        }
                        outs(")");
                        break;
                }
                case IRSTMT_CONDGOTO: {
                        if (irStmtInfo[i].tCondGoto.isNeg)
                                outs("JNE   ");
                        else
                                outs("JE    ");
                        irp_reg(irStmtInfo[i].tCondGoto.condreg);
                        outs(", ");
                        outf("%d", irStmtInfo[i].tCondGoto.tgtstmt);
                        break;
                }
                case IRSTMT_GOTO: {
                        outs("JMP   ");
                        outf("%d", irStmtInfo[i].tGoto.tgtstmt);
                        break;
                }
                case IRSTMT_RETURN: {
                        outs("RET   ");
                        outs("(");
                        int res = irStmtInfo[i].tReturn.firstResult;
                        for (;;) {
                                irp_reg(irReturnResultInfo[res].resultReg);
                                res++;
                                if (res == irReturnResultCnt)
                                        break;
                                if (irReturnResultInfo[res].returnStmt != i)
                                        break;
                                outs(", ");
                        }
                        outs(")");
                        break;
                }
                default:
                        UNHANDLED_CASE();
                }
                outs("\n");
        }
        outs("\n");
}

void irprint(void)
{
        outs("\n");
        DEBUG("Fixing indices\n");
        for (IrStmt i = irStmtCnt; i --> 0;) {
                IrProc irp = irStmtInfo[i].proc;
                irProcInfo[irp].firstIrStmt = i;
        }
        for (IrReg i = irRegCnt; i --> 0;) {
                IrProc irp = irRegInfo[i].proc;
                irProcInfo[irp].firstIrReg = i;
        }
        DEBUG("Printing procs\n");
        outs("\n");
        for (IrProc p = 0; p < irProcCnt; p++) {
                irp_proc(p);
        }
}
