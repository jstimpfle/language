#include "defs.h"
#include "api.h"

void irp_constant(long long x)
{
        outf("%lld", x);
}

void irp_symbol(Symbol sym)
{
        outf("@%s", SS(sym));
}

void irp_reg(IrReg v)
{
        if (irRegInfo[v].sym >= 0)
                outf("%s", SS(irRegInfo[v].sym));
        else
                outf("%%%d", v);
}

void irp_proc(IrProc p)
{
        outf("PROC #%d \"%s\":\n", p, string_buffer(irProcInfo[p].name));
        for (IrStmt i = irProcInfo[p].firstIrStmt;
             i < irStmtCnt && irStmtInfo[i].proc == p;
             i++) {
                outf("%5d   ", i);
                switch (irStmtInfo[i].kind) {
                case IRSTMT_LOADCONSTANT:
                        outs("LOADCONSTANT ");
                        irp_constant(irStmtInfo[i].tLoadConstant.constval);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tLoadConstant.tgtreg);
                        break;
                case IRSTMT_LOADSYMBOLADDR:
                        outs("LOADSYMBOLADDR ");
                        irp_symbol(irStmtInfo[i].tLoadSymbolAddr.sym);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tLoadSymbolAddr.tgtreg);
                        break;
                case IRSTMT_LOAD:
                        outs("LOAD ");
                        irp_reg(irStmtInfo[i].tLoad.srcaddrreg);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tLoad.tgtreg);
                        break;
                case IRSTMT_STORE:
                        outs("STORE ");
                        irp_reg(irStmtInfo[i].tStore.srcreg);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tStore.tgtaddrreg);
                        break;
                case IRSTMT_CALL: {
                        outs("CALL ");
                        irp_reg(irStmtInfo[i].tCall.callee);
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
                        outs(" -> ");
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
                        outs("CONDGOTO ");
                        irp_reg(irStmtInfo[i].tCondGoto.condreg);
                        outs(", ");
                        outf("%d", irStmtInfo[i].tCondGoto.tgtstmt);
                        break;
                }
                case IRSTMT_GOTO: {
                        outs("GOTO ");
                        outf("%d", irStmtInfo[i].tGoto.tgtstmt);
                        break;
                }
                case IRSTMT_RETURN: {
                        outs("RETURN ");
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
        for (IrStmt i = irStmtCnt; i-- > 0;) {
                Proc proc = irStmtInfo[i].proc;
                irProcInfo[proc].firstIrStmt = i;
        }
        DEBUG("Printing procs\n");
        outs("\n");
        for (IrProc p = 0; p < irProcCnt; p++)
                irp_proc(p);
}
