#include "defs.h"
#include "api.h"

INTERNAL
void irp_constant(const struct IrLoadConstantStmtInfo *c)
{
        switch (c->irConstantKind) {
        case IRCONSTANT_INTEGER: {
                outf("%lld", (long long) c->tInteger);
                break;
        }
        case IRCONSTANT_STRING: {
                const char *s = string_buffer(c->tString);
                outc('"');
                for (int i = 0; s[i] != '\0'; i++) {
                        unsigned char c = s[i];
                        if (32 <= c && c < 128)
                                outc(c);
                        else if (c == '\n')
                                outs("\\n");
                        else
                                outf("\\x%.2x", c);
                }
                outc('"');
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL
void irp_symbol(Symbol sym)
{
        outf("@%s", SS(sym));
}

INTERNAL
void irp_reg(IrReg v)
{
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
                if (irRegInfo[reg].sym >= 0)
                        outf(" (%s)", SS(irRegInfo[reg].sym));
                outf(" tp=%d", irRegInfo[reg].tp);
                outs("\n");
        }
        for (IrStmt i = irProcInfo[p].firstIrStmt;
             i < irStmtCnt && irStmtInfo[i].proc == p;
             i++) {
                outf("%5d   ", i);
                switch (irStmtInfo[i].kind) {
                case IRSTMT_LOADCONSTANT:
                        outs("LDC   ");
                        irp_constant(&irStmtInfo[i].tLoadConstant);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tLoadConstant.tgtreg);
                        break;
                case IRSTMT_LOADSYMBOLADDR:
                        outs("LDA   ");
                        irp_symbol(irStmtInfo[i].tLoadSymbolAddr.sym);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tLoadSymbolAddr.tgtreg);
                        break;
                case IRSTMT_LOADREGADDR:
                        outs("LDR   ");
                        irp_reg(irStmtInfo[i].tLoadRegAddr.reg);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tLoadRegAddr.tgtreg);
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
                case IRSTMT_REGREG:
                        outs("MOV   ");
                        irp_reg(irStmtInfo[i].tRegreg.srcreg);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tRegreg.tgtreg);
                        break;
                case IRSTMT_OP1:
                        outs(irOp1String[irStmtInfo[i].tOp1.irOp1Kind]);
                        outs(" ");
                        irp_reg(irStmtInfo[i].tOp1.reg);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tOp1.tgtreg);
                        break;
                case IRSTMT_OP2:
                        outs(irOp2String[irStmtInfo[i].tOp2.irOp2Kind]);
                        outs(" (");
                        irp_reg(irStmtInfo[i].tOp2.reg1);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tOp2.reg2);
                        outs("), ");
                        irp_reg(irStmtInfo[i].tOp2.tgtreg);
                        break;
                case IRSTMT_CMP:
                        outs(irCmpString[irStmtInfo[i].tCmp.kind]);
                        outs(" (");
                        irp_reg(irStmtInfo[i].tCmp.reg1);
                        outs(", ");
                        irp_reg(irStmtInfo[i].tCmp.reg2);
                        outs("), ");
                        irp_reg(irStmtInfo[i].tCmp.tgtreg);
                        break;
                case IRSTMT_CALL: {
                        outs("CALL  ");
                        irp_reg(irStmtInfo[i].tCall.calleeReg);
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
                                irp_reg(irReturnvalInfo[res].resultReg);
                                res++;
                                if (res == irReturnvalCnt)
                                        break;
                                if (irReturnvalInfo[res].returnStmt != i)
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
        MSG(lvl_info, "Fixing indices\n");
        for (IrStmt i = irStmtCnt; i --> 0;) {
                IrProc irp = irStmtInfo[i].proc;
                irProcInfo[irp].firstIrStmt = i;
        }
        for (IrReg i = irRegCnt; i --> 0;) {
                IrProc irp = irRegInfo[i].proc;
                irProcInfo[irp].firstIrReg = i;
        }
        MSG(lvl_info, "Printing procs\n");
        outs("\n");
        for (IrProc p = 0; p < irProcCnt; p++) {
                irp_proc(p);
        }
}
