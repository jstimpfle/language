#include "defs.h"
#include "api.h"
#include "ir.h"

struct IrVarInfo irvars[] = {
#define MAKE(n) { -1, n, -1 }
        MAKE("x"),
        MAKE("y"),
        MAKE("z"),
        MAKE("a"),
        MAKE("b"),
        MAKE("c"),
        MAKE("f"),
#undef MAKE
};

struct IrStmtInfo irs[] = {
#define MAKELOADCONSTANT(x,y)   { -1, IRSTMT_LOADCONSTANT, .tLoadConstant = {(x),(y)}}
#define MAKELOADSYMBOLADDR(x,y) { -1, IRSTMT_LOADSYMBOLADDR, .tLoadSymbolAddr = {(x),(y)}}
#define MAKELOAD(x, y)          { -1, IRSTMT_LOAD, .tLoad = {(x),(y)}}
#define MAKESTORE(x, y)         { -1, IRSTMT_STORE, .tStore = {(x),(y)}}
#define MAKECALL(x)             { -1, IRSTMT_CALL, .tCall = {(x),-1,-1}}
#define MAKECONDGOTO(x, y)      { -1, IRSTMT_CONDGOTO, .tCondGoto = {(x),(y)}}
#define MAKEGOTO(x)             { -1, IRSTMT_GOTO, .tGoto = {(x)}}
        MAKELOADCONSTANT(42, 0),
        MAKELOADCONSTANT(42, 1),
        MAKELOADSYMBOLADDR( (Symbol) 3, (IrVar) 1 ),
        MAKELOADSYMBOLADDR( (Symbol) 4, (IrVar) 2 ),
        MAKELOADSYMBOLADDR( (Symbol) 1, (IrVar) 6 ),
        MAKECALL( 6 ),
        MAKECONDGOTO( (IrVar) 0, 13 ),
        MAKEGOTO( 14 ),
        MAKESTORE( (IrVar) 2, (IrVar) 3 ),
};
#undef MAKELOADCONSTANT
#undef MAKELOADSYMBOLADDR
#undef MAKELOAD
#undef MAKESTORE
#undef MAKECALL
#undef MAKECONDGOTO
#undef MAKEGOTO

void irp_constant(long long x)
{
        outf("%lld", x);
}

void irp_symbol(Symbol sym)
{
        outf("@%s", SS(sym));
}

void irp_var(IrVar v)
{
        outf("%s", irvars[v].name);
}

void irprint(void)
{
        outs("\n");
        for (IrStmt i = 0; i < LENGTH(irs); i++) {
                switch (irs[i].kind) {
                case IRSTMT_LOADCONSTANT:
                        outs("LOADCONSTANT ");
                        irp_constant(irs[i].tLoadConstant.constval);
                        outs(", ");
                        irp_var(irs[i].tLoadConstant.tgtvar);
                        break;
                case IRSTMT_LOADSYMBOLADDR:
                        outs("LOADSYMBOLADDR ");
                        irp_symbol(irs[i].tLoadSymbolAddr.sym);
                        outs(", ");
                        irp_var(irs[i].tLoadSymbolAddr.tgtvar);
                        break;
                case IRSTMT_LOAD:
                        outs("LOAD ");
                        irp_var(irs[i].tLoad.srcaddrvar);
                        outs(", ");
                        irp_var(irs[i].tLoad.tgtvar);
                        break;
                case IRSTMT_STORE:
                        outs("STORE ");
                        irp_var(irs[i].tStore.srcvar);
                        outs(", ");
                        irp_var(irs[i].tStore.tgtaddrvar);
                        break;
                case IRSTMT_CALL:
                        outs("CALL ");
                        irp_var(irs[i].tCall.callee);
                        outs("(...)");
                        break;
                case IRSTMT_CONDGOTO:
                        outs("CONDGOTO ");
                        irp_var(irs[i].tCondGoto.condvar);
                        outs(", ");
                        outf("%d", irs[i].tCondGoto.tgtstmt);
                        break;
                case IRSTMT_GOTO:
                        outs("GOTO ");
                        outf("%d", irs[i].tGoto.tgtstmt);
                        break;
                }
                outs("\n");
        }
        outs("\n");
}
