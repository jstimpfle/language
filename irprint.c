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
#undef MAKE
};

struct IrStmtInfo irs[] = {
#define MAKELOADCONSTANT(x,y)   { IRSTMT_LOADCONSTANT, -1, .tLoadConstant = {(x),(y)}}
#define MAKELOADSYMBOLADDR(x,y) { IRSTMT_LOADSYMBOLADDR, -1, .tLoadSymbolAddr = {(x),(y)}}
#define MAKELOAD(x, y)          { IRSTMT_LOAD, -1, .tLoad = {(x),(y)}}
#define MAKESTORE(x, y)         { IRSTMT_STORE, -1, .tStore = {(x),(y)}}
#define MAKECALL(x)             { IRSTMT_CALL, -1, .tCall = {(x),-1,-1}}
#define MAKECONDGOTO(x, y)      { IRSTMT_CONDGOTO, -1, .tCondGoto = {(x),-1,-1}}
#define MAKEGOTO(x)             { IRSTMT_GOTO, -1, .tGoto = {(x)}}
        MAKELOADCONSTANT(42, 0),
        MAKELOADCONSTANT(42, 1),
        MAKELOADSYMBOLADDR( (Symbol) 3, (IrVar) 2 ),
        MAKELOADSYMBOLADDR( (Symbol) 4, (IrVar) 2 ),
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
                        outf("LOADSYMBOLADDR ");
                        irp_symbol(irs[i].tLoadSymbolAddr.sym);
                        outs(", ");
                        irp_var(irs[i].tLoadConstant.tgtvar);
                        break;
                }
                outs("\n");
        }
        outs("\n");
}
