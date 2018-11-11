/*
 * First try at register allocator and x64 assembler
 */

#include "defs.h"
#include "api.h"

typedef unsigned char uchar;
typedef int X64StackLoc;
typedef long long Constval;

const char *x64regNames[NUM_X64REGS] = {
#define MAKE(x) [x] = #x
        MAKE( X64REG_RAX ),
        MAKE( X64REG_RCX ),
        MAKE( X64REG_RDX ),
        MAKE( X64REG_RBX ),
        MAKE( X64REG_RSP ),
        MAKE( X64REG_RBP ),
        MAKE( X64REG_RSI ),
        MAKE( X64REG_RDI ),
        MAKE( X64REG_R8 ),
        MAKE( X64REG_R9 ),
        MAKE( X64REG_R10 ),
        MAKE( X64REG_R11 ),
        MAKE( X64REG_R12 ),
        MAKE( X64REG_R13 ),
        MAKE( X64REG_R14 ),
        MAKE( X64REG_R15 ),
#undef MAKE
};

INTERNAL
int find_stack_loc(IrReg irreg)
{
        /* for now, just allocate registers globally, on the stack :-) */
        return irreg * 8;
}

INTERNAL
void emit_data(uchar *buf, int len)
{
}

INTERNAL
void emit_code(uchar *buf, int len)
{
        RESIZE_GLOBAL_BUFFER(codeSection, codeSectionCnt + len);
        mem_copy(codeSection + codeSectionCnt, buf, len);
        codeSectionCnt += len;
}

#define BYTE(x, n) (((x) >> (n)) & 255)
#define EMIT(...) do { \
        uchar bytes[] = { __VA_ARGS__ }; \
        emit_code(bytes, sizeof bytes); \
} while (0)

INTERNAL
void emit_mov_64_imm_reg(Constval imm, int x64reg)
{
        EMIT(
                0x48, 0x8b, (x64reg<<3) | 0x04,
                /* up to 3 bytes imm */
                BYTE(imm, 1), BYTE(imm, 2), BYTE(imm, 3)
        );
}

INTERNAL
void emit_mov_64_imm_indirect(Constval imm, int x64reg)
{
        EMIT(
                0x48, 0x89
        );
}

INTERNAL
void emit_mov_64_reg_reg(int r1, int r2)
{
        assert(r1 < 8);
        assert(r2 < 8);
        EMIT(
                0x48, 0x89,
                0xc0 | (r1 << 3) | r2
        );
}

INTERNAL
void emit_mov_64_reg_stack(int x64reg, X64StackLoc loc)
{
        assert(loc < 256);
        EMIT(
                0x48, 0x89, 0x44 | (x64reg << 3),
                0x24, loc
        );
}

INTERNAL
void emit_load_constant_stack(Constval constval, X64StackLoc loc)
{
        emit_mov_64_imm_reg(constval, X64REG_RAX);
        emit_mov_64_reg_stack(X64REG_RAX, loc);
}

INTERNAL
void emit_load_symaddr_stack(Constval constval, X64StackLoc loc)
{
}

INTERNAL
void emit_load_addrval_stack(Constval constval, X64StackLoc loc)
{
}

INTERNAL
void x64asm_proc(IrProc irp)
{
        /* for each variable, find a place on the stack */
        for (IrStmt irs = irProcInfo[irp].firstIrStmt;
             irs < irStmtCnt && irStmtInfo[irs].proc == irp;
             irs++) {
                switch (irStmtInfo[irs].kind) {
                case IRSTMT_LOADCONSTANT: {
                        Constval v = irStmtInfo[irs].tLoadConstant.constval;
                        IrReg irreg = irStmtInfo[irs].tLoadConstant.tgtreg;
                        X64StackLoc loc = find_stack_loc(irreg);
                        emit_load_constant_stack(v, loc);
			break;
                }
                case IRSTMT_LOADSYMBOLADDR: {
                        Symbol sym = irStmtInfo[irs].tLoadSymbolAddr.sym;
                        IrReg irreg = irStmtInfo[irs].tLoadSymbolAddr.tgtreg;
                        X64StackLoc loc = find_stack_loc(irreg);
                        emit_load_symaddr_stack(sym, loc);
			break;
                }
                case IRSTMT_LOAD: {
                        IrReg addrreg = irStmtInfo[irs].tLoad.srcaddrreg;
                        IrReg tgtreg = irStmtInfo[irs].tLoad.tgtreg;
                        X64StackLoc loc = find_stack_loc(tgtreg);
                        emit_load_addrval_stack(addrreg, loc);
			break;
                }
                case IRSTMT_STORE: {
                        IrReg addrreg = irStmtInfo[irs].tStore.srcreg;
                        IrReg tgtreg = irStmtInfo[irs].tStore.tgtaddrreg;
			break;
                }
                case IRSTMT_CALL:
			break;
                case IRSTMT_CONDGOTO:
			break;
                case IRSTMT_GOTO:
			break;
                case IRSTMT_RETURN:
			break;
                }
        }
}

void codegen_x64(void)
{
        x64asm_proc(0);

        for (int i = 0; i < codeSectionCnt; i++) {
                outf("%.2x", codeSection[i]);
                if (i & 7)
                        outf(" ");
                else
                        outf("\n");
        }
        outs("\n");
}
