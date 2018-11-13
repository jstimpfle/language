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
void begin_symbol(Symbol sym)
{
        SymDef sd = symDefCnt++;
        RESIZE_GLOBAL_BUFFER(symDefInfo, symDefCnt);
        symDefInfo[sd].symbol = sym;
        symDefInfo[sd].kind = SECTION_CODE;
        symDefInfo[sd].offset = codeSectionCnt;  //XXX Alignment?
        symDefInfo[sd].size = 0; // set later
}

INTERNAL
void end_symbol(void)
{
        SymDef sd = symDefCnt - 1;
        symDefInfo[sd].size = codeSectionCnt - symDefInfo[sd].offset;
}

INTERNAL
void emit_data(uchar *buf, int len)
{
}

INTERNAL
void emit_code(uchar *buf, int len)
{
        int pos = codeSectionCnt;
        codeSectionCnt += len;
        RESIZE_GLOBAL_BUFFER(codeSection, codeSectionCnt);
        mem_copy(codeSection + pos, buf, len);
}

INTERNAL
void emit_code_relocation(Symbol symbol, int offset)
{
        Reloc reloc = relocCnt++;
        RESIZE_GLOBAL_BUFFER(relocInfo, relocCnt);
        relocInfo[reloc].symbol = symbol;
        relocInfo[reloc].kind = SECTION_CODE;
        relocInfo[reloc].offset = offset;
}

#define BYTE(x, n) ((unsigned long long) (x) >> (8*(n)))
#define EMIT(...) do { \
        uchar bytes[] = { __VA_ARGS__ }; \
        emit_code(bytes, sizeof bytes); \
} while (0)

INTERNAL
void emit_mov_64_imm_reg(Constval imm, int x64reg)
{
#if 0
        EMIT(
                0x4c, 0x01, (x64reg<<0) | 0x00,
                /* up to 3 bytes imm */
                BYTE(imm, 0), BYTE(imm, 1), BYTE(imm, 2)
        );
#endif
        EMIT(
                0x00, 0x00,
                0x01, 0x00,
                0x02, 0x00,
                0x03, 0x00,
                0x04, 0x00,

                      0x89, 0x00,
                0x4c, 0x89, 0x00
        );
}

#define REX_BASE 0x40
#define REX_W    0x08  /* usually means "64-bit operand instead of default size" */
#define REX_R    0x04  /* usually shift register range of 1st operand to R8-... from RAX-... */
#define REX_X    0x02
#define REX_B    0x01  /* usually shift register range of 2st operand to R8-... from RAX-... */

/* Emit the MOD-REG-R/M byte and the optional displacement byte(s).
 *
 * r1 and r2 are interpreted without the 4th LSB,
 * such that they are always one of the low 8 registers. (The 4th LSB is encoded
 * in a different place in the x64 ISA). After that reinterpretation, r2 must
 * not be X64REG_RBP -- that value is taken for SIB addressing (scale index
 * byte).
 *
 * d is the displacement (register relative offset, which applies to r1 or to r2
 * depending on the preceding opcode). The value of d determines the 2 MSB of
 * the MOD-REG-R/M byte (i.e., the MOD part).
 *
 * If d == 0, MOD = 0b00 (no displacement, and no displacement bytes following).
 *
 * Else if -128 <= d <= 127, MOD = 0b01 (8bit signed displacement, and 1
 * displacement byte following).
 *
 * Else MOD = 0b10 (32bit signed displacement, and 4 displacement bytes
 * following).
 */
static inline int emit_modrmreg_and_displacement_bytes(int r1, int r2, long d)
{
        r1 &= 7;
        r2 &= 7;
        assert(0 <= r1 && r1 < 8);
        assert(0 <= r2 && r2 < 8 && r2 != X64REG_RBP);

        int mod;
        if (d == 0)
                mod = 0x00;
        else if (-128 <= d && d < 128) {
                mod = 0x01;
        }
        else {
                assert(-(1LL << 31) <= d);
                assert(d < (1LL << 31));
                mod = 0x02;
        }

        int modrmreg = (mod << 6) | (r1 << 3) | r2;

        EMIT(modrmreg);
        if (mod == 0x01)
                EMIT(d);
        else if (mod == 0x02)
                EMIT(BYTE(d, 0), BYTE(d, 1), BYTE(d, 2), BYTE(d, 3));
}

INTERNAL
void emit_add_64_imm_RAX(Constval imm)
{
        assert(imm < (1ULL << 32));
        EMIT(
                REX_BASE|REX_W, 0x05,
                BYTE(imm, 0), BYTE(imm, 1), BYTE(imm, 2), BYTE(imm, 3)
        );
}

INTERNAL
void emit_add_64_reg_indirect(int r1, int r2, long d)
{
        int R = (r1 >= 8) ? REX_R : 0;
        int B = (r2 >= 8) ? REX_B : 0;
        EMIT(REX_BASE|REX_W|R|B, 0x01);
        emit_modrmreg_and_displacement_bytes(r1, r2, d);
}

INTERNAL
void emit_add_64_indirect_reg(int r1, int r2, long d)
{
        int R = (r2 >= 8) ? REX_R : 0;
        int B = (r1 >= 8) ? REX_B : 0;
        EMIT(REX_BASE|REX_W|R|B, 0x03);
        emit_modrmreg_and_displacement_bytes(r1, r2, d);
}

void emit_mov_64_imm_indirect(Constval imm, int x64reg, long d)
{
        /*
        EMIT(
                REX_BASE|REX_W, 0x8a,
                BYTE(imm, 0), BYTE(imm, 1), BYTE(imm, 2), BYTE(imm, 3)
        );
        */
}

INTERNAL
void emit_mov_64_reg_indirect(int r1, int r2, long d)
{
        int R = (r1 >= 8) ? REX_R : 0;
        int B = (r2 >= 8) ? REX_B : 0;
        EMIT(REX_BASE|REX_W|R|B, 0x89);
        emit_modrmreg_and_displacement_bytes(r1, r2, d);
}

/* d = displacement value. */
INTERNAL
void emit_mov_64_indirect_reg(int r1, int r2, long d)
{
        int R = (r2 >= 8) ? REX_R : 0;
        int B = (r1 >= 8) ? REX_B : 0;
        EMIT(REX_BASE|REX_W|R|B, 0x89|0x02);
        emit_modrmreg_and_displacement_bytes(r1, r2, d);
}

INTERNAL
void emit_mov_64_reg_stack(int x64reg, X64StackLoc loc)
{
        // TODO
}

INTERNAL
void emit_load_constant_stack(Constval constval, X64StackLoc loc)
{
        emit_mov_64_imm_reg(constval, X64REG_RAX);
        emit_mov_64_reg_stack(X64REG_RAX, loc);
}

INTERNAL
void emit_load_symaddr_stack(Symbol symbol, X64StackLoc loc)
{
        emit_code_relocation(symbol, codeSectionCnt);
        emit_mov_64_imm_reg(0, X64REG_RAX);
        emit_mov_64_reg_stack(X64REG_RAX, loc);
}

INTERNAL
void emit_load_addrval_stack(Constval constval, X64StackLoc loc)
{
}

INTERNAL
void x64asm_proc(IrProc irp)
{
        begin_symbol(irProcInfo[irp].symbol);

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

        end_symbol();
}

void codegen_x64(void)
{
        for (IrProc x = 0; x < irProcCnt; x++)
                x64asm_proc(x);

        emit_add_64_imm_RAX(0x31);
        emit_add_64_indirect_reg(X64REG_RCX, X64REG_RDX, 0x3f3f3f3f);
        emit_add_64_reg_indirect(X64REG_RCX, X64REG_RDX, 0);
        emit_mov_64_reg_indirect(X64REG_RDX, X64REG_R11, 3);
        emit_mov_64_imm_indirect(42, X64REG_RAX, 0);
        emit_mov_64_indirect_reg(X64REG_RDX, X64REG_RBX, 127);
        //emit_mov_64_reg_indirect(X64REG_RSP, X64REG_R13);

        for (int i = 0; i < codeSectionCnt; i++) {
                if (i & 7)
                        outf(" ");
                else
                        outf("\n");
                outf("%.2x", codeSection[i]);
        }
        outs("\n");
}
