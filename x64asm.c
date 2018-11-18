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
void emit_bytes(uchar *buf, int len)
{
        int pos = codeSectionCnt;
        codeSectionCnt += len;
        RESIZE_GLOBAL_BUFFER(codeSection, codeSectionCnt);
        copy_mem(codeSection + pos, buf, len);
}

INTERNAL
void emit(Constval c)
{
        assert(-128 <= c && c < 128);
        uchar b = c;
        emit_bytes(&b, 1);
}

#define BYTE(x, n) ((unsigned long long) (x) >> (8*(n)))
void emit4(Constval c)
{
        uchar bs[] = { BYTE(c, 0), BYTE(c, 1), BYTE(c, 2), BYTE(c, 3) };
        emit_bytes(bs, 4);
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

#define EMIT(...) do { \
        uchar bytes[] = { __VA_ARGS__ }; \
        emit_bytes(bytes, sizeof bytes); \
} while (0)

static int make_modrm_byte(unsigned mod, unsigned reg, unsigned rm)
{
        assert(mod < 4);
        assert(reg < 8);
        assert(rm < 8);
        return (mod << 6) | (reg << 3) | rm;
}

#define REX_BASE 0x40
#define REX_W    0x08  /* usually means "64-bit operand instead of default size" */
#define REX_R    0x04  /* Extension of the ModR/M reg field */
#define REX_X    0x02  /* Extension of the SIB index field */
#define REX_B    0x01  /* Extension of the ModR/M r/m field, SIB base field, or Opcode reg field */

/* Emit the MOD-REG-R/M byte and the optional SIB and displacement byte(s).
 *
 * r1 and r2 are interpreted without the 4th LSB,
 * such that they are always one of the low 8 registers. (The 4th LSB is encoded
 * in a different place in the x64 ISA).
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

        int mod;
        int sib;
        int havesib = 0;

        /* RBP indirect with 0 displacement is not possible with a mod=0x00
         * encoding I think. So we make a single byte displacement in this case.
         */
        if (d == 0 && r2 != X64REG_RBP) {
                mod = 0x00;
        }
        else if (-128 <= d && d < 128) {
                mod = 0x01;
        }
        else {
                assert(-(1LL << 31) <= d);
                assert(d < (1LL << 31));
                mod = 0x02;
        }

        if (r2 == X64REG_RSP) {
                havesib = 1;
                sib = X64REG_RSP;
        }
        else if (r2 == X64REG_RBP && mod == 0x00) {
                mod = 0x01;
        }

        int modrm = make_modrm_byte(mod, r1, r2);
        EMIT(modrm);

        if (havesib)
                emit(sib);

        if (mod == 0x01)
                emit(d);
        else if (mod == 0x02) {
                emit4(d);
        }
}

INTERNAL
void emit_mov_64_imm_reg(Constval imm, int r1, long long d)
{
        /*
        EMIT(0x48, 0xb8);
        emit_modrmreg_and_displacement_bytes(X64REG_RAX, r1, 10);
        */
}

INTERNAL
void emit_mov_64_imm_RAX(Constval imm)
{
        EMIT(0x48, 0xb8);
        emit4(imm);
}

INTERNAL
void emit_add_64_imm_RAX(Constval imm)
{
        assert(imm < (1ULL << 32));
        EMIT(REX_BASE|REX_W, 0x05);
        emit4(imm);
}

INTERNAL
void emit_add_64_reg_indirect(int r1, int r2, long d)
{
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        EMIT(REX_BASE|REX_W|R|B, 0x01);
        emit_modrmreg_and_displacement_bytes(r1, r2, d);
}

INTERNAL
void emit_add_64_indirect_reg(int r1, int r2, long d)
{
        int R = (r2 & ~7) ? REX_R : 0;
        int B = (r1 & ~7) ? REX_B : 0;
        EMIT(REX_BASE|REX_W|R|B, 0x03);
        emit_modrmreg_and_displacement_bytes(r2, r1, d);
}

INTERNAL
void emit_mov_64_reg_reg(int r1, int r2)
{
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        int modrm = make_modrm_byte(0x03, r1 & 7, r2 & 7);
        EMIT(REX_BASE|REX_W|R|B, 0x89, modrm);
}

INTERNAL
void emit_mov_64_reg_indirect(int r1, int r2, long d)
{
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        EMIT(REX_BASE|REX_W|R|B, 0x89);
        emit_modrmreg_and_displacement_bytes(r1, r2, d);
}

/* d = displacement value. */
INTERNAL
void emit_mov_64_indirect_reg(int r1, int r2, long d)
{
        int R = (r2 & ~7) ? REX_R : 0;
        int B = (r1 & ~7) ? REX_B : 0;
        EMIT(REX_BASE|REX_W|R|B, 0x89|0x02);
        emit_modrmreg_and_displacement_bytes(r2, r1, d);
}

INTERNAL
void emit_mov_64_reg_stack(int x64reg, X64StackLoc loc)
{
        // TODO
}

INTERNAL
void emit_load_constant_stack(Constval constval, X64StackLoc loc)
{
        emit_mov_64_imm_reg(constval, X64REG_RSP, loc);
}

INTERNAL
void emit_load_symaddr_stack(Symbol symbol, X64StackLoc loc)
{
        emit_code_relocation(symbol, codeSectionCnt);
        emit_mov_64_imm_reg(0, X64REG_RAX, 0);
        emit_mov_64_reg_stack(X64REG_RAX, loc);
}

INTERNAL
void emit_load_addrval_stack(Constval constval, X64StackLoc loc)
{
}

INTERNAL
void emit_function_epilogue(void)
{
        emit_mov_64_reg_reg(X64REG_RBP, X64REG_RSP);
        EMIT(0xc3);
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
                case IRSTMT_RETURN: {
                        IrReturnResult result = irStmtInfo[irs].tReturn.firstResult;
                        if (result != -1) {
                                IrReg irreg = irReturnResultInfo[result].resultReg;
                                X64StackLoc loc = find_stack_loc(irreg);
                                loc = 0x13; //XXX test
                                emit_mov_64_indirect_reg(X64REG_RSP, X64REG_RAX, loc);
                        }
                        goto funcreturn;
			break;
                }
                default:
                        UNHANDLED_CASE();
                }
        }

funcreturn:
        emit_function_epilogue();
        end_symbol();
}

void codegen_x64(void)
{
        /*
        for (IrProc x = 0; x < irProcCnt; x++)
                x64asm_proc(x);
                */

        emit_add_64_imm_RAX(0x31);
        emit_add_64_indirect_reg(X64REG_RCX, X64REG_RDX, 0x3f3f3f3f);
        emit_add_64_reg_indirect(X64REG_RCX, X64REG_RDX, 0x27);
        emit_mov_64_reg_reg(X64REG_R11, X64REG_RSP);
        emit_mov_64_reg_indirect(X64REG_RDX, X64REG_RBP, 0x0);
        emit_mov_64_indirect_reg(X64REG_RDX, X64REG_RBX, 0x2);
        emit_mov_64_reg_indirect(X64REG_RSP, X64REG_RSP, 0x7);

        for (int i = 0; i < codeSectionCnt; i++) {
                if (i & 7)
                        outf(" ");
                else
                        outf("\n");
                outf("%.2x", codeSection[i]);
        }
        outs("\n");
}
