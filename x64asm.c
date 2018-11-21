/*
 * First try at register allocator and x64 assembler
 */

#include "defs.h"
#include "api.h"
#include <stdint.h>

typedef unsigned char uchar;
typedef int X64StackLoc;
typedef uint8_t Imm8;
typedef uint16_t Imm16;
typedef uint32_t Imm32;
typedef uint64_t Imm64;

const char *x64regNames[NUM_X64REGS] = {
#define MAKE(x) [x] = #x
        MAKE( X64_RAX ),
        MAKE( X64_RCX ),
        MAKE( X64_RDX ),
        MAKE( X64_RBX ),
        MAKE( X64_RSP ),
        MAKE( X64_RBP ),
        MAKE( X64_RSI ),
        MAKE( X64_RDI ),
        MAKE( X64_R8 ),
        MAKE( X64_R9 ),
        MAKE( X64_R10 ),
        MAKE( X64_R11 ),
        MAKE( X64_R12 ),
        MAKE( X64_R13 ),
        MAKE( X64_R14 ),
        MAKE( X64_R15 ),
#undef MAKE
};

INTERNAL
int find_stack_loc(IrReg irreg)
{
        // XXX
        int r = 0;
        while (irreg > 0 && irRegInfo[irreg-1].proc == irRegInfo[irreg].proc) {
                irreg--;
                r += 8;
        }
        return -r;
}

INTERNAL UNUSED
int is_imm8(Imm64 imm)
{
        return imm < (1u << 8);
}

INTERNAL UNUSED
int is_imm16(Imm64 imm)
{
        return imm < (1u << 16);
}

INTERNAL
int is_imm32(Imm64 imm)
{
        return imm < (1ull << 32);
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
void emit(uint8_t c)
{
        uint8_t b = c;
        emit_bytes(&b, 1);
}

#define BYTE(x, n) ((unsigned long long) (x) >> (8*(n)))
void emit16(uint32_t c)
{
        uchar bs[] = { BYTE(c, 0), BYTE(c, 1) };
        emit_bytes(bs, LENGTH(bs));
}

void emit32(uint32_t c)
{
        uchar bs[] = { BYTE(c, 0), BYTE(c, 1), BYTE(c, 2), BYTE(c, 3) };
        emit_bytes(bs, LENGTH(bs));
}

void emit64(uint32_t c)
{
        uchar bs[] = {
                BYTE(c, 0), BYTE(c, 1), BYTE(c, 2), BYTE(c, 3),
                BYTE(c, 4), BYTE(c, 5), BYTE(c, 6), BYTE(c, 7),
        };
        emit_bytes(bs, LENGTH(bs));
}

INTERNAL
int make_modrm_byte(unsigned mod, unsigned reg, unsigned rm)
{
        assert(mod < 4);
        assert(reg < 8);
        assert(rm < 8);
        return (mod << 6) | (reg << 3) | rm;
}


enum {
        SIB_SCALE_1 = 0x00,
        SIB_SCALE_2 = 0x01,
        SIB_SCALE_4 = 0x02,
        SIB_SCALE_8 = 0x03,
};

INTERNAL
int make_sib_byte(unsigned scale, unsigned r1, unsigned r2)
{
        assert(scale < 4);
        assert(r1 < 8);
        assert(r2 < 8);
        return (scale << 6) | (r1 << 3) | r2;
}

#define REX_BASE 0x40
#define REX_W    0x08  /* usually means "64-bit operand instead of default size" */
#define REX_R    0x04  /* Extension of the ModR/M reg field */
#define REX_X    0x02  /* Extension of the SIB index field */
#define REX_B    0x01  /* Extension of the ModR/M r/m field, SIB base field, or Opcode reg field */

/* Emit the MOD-REG-R/M byte and the optional SIB and displacement byte(s).
 *
 * r1 and r2 are interpreted without the 4th LSB, such that they are always one
 * of the low 8 registers. (The 4th LSB is encoded in a different place in the
 * x64 ISA).
 *
 * d is the displacement (register relative offset, which applies to r1 or to r2
 * depending on the preceding opcode).
 */
static inline void emit_modrmreg_and_displacement_bytes(int r1, int r2, long d)
{
        r1 &= 7;
        r2 &= 7;

        int mod;
        int sib;
        int havesib = 0;

        /* RBP indirect with 0 displacement is not possible with a mod=0x00
         * encoding I think. So we make a single byte displacement in this case.
         */
        if (d == 0 && r2 != X64_RBP) {
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

        if (r2 == X64_RSP) {
                havesib = 1;
                sib = make_sib_byte(0x00, 0x00, X64_RSP);
        }
        else if (r2 == X64_RBP && mod == 0x00) {
                mod = 0x01;
        }

        int modrm = make_modrm_byte(mod, r1, r2);
        emit(modrm);

        if (havesib)
                emit(sib);

        if (mod == 0x01)
                emit(d);
        else if (mod == 0x02) {
                emit32(d);
        }
}

INTERNAL UNUSED
void emit_add_64_reg_indirect(int r1, int r2, long d)
{
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|R|B);
        emit(0x01);
        emit_modrmreg_and_displacement_bytes(r1, r2, d);
}

INTERNAL UNUSED
void emit_add_64_indirect_reg(int r1, int r2, long d)
{
        int R = (r2 & ~7) ? REX_R : 0;
        int B = (r1 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|R|B);
        emit(0x01 | 0x02);
        emit_modrmreg_and_displacement_bytes(r2, r1, d);
}

INTERNAL UNUSED
void emit_add_64_reg_reg(int r1, int r2)
{
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|R|B);
        emit(0x01);
        emit(make_modrm_byte(0x03, r1 & 7, r2 & 7));
}

INTERNAL
void emit_add_imm32_reg(Imm32 imm, int reg)
{
        if (reg == X64_RAX) {
                /* optimization */
                emit(0x05);
                emit32(imm);
        }
        else {
                int B = (reg & ~7) ? REX_B : 0;
                emit(REX_BASE|REX_W|B);
                emit(0x81);
                emit(make_modrm_byte(0x03, 0x00, reg & 7));
                emit32(imm);
        }
}

INTERNAL
void emit_mov_64_imm_reg(Imm64 imm, int r1)
{
        if (is_imm32(imm)) {
                emit(0xb8 | r1);
                emit32(imm);
        }
        else {
                emit(REX_BASE|REX_W);
                emit(0xb8 | r1);
                emit64(imm);
        }
}

/* Like emit_mov_64_imm_reg(), but always use the 8-byte version */
INTERNAL UNUSED
void emit_mov_64_address_reg(Imm64 imm, int r1)
{
        int B = (r1 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|B);
        emit(0xb8 | r1);
        emit64(imm);
}
/* Like emit_mov_64_imm_reg(), but always use the 8-byte version and emit
 * a relocation */
INTERNAL
void emit_mov_64_reloc_reg(Symbol symbol, int r1)
{
        int B = (r1 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|B);
        emit(0xb8 | r1);
        {
                int offset = codeSectionCnt;
                Reloc reloc = relocCnt++;
                RESIZE_GLOBAL_BUFFER(relocInfo, relocCnt);
                relocInfo[reloc].symbol = symbol;
                relocInfo[reloc].kind = SECTION_CODE;
                relocInfo[reloc].offset = offset;
        }
        emit64(0x00);
}

INTERNAL
void emit_mov_64_reg_reg(int r1, int r2)
{
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|R|B);
        emit(0x89);
        emit(make_modrm_byte(0x03, r1 & 7, r2 & 7));
}

INTERNAL
void emit_mov_64_reg_indirect(int r1, int r2, long d)
{
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|R|B);
        emit(0x89);
        emit_modrmreg_and_displacement_bytes(r1, r2, d);
}

/* d = displacement value. */
INTERNAL
void emit_mov_64_indirect_reg(int r1, int r2, long d)
{
        int R = (r2 & ~7) ? REX_R : 0;
        int B = (r1 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|R|B);
        emit(0x89|0x02);
        emit_modrmreg_and_displacement_bytes(r2, r1, d);
}

INTERNAL
void emit_mov_64_reg_stack(int reg, X64StackLoc loc)
{
        emit_mov_64_reg_indirect(reg, X64_RBP, loc);
}

INTERNAL
void emit_mov_64_stack_reg(X64StackLoc loc, int reg)
{
        emit_mov_64_indirect_reg(X64_RBP, reg, loc);
}

INTERNAL
void emit_load_constant_stack(Imm64 imm, X64StackLoc loc)
{
        /* 64-bit immediate loads to stack variable only via imm64
         * register load */
        emit_mov_64_imm_reg(imm, X64_RAX);
        emit_mov_64_reg_stack(X64_RAX, loc);
}

INTERNAL
void emit_load_symaddr_stack(Symbol symbol, X64StackLoc loc)
{
        int r1 = X64_RAX;
        if (symbolInfo[symbol].scope == SCOPE_GLOBAL)
                emit_mov_64_reloc_reg(symbol, r1);
        else
                /* for now. TODO: get proper register or memory location where
                 * symbol is */
                emit_mov_64_address_reg(0, r1);
        emit_mov_64_reg_stack(r1, loc);
}

INTERNAL
void emit_call_reg(int r1)
{
        /* FF /2 */
        int B = (r1 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|B);
        emit(0xff);
        emit(make_modrm_byte(0x3, 0x02, r1 & 7));
}

INTERNAL
void emit_function_prologue(void)
{
        emit(0x55);  // push rbp
        emit_mov_64_reg_reg(X64_RSP, X64_RBP);
        emit_add_imm32_reg(-0x100 /*XXX*/, X64_RSP);
}

INTERNAL
void emit_function_epilogue(void)
{
        //emit_mov_64_reg_reg(X64_RBP, X64_RSP);
        emit(0xc9);  // leave
        emit(0xc3);  // ret
}

INTERNAL
void x64asm_proc(IrProc irp)
{
        begin_symbol(irProcInfo[irp].symbol);
        emit_function_prologue();
        for (IrStmt irs = irProcInfo[irp].firstIrStmt;
             irs < irStmtCnt && irStmtInfo[irs].proc == irp;
             irs++) {
                switch (irStmtInfo[irs].kind) {
                case IRSTMT_LOADCONSTANT: {
                        Imm64 v = irStmtInfo[irs].tLoadConstant.constval;
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
                        X64StackLoc srcloc = find_stack_loc(addrreg);
                        X64StackLoc tgtloc = find_stack_loc(tgtreg);
                        emit_mov_64_stack_reg(srcloc, X64_RAX);
                        emit_mov_64_indirect_reg(X64_RAX, X64_RAX, 0);
                        emit_mov_64_reg_stack(X64_RAX, tgtloc);
			break;
                }
                case IRSTMT_STORE: {
                        IrReg addrreg = irStmtInfo[irs].tStore.srcreg;
                        IrReg tgtreg = irStmtInfo[irs].tStore.tgtaddrreg;
                        X64StackLoc srcloc = find_stack_loc(addrreg);
                        X64StackLoc tgtloc = find_stack_loc(tgtreg);
                        emit_mov_64_stack_reg(srcloc, X64_RAX);
                        emit_mov_64_stack_reg(tgtloc, X64_RCX);
                        emit_mov_64_reg_indirect(X64_RAX, X64_RCX, 0);
			break;
                }
                case IRSTMT_REGREG: {
                        IrReg srcreg = irStmtInfo[irs].tRegreg.srcreg;
                        IrReg tgtreg = irStmtInfo[irs].tRegreg.tgtreg;
                        X64StackLoc srcloc = find_stack_loc(srcreg);
                        X64StackLoc tgtloc = find_stack_loc(tgtreg);
                        emit_mov_64_stack_reg(srcloc, X64_RAX);
                        emit_mov_64_reg_stack(X64_RAX, tgtloc);
			break;
                }
                case IRSTMT_CALL: {
                        static const int cc[] = { /* "calling convention" */
                                X64_RDI, X64_RSI, X64_RDX,
                                X64_RCX, X64_R8, X64_R9
                        };
                        IrReg calleeReg = irStmtInfo[irs].tCall.calleeReg;
                        X64StackLoc calleeloc = find_stack_loc(calleeReg);
                        IrCallArg firstArg = irStmtInfo[irs].tCall.firstIrCallArg;
                        for (int i = 0; ; i++) {
                                IrCallArg a = firstArg + i;
                                if (!(a < irCallArgCnt
                                      && irCallArgInfo[a].callStmt == irs))
                                        break;
                                IrReg r = irCallArgInfo[a].srcreg;
                                X64StackLoc loc = find_stack_loc(r);
                                // move to appropriate register
                                // XXX: this is of course ad-hoc and wrong. For
                                // instance, we do not even check if that
                                // register is free
                                emit_mov_64_stack_reg(loc, cc[i]);
                        }
                        IrCallResult cr0 = irStmtInfo[irs].tCall.firstIrCallResult;
                        IrReg rreg = irCallResultInfo[cr0].tgtreg;
                        X64StackLoc rloc = find_stack_loc(rreg);
                        emit_mov_64_stack_reg(calleeloc, X64_R11);
                        emit_call_reg(X64_R11);
                        emit_mov_64_reg_stack(X64_RAX, rloc);
			break;
                }
                case IRSTMT_CONDGOTO:
			break;
                case IRSTMT_GOTO:
			break;
                case IRSTMT_RETURN: {
                        IrReturnResult result = irStmtInfo[irs].tReturn.firstResult;
                        if (result != -1) {
                                IrReg irreg = irReturnResultInfo[result].resultReg;
                                X64StackLoc loc = find_stack_loc(irreg);
                                emit_mov_64_stack_reg(loc, X64_RAX);
                        }
                        /* if it's not the last statement then emit an extra
                         * return */
                        if (irs+1 < irStmtCnt &&
                            irStmtInfo[irs+1].proc == irStmtInfo[irs].proc) {
                                emit_function_epilogue();
                        }
			break;
                }
                default:
                        UNHANDLED_CASE();
                }
        }
        emit_function_epilogue();
        end_symbol();
}

void codegen_x64(void)
{
        for (IrProc x = 0; x < irProcCnt; x++)
                x64asm_proc(x);

        /*
        emit_add_64_imm_RAX(0x31);
        emit_add_64_indirect_reg(X64_RCX, X64_RDX, 0x3f3f3f3f);
        emit_add_64_reg_indirect(X64_RCX, X64_RDX, 0x27);
        emit_mov_64_reg_reg(X64_R11, X64_RSP);
        emit_mov_64_reg_indirect(X64_RDX, X64_RBP, 0x0);
        emit_mov_64_indirect_reg(X64_RDX, X64_RBX, 0x2);
        emit_mov_64_reg_indirect(X64_RSP, X64_RSP, 0x7);
        */

        /*
        for (int i = 0; i < codeSectionCnt; i++) {
                if (i & 7)
                        outf(" ");
                else
                        outf("\n");
                outf("%.2x", codeSection[i]);
        }
        outs("\n");
        */
}
