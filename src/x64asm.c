/*
 * First try at register allocator and x64 assembler
 */

#include "defs.h"
#include "api.h"
#include <stdint.h>

#define BYTE(x, n) ((unsigned char) ((unsigned long long) (x) >> (8*(n))))

typedef int8_t Imm8;
typedef int16_t Imm16;
typedef int32_t Imm32;
typedef int64_t Imm64;

enum {
        REX_BASE = 0x40,
        REX_W = 0x08,  /* usually means "64-bit operand instead of default size" */
        REX_R = 0x04,  /* Extension of the ModR/M reg field */
        REX_X = 0x02,  /* Extension of the SIB index field */
        REX_B = 0x01,  /* Extension of the ModR/M r/m field, SIB base field, or Opcode reg field */

        REX_NOT_W = 0x00,  /* just to be able to be extra clear */
};

enum {
        MODRM_NODISP = 0x00,
        MODRM_DISP8 = 0x01,
        MODRM_DISP32 = 0x02,
        MODRM_REGREG = 0x03,
};

enum {
        SIB_SCALE_1 = 0x00,
        SIB_SCALE_2 = 0x01,
        SIB_SCALE_4 = 0x02,
        SIB_SCALE_8 = 0x03,
};


enum {
        /* The order is important here: these values (0000...1111 binary) are
         * also used for instruction encoding. */
        X64_RAX,
        X64_RCX,
        X64_RDX,
        X64_RBX,
        X64_RSP,
        X64_RBP,
        X64_RSI,
        X64_RDI,
        X64_R8,
        X64_R9,
        X64_R10,
        X64_R11,
        X64_R12,
        X64_R13,
        X64_R14,
        X64_R15,
        NUM_X64REGS,
};

/* a few SSE registers */
enum {
        XMM0,
        XMM1,
        XMM2,
        XMM3,
};

enum {
        X64CMP_LT,
        X64CMP_GT,
        X64CMP_LE,
        X64CMP_GE,
        X64CMP_EQ,
        X64CMP_NE,
        NUM_X64CMP_KINDS,
};

struct CCState {
        int regCnt;
        int floatCnt;
};

void reset_ccstate(struct CCState *state)
{
        state->regCnt = 0;
        state->floatCnt = 0;
}



enum {
        PASSING_STACK,
        PASSING_X64REG,
        PASSING_SINGLEPRECISION,
        PASSING_DOUBLEPRECISION,
};

INTERNAL const int cc[] = { /* "calling convention" */
        /* XXX: for now, a compile-time switch will do for chosing the
         * calling convention */
#ifdef _MSC_VER
                X64_RCX, X64_RDX, X64_R8, X64_R9,
#else
                X64_RDI, X64_RSI, X64_RDX,
                X64_RCX, X64_R8, X64_R9
#endif
};
INTERNAL const int numXmm = {
#ifdef _MSC_VER
                4,
#else
                8,
#endif
};

void ccstate_add_param(struct CCState *state, Type tp, int *passingKind, int *regbits)
{

        if (type_equal(tp, builtinType[BUILTINTYPE_FLOAT])) {
                int x = state->floatCnt ++;
                ASSERT(0 <= x && x < numXmm);
                *passingKind = PASSING_SINGLEPRECISION;
                *regbits = x;
        }
        else if (type_equal(tp, builtinType[BUILTINTYPE_DOUBLE])) {
                int x = state->floatCnt++;
                ASSERT(0 <= x && x < numXmm);
                *passingKind = PASSING_DOUBLEPRECISION;
                *regbits = x;
        }
        else if (state->regCnt == LENGTH(cc)) {
                *passingKind = PASSING_STACK;
                *regbits = 0;
        }
        else {
                int x = state->regCnt ++;
                ASSERT(0 <= x && x < LENGTH(cc));
                *passingKind = PASSING_X64REG;
                *regbits = cc[x];
        }
}


INTERNAL X64Float float_to_X64Float(float v)
{
        /*XXX*/
        return v;
}

INTERNAL Imm32 x64Float_to_imm32(X64Float v)
{
        /*XXX*/
        union {
                float v;
                Imm32 i;
        } u;
        u.v = v;
        return u.i;
}

INTERNAL Imm64 x64Float_to_imm64(X64Float v)
{
        /*XXX*/
        union {
                double v;
                Imm64 i;
        } u;
        u.v = (double)v;
        return u.i;
}

INTERNAL int make_modrm_byte(int mod, int reg, int rm)
{
        ASSERT(mod < 4);
        ASSERT(reg < 8);
        ASSERT(rm < 8);
        return (mod << 6) | (reg << 3) | rm;
}

INTERNAL int make_sib_byte(int scale, int r1, int r2)
{
        ASSERT(scale < 4);
        ASSERT(r1 < 8);
        ASSERT(r2 < 8);
        return (scale << 6) | (r1 << 3) | r2;
}

INTERNAL int find_stack_loc(IrReg irreg)
{
        int out = 0;
        for (IrReg reg = irreg;
             reg >= 0 && irRegInfo[reg].proc == irRegInfo[irreg].proc;
             reg--) {
                Type tp = referenced_type(irRegInfo[reg].tp);
                ASSERT(tp != (Type) -1);
                if (tp == builtinType[BUILTINTYPE_VOID]) {
                        /*
                         * XXX: this is to work around against the IR compiler
                         * emitting IrReg register of type void. That should be
                         * fixed in the compiler proper at some point. But it
                         * will take a few structural changes so its a lot more
                         * work.
                         */
                        continue;
                }
                int size = get_type_size(tp);
                if (size <= 0)
                        MSG(lvl_error, "sizeof reg=%d proc=%d tp=%d is %d\n",
                            reg, irRegInfo[reg].proc, tp, size);
                ASSERT(size > 0);
                out += size;
        }
        return -out;
}

INTERNAL int compute_size_of_stack_frame(IrProc irp)
{
        int out = 0;
        for (IrReg irreg = irProcInfo[irp].firstIrReg;
             irreg < irRegCnt && irRegInfo[irreg].proc == irp;
             irreg++)
        {
                Type tp = irRegInfo[irreg].tp;
                ASSERT(tp >= 0);
                int size = get_type_size(tp);
                if (size < 0) {
                        /* There are still issues causing void on the stack. */
                        continue;
                }
                out += size;
        }
        return out;
}

INTERNAL void emit8(int sectionKind, uint8_t c)
{
        unsigned char b = c;
        emit_bytes(sectionKind, &b, 1);
}

UNUSEDFUNC
INTERNAL void emit16(int sectionKind, uint16_t c)
{
        unsigned char bs[2];
        bs[0] = BYTE(c, 0);
        bs[1] = BYTE(c, 1);
        emit_bytes(sectionKind, bs, 2);
}

INTERNAL void emit32(int sectionKind, uint32_t c)
{
        unsigned char bs[4];
        bs[0] = BYTE(c, 0);
        bs[1] = BYTE(c, 1);
        bs[2] = BYTE(c, 2);
        bs[3] = BYTE(c, 3);
        emit_bytes(sectionKind, bs, 4);
}

INTERNAL void emit64(int sectionKind, uint64_t c)
{
        unsigned char bs[8];
        bs[0] = BYTE(c, 0);
        bs[1] = BYTE(c, 1);
        bs[2] = BYTE(c, 2);
        bs[3] = BYTE(c, 3);
        bs[4] = BYTE(c, 4);
        bs[5] = BYTE(c, 5);
        bs[6] = BYTE(c, 6);
        bs[7] = BYTE(c, 7);
        emit_bytes(sectionKind, bs, 8);
}

INTERNAL UNUSEDFUNC void emit_X64Float(int sectionKind, X64Float v)
{
        emit_bytes(sectionKind, &v, 4);  /*XXX*/
}

INTERNAL void emit_rex_byte(int w, int r1, int r2)
{
        ASSERT(w == 0 || w == REX_W);
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        emit8(SECTION_CODE, (uint8_t) (REX_BASE | w | R | B));
}

/* Emit opcode. The opcodes that we use here are, so far, either single byte
 * opcodes, or multi-byte opcodes where the first byte is 0F. I don't know what
 * surprise the x64 ISA still has in store for us, but for now we'll encode that
 * as an int. */
INTERNAL void emit_opcode(unsigned opcode)
{
        if (opcode >> 16) goto do24;
        else if (opcode >> 8) goto do16;
        else goto do8;
do24:
        emit8(SECTION_CODE, (opcode >> 16) & 0xFF);
do16:
        emit8(SECTION_CODE, (opcode >> 8) & 0xFF);
do8:
        emit8(SECTION_CODE, opcode & 0xFF);
}

INTERNAL void emit_modrm_byte(int mod, int reg, int rm)
{
        /* only the x86 registers (3bits) are relevant */
        reg &= 7;
        rm &= 7;
        emit8(SECTION_CODE, (uint8_t) make_modrm_byte(mod, reg, rm));
}

/* Emit the MOD-REG-R/M byte and the optional SIB and displacement byte(s).
 *
 * r1 and r2 are interpreted without the 4th LSB, such that they are always one
 * of the low 8 registers. (The 4th LSB is encoded in a different place in the
 * x64 ISA).
 *
 * d is the displacement (register relative offset, which applies to r1 or to r2
 * depending on the preceding opcode).
 */
INTERNAL void emit_modrm_and_sib_and_displacement_bytes(int r1, int r2, long d)
{
        int mod;
        int sib = 0;  /* initialization only because MSVC is too stupid */
        int havesib = 0;

        /* only the x86 registers (3bits) are relevant */
        r1 &= 7;
        r2 &= 7;

        /* RBP indirect with 0 displacement is not possible with a mod=0x00
         * encoding I think. So we make a single byte displacement in this case.
         */
        if (d == 0 && r2 != X64_RBP)
                mod = MODRM_NODISP;
        else if (-128 <= d && d < 128)
                mod = MODRM_DISP8;
        else {
                ASSERT(-(1LL << 31) <= d && d < (1LL < 31));
                mod = MODRM_DISP32;
        }

        /* RSP indirect is never possible with only the MOD-REG-R/M byte.
         * So we make a SIB byte specifying base register RSP without offset. */
        if (r2 == X64_RSP) {
                havesib = 1;
                /* The third argument to make_sib_byte() specifies RSP as base
                 * register. The second argument is also RSP which is a special
                 * case that means "no index". The first argument is an
                 * arbitrary scale for the index (it's arbitrary since we
                 * specified no index). */
                sib = make_sib_byte(SIB_SCALE_1, X64_RSP, X64_RSP);
        }
        else if (r2 == X64_RBP && mod == MODRM_NODISP) {
                mod = MODRM_DISP8;
        }

        emit_modrm_byte(mod, r1, r2);
        if (havesib)
                emit8(SECTION_CODE, (uint8_t) sib);
        if (mod == MODRM_DISP8)
                emit8(SECTION_CODE, (uint8_t) d);
        else if (mod == MODRM_DISP32)
                emit32(SECTION_CODE, (uint32_t) d);
}

/*****************************************
 * Instruction encoding (no REX byte)
 *****************************************/

INTERNAL void emit_instruction_reg_reg(int opcode, int r1, int r2)
{
        emit_opcode(opcode);
        emit_modrm_byte(MODRM_REGREG, r1 & 7, r2);
}

INTERNAL void emit_instruction_reg_indirect(int opcode, int r1, int r2, long d)
{
        emit_opcode(opcode);
        emit_modrm_and_sib_and_displacement_bytes(r1, r2, d);
}

INTERNAL void emit_instruction_reg(int opcode, int morebits, int r1)
{
        ASSERT(0 <= morebits && morebits < 8);
        emit_opcode(opcode);
        emit_modrm_byte(MODRM_REGREG, morebits, r1);
}

INTERNAL UNUSEDFUNC void emit_instruction_imm8_reg(int opcode, int morebits, Imm8 imm, int r1)
{
        ASSERT(0 <= morebits && morebits < 8);
        emit_opcode(opcode);
        emit_modrm_byte(MODRM_REGREG, morebits, r1 & 7);
        emit8(SECTION_CODE, imm);
}

INTERNAL void emit_instruction_imm32_reg(int opcode, int morebits, Imm32 imm, int r1)
{
        ASSERT(0 <= morebits && morebits < 8);
        emit_opcode(opcode);
        emit_modrm_byte(MODRM_REGREG, morebits, r1 & 7);
        emit32(SECTION_CODE, imm);
}

INTERNAL void emit_instruction_imm8_indirect(int opcode, int morebits, Imm8 imm, int r1, long d)
{
        ASSERT(0 <= morebits && morebits < 8);
        emit_opcode(opcode);
        emit_modrm_and_sib_and_displacement_bytes(morebits, r1, d);
        emit8(SECTION_CODE, imm);
}

INTERNAL void emit_instruction_imm32_indirect(int opcode, int morebits, Imm32 imm, int r1, long d)
{
        ASSERT(0 <= morebits && morebits < 8);
        emit_opcode(opcode);
        emit_modrm_and_sib_and_displacement_bytes(morebits, r1, d);
        emit32(SECTION_CODE, imm);
}


/**************************
 * Instruction encoding including REX byte. Do we really need those?
 **************************/

INTERNAL void emit_rex_instruction_reg_reg(int opcode, int r1, int r2)
{
        emit_rex_byte(REX_W, r1, r2);
        emit_instruction_reg_reg(opcode, r1, r2);
}

INTERNAL void emit_rex_instruction_reg_indirect(int w, int opcode, int r1, int r2, long d)
{
        emit_rex_byte(w, r1, r2);
        emit_instruction_reg_indirect(opcode, r1, r2, d);
}

INTERNAL void emit_rex_instruction_reg(int opcode, int morebits, int r1)
{
        emit_rex_byte(REX_W, 0, r1);
        emit_instruction_reg(opcode, morebits, r1);
}

INTERNAL UNUSEDFUNC void emit_rex_instruction_imm8_reg(int w, int opcode, int morebits, Imm8 imm, int r1)
{
        emit_rex_byte(w, 0, r1);
        emit_instruction_imm8_reg(opcode, morebits, imm, r1);
}

INTERNAL void emit_rex_instruction_imm32_reg(int opcode, int morebits, Imm32 imm, int r1)
{
        emit_rex_byte(REX_W, 0, r1);
        emit_instruction_imm32_reg(opcode, morebits, imm, r1);
}

INTERNAL void emit_rex_instruction_imm8_indirect(int w, int opcode, int morebits, Imm8 imm, int r1, long d)
{
        emit_rex_byte(w, 0, r1);
        emit_instruction_imm8_indirect(opcode, morebits, imm, r1, d);
}

INTERNAL void emit_rex_instruction_imm32_indirect(int w, int opcode, int morebits, Imm32 imm, int r1, long d)
{
        emit_rex_byte(w, 0, r1);
        emit_instruction_imm32_indirect(opcode, morebits, imm, r1, d);
}

/* opreg == register encoded in the low 3 bits of operand (+ optional REX_B).
 * No MOD/RM byte. */
INTERNAL void emit_rex_instruction_opreg(int opcode, int r1)
{
        ASSERT(!(opcode & 7));
        int B = (r1 & ~7) ? REX_B : 0;
        emit8(SECTION_CODE, (uint8_t) (REX_BASE | REX_W | B));
        emit_opcode(opcode | (r1 & 7));
}

INTERNAL void emit_rex_instruction_imm64_opreg(int opcode, Imm64 imm, int r1)
{
        ASSERT(!(opcode & 7));
        int B = (r1 & ~7) ? REX_B : 0;
        emit8(SECTION_CODE, (uint8_t) (REX_BASE | REX_W | B));
        emit_opcode(opcode | (r1 & 7));
        emit64(SECTION_CODE, imm);
}

INTERNAL void emit_mov_64_reg_reg     (int r1, int r2) { emit_rex_instruction_reg_reg(0x89, r1, r2); }
INTERNAL void emit_add_64_reg_reg     (int r1, int r2) { emit_rex_instruction_reg_reg(0x01, r1, r2); }
INTERNAL void emit_sub_64_reg_reg     (int r1, int r2) { emit_rex_instruction_reg_reg(0x2B, r1, r2); }
INTERNAL void emit_bitand_64_reg_reg  (int r1, int r2) { emit_rex_instruction_reg_reg(0x23, r1, r2); }
INTERNAL void emit_bitor_64_reg_reg   (int r1, int r2) { emit_rex_instruction_reg_reg(0x0B, r1, r2); }
INTERNAL void emit_bitxor_64_reg_reg  (int r1, int r2) { emit_rex_instruction_reg_reg(0x33, r1, r2); }
INTERNAL void emit_cmp_64_reg_reg     (int r1, int r2) { emit_rex_instruction_reg_reg(0x3B, r1, r2); }

/* Multiply RAX with given register. Result is in RDX:RAX */
INTERNAL void emit_mul_64_rax_reg     (int r1)          { emit_rex_instruction_reg(0xF7, 0x04, r1); }
/* Divide RAX with given register. Result is in ?? */
INTERNAL void emit_div_64             (int r1)          { emit_rex_instruction_reg(0xF7, 0x06, r1); }

INTERNAL void emit_bitwisenot_64_reg  (int r1) { emit_rex_instruction_reg(0xF7, 0x02, r1); }
INTERNAL void emit_inc_64_reg         (int r1) { emit_rex_instruction_reg(0xFF, 0x00, r1); }
INTERNAL void emit_dec_64_reg         (int r1) { emit_rex_instruction_reg(0xFF, 0x01, r1); }
INTERNAL void emit_call_reg           (int r1) { emit_rex_instruction_reg(0xFF, 0x02, r1); }

INTERNAL void emit_mov_8_reg_indirect   (int r1, int r2, long d) { emit_rex_instruction_reg_indirect(REX_NOT_W, 0x88, r1, r2, d); }
INTERNAL void emit_mov_32_reg_indirect  (int r1, int r2, long d) { emit_rex_instruction_reg_indirect(REX_NOT_W, 0x89, r1, r2, d); }
INTERNAL void emit_mov_64_reg_indirect  (int r1, int r2, long d) { emit_rex_instruction_reg_indirect(REX_W, 0x89, r1, r2, d); }
UNUSEDFUNC
INTERNAL void emit_add_64_reg_indirect  (int r1, int r2, long d) { emit_rex_instruction_reg_indirect(REX_W, 0x01, r1, r2, d); }

INTERNAL void emit_mov_8_indirect_reg   (int r1, int r2, long d) { emit_rex_instruction_reg_indirect(REX_NOT_W, 0x0FB6, r2, r1, d); }
INTERNAL void emit_mov_32_indirect_reg  (int r1, int r2, long d) { emit_rex_instruction_reg_indirect(REX_NOT_W, 0x8B, r2, r1, d); }  /* TODO: zero extend! */
INTERNAL void emit_mov_64_indirect_reg  (int r1, int r2, long d) { emit_rex_instruction_reg_indirect(REX_W, 0x8B, r2, r1, d); }
UNUSEDFUNC
INTERNAL void emit_add_64_indirect_reg  (int r1, int r2, long d) { emit_rex_instruction_reg_indirect(REX_W, 0x03, r2, r1, d); }

UNUSEDFUNC
INTERNAL void emit_add_64_imm8_reg   (Imm8 imm, int reg)  { emit_rex_instruction_imm8_reg(REX_W, 0x83, 0x00, imm, reg); }
INTERNAL void emit_add_64_imm32_reg  (Imm32 imm, int reg) { emit_rex_instruction_imm32_reg(0x81, 0x00, imm, reg); }
UNUSEDFUNC
INTERNAL void emit_mov_64_imm32_reg  (Imm32 imm, int reg) { emit_rex_instruction_imm32_reg(0xC7, 0x00, imm, reg); }
INTERNAL void emit_mov_64_imm64_reg  (Imm64 imm, int reg) { emit_rex_instruction_imm64_opreg(0xB8, imm, reg); }

UNUSEDFUNC
INTERNAL void emit_add_64_imm8_indirect(Imm8 imm, int reg, long d)   { emit_rex_instruction_imm8_indirect(REX_NOT_W, 0x83, 0x00, imm, reg, d); }
UNUSEDFUNC
INTERNAL void emit_add_64_imm32_indirect(Imm32 imm, int reg, long d)          { emit_rex_instruction_imm32_indirect(REX_NOT_W, 0x81, 0x00, imm, reg, d); }
INTERNAL void emit_mov_32_imm32_indirect(Imm32 imm, int r2, X64StackLoc loc)  { emit_rex_instruction_imm32_indirect(REX_NOT_W, 0xC7, 0x00, imm, r2, loc); }


INTERNAL void emit_movss_xmm_indirect(int xreg, int reg, long d) { emit_instruction_reg_indirect(0xF30F11, xreg, reg, d); }
INTERNAL void emit_movss_indirect_xmm(int reg, int xreg, long d) { emit_instruction_reg_indirect(0xF30F10, xreg, reg, d); }
INTERNAL void emit_addss_xmm_xmm(int x1, int x2)                 { emit_instruction_reg_reg(0xF30F58, x2, x1); }
INTERNAL void emit_subss_xmm_xmm(int x1, int x2)                 { emit_instruction_reg_reg(0xF30F5C, x2, x1); }
INTERNAL void emit_mulss_xmm_xmm(int x1, int x2)                 { emit_instruction_reg_reg(0xF30F59, x2, x1); }
INTERNAL void emit_divss_xmm_xmm(int x1, int x2)                 { emit_instruction_reg_reg(0xF30F5E, x2, x1); }

INTERNAL void emit_movsd_xmm_indirect(int xreg, int reg, long d) { emit_instruction_reg_indirect(0xF20F11, xreg, reg, d); }
INTERNAL void emit_movsd_indirect_xmm(int reg, int xreg, long d) { emit_instruction_reg_indirect(0xF20F10, xreg, reg, d); }
INTERNAL void emit_addsd_xmm_xmm(int x1, int x2) { emit_instruction_reg_reg(0xF20F58, x2, x1); }
INTERNAL void emit_subsd_xmm_xmm(int x1, int x2) { emit_instruction_reg_reg(0xF20F5C, x2, x1); }
INTERNAL void emit_mulsd_xmm_xmm(int x1, int x2) { emit_instruction_reg_reg(0xF20F59, x2, x1); }
INTERNAL void emit_divsd_xmm_xmm(int x1, int x2) { emit_instruction_reg_reg(0xF20F5E, x2, x1); }


//INTERNAL void emit_mov_float_rip_xreg(X64Float v, int xr) { emit_opcode(0xF30F10); emit32(SECTION_CODE, v); }

INTERNAL void emit_setcc(int x64CmpKind, int r1)
{
        ASSERT(0 <= x64CmpKind && x64CmpKind < NUM_X64CMP_KINDS);
        static const int x64CmpToOpcode[NUM_X64CMP_KINDS] = {
                [X64CMP_LT] = 0x0F9C,
                [X64CMP_GT] = 0x0F9F,
                [X64CMP_LE] = 0x0F9E,
                [X64CMP_GE] = 0x0F9D,
                [X64CMP_EQ] = 0x0F94,
                [X64CMP_NE] = 0x0F95,
        };
        int opcode = x64CmpToOpcode[x64CmpKind];
        int morebits = 0x00;
        emit_rex_instruction_reg(opcode, morebits, r1);
}

INTERNAL void emit_push_64(int r1)
{
        /*
        if (r1 == (r1 & 7))
        emit8(SECTION_CODE, 0x50 | r1);
        else
        */
        emit_rex_instruction_opreg(0x50, r1);
}

INTERNAL void emit_pop_64(int r1)
{
        /*
        if (r1 == (r1 & 7))
        emit8(SECTION_CODE, 0x58 | r1);
        else
        */
        emit_rex_instruction_opreg(0x58, r1);
}

INTERNAL void emit_mov_64_imm_reg(Imm64 imm, int r1)
{
        /*
        if (!(r1 & ~7) && is_imm32(imm)) {
                emit8(SECTION_CODE, 0xb8 | r1);
                emit32(SECTION_CODE, (Imm32) imm);
        }
        else */ {
                emit_rex_instruction_imm64_opreg(0xB8, imm, r1);
        }
}

/* Like emit_mov_64_imm_reg(), but always use the 8-byte version and emit
 * a relocation */
INTERNAL void emit_mov_64_reloc_reg(Symbol symbol, int addend, int r1)
{
        emit_rex_instruction_imm64_opreg(0xB8, 0, r1);

        /* the last 8 byte of the instruction we just encoded are the imm64 */
        int codepos = codeSectionCnt - 8;
        emit_relative_relocation(symbol, addend, codepos);
}

INTERNAL void emit_mov_64_imm_stack(Imm64 imm, X64StackLoc loc)
{
        /* 64-bit immediate loads to stack variable only via imm64
         * register load */
        emit_mov_64_imm_reg(imm, X64_RAX);
        emit_mov_64_reg_indirect(X64_RAX, X64_RBP, loc);
}

INTERNAL void emit_mov_64_reloc_stack(int sectionKind, int sectionPos, X64StackLoc loc)
{
        emit_mov_64_imm64_reg(0 /*relocation */, X64_RAX);
        int codePos = codeSectionCnt - 8;  // XXX
        emit_mov_64_reg_indirect(X64_RAX, X64_RBP, loc);
        emit_section_relative_relocation(sectionKind, sectionPos, codePos);
}

INTERNAL void emit_mov_size_indirect_reg(int sz, int r1, int r2, long d)
{
        if (sz == 1) emit_mov_8_indirect_reg(r1, r2, d);
        else if (sz == 4) emit_mov_32_indirect_reg(r1, r2, d);
        else if (sz == 8) emit_mov_64_indirect_reg(r1, r2, d);
        else UNHANDLED_CASE();
}

INTERNAL void emit_mov_size_reg_indirect(int sz, int r1, int r2, long d)
{
        if (sz == 1) emit_mov_8_reg_indirect(r1, r2, d);
        else if (sz == 4) emit_mov_32_reg_indirect(r1, r2, d);
        else if (sz == 8) emit_mov_64_reg_indirect(r1, r2, d);
        else UNHANDLED_CASE();
}

INTERNAL void emit_load_symaddr_stack(Symbol symbol, X64StackLoc loc)
{
        int r1 = X64_RAX;
        ASSERT(scopeInfo[symbolInfo[symbol].scope].scopeKind == SCOPE_GLOBAL);
        emit_mov_64_reloc_reg(symbol, 0, r1);
        emit_mov_64_reg_indirect(r1, X64_RBP, loc);
}

INTERNAL void emit_push_stack(IrReg UNUSED(reg), X64StackLoc loc)
{
        // assuming RAX is not used as argument
        //emit_mov_64_indirect_reg(X64_RBP, X64_RAX, loc);
        emit_push_64(X64_RAX);
}

INTERNAL void emit_local_jump(IrStmt tgtstmt)
{
        emit_mov_64_imm64_reg(0 /* relocation */, X64_RAX);
        int relocpos = codeSectionCnt - 8; //XXX

        emit_rex_instruction_reg(0xFF, 0x04, X64_RAX);

        int x = gotoCnt++;
        RESIZE_GLOBAL_BUFFER(gotoInfo, gotoCnt);
        gotoInfo[x].codepos = relocpos;
        gotoInfo[x].tgtstmt = tgtstmt;
}

INTERNAL void emit_local_conditional_jump(X64StackLoc condloc, IrStmt tgtstmt, int isNeg)
{
        emit_mov_64_indirect_reg(X64_RBP, X64_RAX, condloc);
        // CMP AL, imm8
        emit8(SECTION_CODE, 0x3c);
        emit8(SECTION_CODE, 0x00);

        if (isNeg)
                emit8(SECTION_CODE, 0x75); // JNZ
        else
                emit8(SECTION_CODE, 0x74); // JZ
        int oldpos = codeSectionCnt;
        emit8(SECTION_CODE, 0); // fix later
        emit_local_jump(tgtstmt);
        int jumpLength = codeSectionCnt - oldpos - 1;
        ASSERT((uint8_t) jumpLength == jumpLength);
        codeSection[oldpos] = (uint8_t) jumpLength; // this is the fix
}

INTERNAL void emit_function_prologue(IrProc irp)
{
        emit_push_64(X64_RBP);
        emit_mov_64_reg_reg(X64_RSP, X64_RBP);
        int size = compute_size_of_stack_frame(irp);
        size += 8;
        size += 15;
        size -= (size) % 16;
        size -= 8;
        emit_add_64_imm32_reg(-size, X64_RSP);
        emit_push_64(X64_RBX);
}

INTERNAL void emit_function_epilogue(void)
{
        emit_pop_64(X64_RBX);
        // not needed because the leave instruction does that
        // emit_mov_64_indirect_reg(X64_RBP, X64_RBX, -8);

        emit8(SECTION_CODE, 0xc9);  // leave
        emit8(SECTION_CODE, 0xc3);  // ret
}

INTERNAL void x64asm_loadconstant_irstmt(IrStmt irs)
{
        switch (irStmtInfo[irs].tLoadConstant.irConstantKind) {
        case IRCONSTANT_INTEGER: {
                Imm64 v = irStmtInfo[irs].tLoadConstant.tInteger;
                IrReg irreg = irStmtInfo[irs].tLoadConstant.tgtreg;
                X64StackLoc loc = find_stack_loc(irreg);
                emit_mov_64_imm_stack(v, loc);
                break;
        }
        case IRCONSTANT_FLOAT: {
                float v = irStmtInfo[irs].tLoadConstant.tFloat;
                X64Float xv = float_to_X64Float(v);
                IrReg irreg = irStmtInfo[irs].tLoadConstant.tgtreg;
                X64StackLoc loc = find_stack_loc(irreg);

                //XXX
                Type tp = irRegInfo[irreg].tp;
                if (type_equal(tp, builtinType[BUILTINTYPE_FLOAT]))
                        emit_mov_32_imm32_indirect(x64Float_to_imm32(xv), X64_RBP, loc);
                else if (type_equal(tp, builtinType[BUILTINTYPE_DOUBLE])) {
                        emit_mov_64_imm64_reg(x64Float_to_imm64(xv), X64_RAX);
                        emit_mov_64_reg_indirect(X64_RAX, X64_RBP, loc);
                }
                break;
        }
        case IRCONSTANT_STRING: {
                String s = irStmtInfo[irs].tLoadConstant.tString;
                IrReg irreg = irStmtInfo[irs].tLoadConstant.tgtreg;
                X64StackLoc loc = find_stack_loc(irreg);
                int rodataPos = rodataSectionCnt;
                emit_bytes(SECTION_RODATA, string_buffer(s), string_length(s) + 1);
                emit_mov_64_reloc_stack(SECTION_RODATA, rodataPos, loc);
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL void x64asm_loadsymboladdr_irstmt(IrStmt irs)
{
        Symbol sym = irStmtInfo[irs].tLoadSymbolAddr.sym;
        ASSERT(scopeInfo[symbolInfo[sym].scope].scopeKind == SCOPE_GLOBAL);
        IrReg irreg = irStmtInfo[irs].tLoadSymbolAddr.tgtreg;
        X64StackLoc loc = find_stack_loc(irreg);
        emit_load_symaddr_stack(sym, loc);
}

INTERNAL void x64asm_loadregaddr_irstmt(IrStmt irs)
{
        IrReg reg = irStmtInfo[irs].tLoadRegAddr.reg;
        IrReg tgtreg = irStmtInfo[irs].tLoadRegAddr.tgtreg;
        X64StackLoc loc = find_stack_loc(reg);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        emit_mov_64_reg_reg(X64_RBP, X64_RAX);
        emit_add_64_imm32_reg(loc, X64_RAX);
        emit_mov_64_reg_indirect(X64_RAX, X64_RBP, tgtloc);
}

INTERNAL void x64asm_load_irstmt(IrStmt irs)
{
        IrReg addrreg = irStmtInfo[irs].tLoad.srcaddrreg;
        IrReg tgtreg = irStmtInfo[irs].tLoad.tgtreg;
        X64StackLoc addrloc = find_stack_loc(addrreg);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        int size = get_type_size(irRegInfo[tgtreg].tp);
        emit_mov_64_indirect_reg(X64_RBP, X64_RAX, addrloc);
        emit_mov_size_indirect_reg(size, X64_RAX, X64_RAX, 0);
        emit_mov_size_reg_indirect(size, X64_RAX, X64_RBP, tgtloc);
}

INTERNAL void x64asm_store_irstmt(IrStmt irs)
{
        IrReg srcreg = irStmtInfo[irs].tStore.srcreg;
        IrReg addrreg = irStmtInfo[irs].tStore.tgtaddrreg;
        X64StackLoc srcloc = find_stack_loc(srcreg);
        X64StackLoc addrloc = find_stack_loc(addrreg);
        int size = get_type_size(irRegInfo[srcreg].tp);
        emit_mov_size_indirect_reg(size, X64_RBP, X64_RAX, srcloc);
        emit_mov_64_indirect_reg(X64_RBP, X64_RCX, addrloc);
        emit_mov_size_reg_indirect(size, X64_RAX, X64_RCX, 0);
}

INTERNAL void x64asm_regreg_irstmt(IrStmt irs)
{
        IrReg srcreg = irStmtInfo[irs].tRegreg.srcreg;
        IrReg tgtreg = irStmtInfo[irs].tRegreg.tgtreg;
        X64StackLoc srcloc = find_stack_loc(srcreg);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        int size = get_type_size(irRegInfo[srcreg].tp);
        emit_mov_size_indirect_reg(size, X64_RBP, X64_RAX, srcloc);
        emit_mov_size_reg_indirect(size, X64_RAX, X64_RBP, tgtloc);
}

INTERNAL void x64asm_op1_irstmt(IrStmt irs)
{
        IrReg reg = irStmtInfo[irs].tOp1.reg;
        IrReg tgtreg = irStmtInfo[irs].tOp1.tgtreg;
        X64StackLoc loc = find_stack_loc(reg);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        emit_mov_64_indirect_reg(X64_RBP, X64_RAX, loc);
        switch (irStmtInfo[irs].tOp1.irOp1Kind) {
        case IROP1_INC: emit_inc_64_reg(X64_RAX); break;
        case IROP1_DEC: emit_dec_64_reg(X64_RAX); break;
        case IROP1_BITWISENOT: emit_bitwisenot_64_reg(X64_RAX); break;
        default: UNHANDLED_CASE();
        }
        emit_mov_64_reg_indirect(X64_RAX, X64_RBP, tgtloc);
}

INTERNAL void x64asm_op2_irstmt_int(IrStmt irs)
{
        IrReg reg1 = irStmtInfo[irs].tOp2.reg1;
        IrReg reg2 = irStmtInfo[irs].tOp2.reg2;
        IrReg tgtreg = irStmtInfo[irs].tOp2.tgtreg;
        X64StackLoc loc1 = find_stack_loc(reg1);
        X64StackLoc loc2 = find_stack_loc(reg2);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);

        emit_mov_64_indirect_reg(X64_RBP, X64_RAX, loc1);
        emit_mov_64_indirect_reg(X64_RBP, X64_RBX, loc2);
        switch (irStmtInfo[irs].tOp2.irOp2Kind) {
        case IROP2_ADD: emit_add_64_reg_reg(X64_RBX, X64_RAX); break;
        case IROP2_SUB: emit_sub_64_reg_reg(X64_RAX, X64_RBX); break;
        case IROP2_MUL: emit_mul_64_rax_reg(X64_RBX); break;
        case IROP2_DIV:
                // clear rdx before div, with xor %rdx, %rdx
                emit8(SECTION_CODE, 0x48); emit8(SECTION_CODE, 0x31); emit8(SECTION_CODE, 0xD2);
                emit_div_64(X64_RBX);
                break;
        case IROP2_BITAND: emit_bitand_64_reg_reg(X64_RAX, X64_RBX); break;
        case IROP2_BITOR: emit_bitor_64_reg_reg(X64_RAX, X64_RBX); break;
        case IROP2_BITXOR: emit_bitxor_64_reg_reg(X64_RAX, X64_RBX); break;
        default: UNHANDLED_CASE();
        }
        emit_mov_64_reg_indirect(X64_RAX, X64_RBP, tgtloc);
}

INTERNAL void x64asm_op2_irstmt_float(IrStmt irs)
{
        IrReg reg1 = irStmtInfo[irs].tOp2.reg1;
        IrReg reg2 = irStmtInfo[irs].tOp2.reg2;
        IrReg tgtreg = irStmtInfo[irs].tOp2.tgtreg;
        X64StackLoc loc1 = find_stack_loc(reg1);
        X64StackLoc loc2 = find_stack_loc(reg2);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);

        emit_movss_indirect_xmm(X64_RBP, XMM0, loc1);
        emit_movss_indirect_xmm(X64_RBP, XMM1, loc2);
        switch (irStmtInfo[irs].tOp2.irOp2Kind) {
        case IROP2_ADD: emit_addss_xmm_xmm(XMM1, XMM0); break;
        case IROP2_SUB: emit_subss_xmm_xmm(XMM1, XMM0); break;
        case IROP2_MUL: emit_mulss_xmm_xmm(XMM1, XMM0); break;
        case IROP2_DIV: emit_divss_xmm_xmm(XMM1, XMM0); break;
        case IROP2_BITAND: /*TODO*/
        case IROP2_BITOR: /*TODO*/
        case IROP2_BITXOR: /*TODO*/
        default: UNHANDLED_CASE();
        }
        emit_movss_xmm_indirect(XMM0, X64_RBP, tgtloc);
}

INTERNAL void x64asm_op2_irstmt_double(IrStmt irs)
{
        IrReg reg1 = irStmtInfo[irs].tOp2.reg1;
        IrReg reg2 = irStmtInfo[irs].tOp2.reg2;
        IrReg tgtreg = irStmtInfo[irs].tOp2.tgtreg;
        X64StackLoc loc1 = find_stack_loc(reg1);
        X64StackLoc loc2 = find_stack_loc(reg2);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);

        emit_movsd_indirect_xmm(X64_RBP, XMM0, loc1);
        emit_movsd_indirect_xmm(X64_RBP, XMM1, loc2);
        switch (irStmtInfo[irs].tOp2.irOp2Kind) {
        case IROP2_ADD: emit_addsd_xmm_xmm(XMM1, XMM0); break;
        case IROP2_SUB: emit_subsd_xmm_xmm(XMM1, XMM0); break;
        case IROP2_MUL: emit_mulsd_xmm_xmm(XMM1, XMM0); break;
        case IROP2_DIV: emit_divsd_xmm_xmm(XMM1, XMM0); break;
        case IROP2_BITAND: /*TODO*/
        case IROP2_BITOR: /*TODO*/
        case IROP2_BITXOR: /*TODO*/
        default: UNHANDLED_CASE();
        }
        emit_movsd_xmm_indirect(XMM0, X64_RBP, tgtloc);
}

INTERNAL void x64asm_op2_irstmt(IrStmt irs)
{
        IrReg reg1 = irStmtInfo[irs].tOp2.reg1;
        IrReg reg2 = irStmtInfo[irs].tOp2.reg2;

        Type t1 = referenced_type(irRegInfo[reg1].tp);
        Type t2 = referenced_type(irRegInfo[reg2].tp);

        if ((t1 == builtinType[BUILTINTYPE_INT]
                /* for pointer subscripting */
                || typeInfo[t1].typeKind == TYPE_POINTER)
                && t2 == builtinType[BUILTINTYPE_INT])
                x64asm_op2_irstmt_int(irs);
        else if (t1 == builtinType[BUILTINTYPE_FLOAT])
                x64asm_op2_irstmt_float(irs);
        else if (t1 == builtinType[BUILTINTYPE_DOUBLE])
                x64asm_op2_irstmt_double(irs);
        else
                UNHANDLED_CASE();
}

INTERNAL void x64asm_cmp_irstmt(IrStmt irs)
{
        IrReg reg1 = irStmtInfo[irs].tOp2.reg1;
        IrReg reg2 = irStmtInfo[irs].tOp2.reg2;
        IrReg tgtreg = irStmtInfo[irs].tOp2.tgtreg;
        X64StackLoc loc1 = find_stack_loc(reg1);
        X64StackLoc loc2 = find_stack_loc(reg2);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        emit_mov_64_indirect_reg(X64_RBP, X64_RAX, loc1);
        emit_mov_64_indirect_reg(X64_RBP, X64_RBX, loc2);
        /*
         * We use ecx here instead of eax as the target. That's
         * because we need to clear the target register before
         * calling setcc. (Setcc only sets the lower 8 bits of
         * the target register). And since clearing itself sets
         * the CPU status flags, we need to clear *before*
         * comparing. Which means we need to use a different
         * register!
         */
        emit8(SECTION_CODE, 0x31); emit8(SECTION_CODE, 0xC9); // xor %ecx, %ecx
        emit_cmp_64_reg_reg(X64_RAX, X64_RBX);
        switch (irStmtInfo[irs].tCmp.irCmpKind) {
        case IRCMP_LT: emit_setcc(X64CMP_LT, X64_RCX); break;
        case IRCMP_GT: emit_setcc(X64CMP_GT, X64_RCX); break;
        case IRCMP_LE: emit_setcc(X64CMP_LE, X64_RCX); break;
        case IRCMP_GE: emit_setcc(X64CMP_GE, X64_RCX); break;
        case IRCMP_EQ: emit_setcc(X64CMP_EQ, X64_RCX); break;
        case IRCMP_NE: emit_setcc(X64CMP_NE, X64_RCX); break;
        default: UNHANDLED_CASE();
        }
        emit_mov_64_reg_indirect(X64_RCX, X64_RBP, tgtloc);
}

INTERNAL void x64asm_call_irstmt(IrStmt irs)
{
        IrReg calleeReg = irStmtInfo[irs].tCall.calleeReg;
        X64StackLoc calleeloc = find_stack_loc(calleeReg);
        IrCallArg firstArg = irStmtInfo[irs].tCall.firstIrCallArg;

        IrCallArg lastArg = firstArg;
        while (lastArg < irCallArgCnt && irCallArgInfo[lastArg].callStmt == irs)
                lastArg++;

        struct {
                IrReg reg;
                X64StackLoc loc;
        } stackArgs[32];
        int numStackArgs = 0;

        struct CCState ccstate;
        reset_ccstate(&ccstate);

        for (int a = firstArg; a < lastArg; a++) {
                if (!(a < irCallArgCnt && irCallArgInfo[a].callStmt == irs))
                        break;
                IrReg r = irCallArgInfo[a].srcreg;
                X64StackLoc loc = find_stack_loc(r);
                // move to appropriate register
                // XXX: this is of course ad-hoc and wrong. For
                // instance, we do not even check if that
                // register is free
                int passingKind;
                int regbits;
                ccstate_add_param(&ccstate, irRegInfo[r].tp, &passingKind, &regbits);
                switch (passingKind) {
                case PASSING_SINGLEPRECISION: emit_movss_indirect_xmm(X64_RBP, regbits, loc); break;
                case PASSING_DOUBLEPRECISION: emit_movsd_indirect_xmm(X64_RBP, regbits, loc); break;
                case PASSING_X64REG:          emit_mov_64_indirect_reg(X64_RBP, regbits, loc); break;
                case PASSING_STACK:
                        ASSERT(numStackArgs < LENGTH(stackArgs));
                        stackArgs[numStackArgs].reg = r;
                        stackArgs[numStackArgs].loc = loc;
                        numStackArgs++;
                        break;
                default: UNHANDLED_CASE();
                }
        }

        // push stack arguments in reverse order
        while (numStackArgs > 0) {
                IrReg reg = stackArgs[numStackArgs].reg;
                X64StackLoc loc = stackArgs[numStackArgs].loc;
                --numStackArgs;
                emit_push_stack(reg, loc);
        }

        emit_mov_64_indirect_reg(X64_RBP, X64_R11, calleeloc);
        emit_call_reg(X64_R11);
        IrCallResult cr0 = irStmtInfo[irs].tCall.firstIrCallResult;
        IrReg rreg = irCallResultInfo[cr0].tgtreg;
        ASSERT(rreg != (IrReg) -1); // is this true? what about void returns? How to handle multiple return values later?
        /* FIXME: if the return value is void then the stack location is the
        * same as previous register's stack location. That's not a problem in
        * itself, but since we currently *always* move 8 bytes, ignoring the
        * types real size, that results in overwriting the previous register's
        * data. So for now we make this conditional */
        if (get_type_size(irRegInfo[rreg].tp) > 0) {
                X64StackLoc rloc = find_stack_loc(rreg);
                emit_mov_64_reg_indirect(X64_RAX, X64_RBP, rloc);
        }
}

INTERNAL void x64asm_condgoto_irstmt(IrStmt irs)
{
        IrReg condreg = irStmtInfo[irs].tCondGoto.condreg;
        X64StackLoc condloc = find_stack_loc(condreg);
        IrStmt tgtstmt = irStmtInfo[irs].tCondGoto.tgtstmt;
        int isNeg = irStmtInfo[irs].tCondGoto.isNeg;
        emit_local_conditional_jump(condloc, tgtstmt, isNeg);
}

INTERNAL void x64asm_goto_irstmt(IrStmt irs)
{
        IrStmt tgtstmt = irStmtInfo[irs].tGoto.tgtstmt;
        emit_local_jump(tgtstmt);
}

INTERNAL void x64asm_return_irstmt(IrStmt irs)
{
        IrReturnval result = irStmtInfo[irs].tReturn.firstResult;
        if (result != -1) {
                IrReg irreg = irReturnvalInfo[result].resultReg;
                X64StackLoc loc = find_stack_loc(irreg);
                emit_mov_64_indirect_reg(X64_RBP, X64_RAX, loc);
        }
        emit_function_epilogue();
}

INTERNAL void (*irStmtKindToX64asmHandler[NUM_IRSTMT_KINDS])(IrStmt irs) = {
        [IRSTMT_LOADCONSTANT]    = x64asm_loadconstant_irstmt,
        [IRSTMT_LOADSYMBOLADDR]  = x64asm_loadsymboladdr_irstmt,
        [IRSTMT_LOADREGADDR]     = x64asm_loadregaddr_irstmt,
        [IRSTMT_LOAD]            = x64asm_load_irstmt,
        [IRSTMT_STORE]           = x64asm_store_irstmt,
        [IRSTMT_REGREG]          = x64asm_regreg_irstmt,
        [IRSTMT_OP1]             = x64asm_op1_irstmt,
        [IRSTMT_OP2]             = x64asm_op2_irstmt,
        [IRSTMT_CMP]             = x64asm_cmp_irstmt,
        [IRSTMT_CALL]            = x64asm_call_irstmt,
        [IRSTMT_CONDGOTO]        = x64asm_condgoto_irstmt,
        [IRSTMT_GOTO]            = x64asm_goto_irstmt,
        [IRSTMT_RETURN]          = x64asm_return_irstmt,
};

INTERNAL void x64asm_proc(IrProc irp)
{
        Symbol psym = irProcInfo[irp].symbol;
        ASSERT(symbolInfo[psym].symbolKind == SYMBOL_PROC);
        Type tp = symbolInfo[psym].tProc.tp;
        ASSERT(tp != -1);
        ASSERT(typeInfo[tp].typeKind == TYPE_PROC);

        begin_symbol(psym);
        emit_function_prologue(irp);

        {
                struct CCState ccstate;
                reset_ccstate(&ccstate);
                for (Param i = firstProctypeParam[tp];
                     i < paramCnt && paramInfo[i].proctp == tp;
                     i++) {
                        Symbol sym = paramInfo[i].sym;
                        ASSERT(scopeInfo[symbolInfo[sym].scope].scopeKind
                               == SCOPE_PROC);
                        ASSERT(symbolInfo[sym].symbolKind == SYMBOL_DATA);
                        Data data = symbolInfo[sym].tData.optionaldata;
                        ASSERT(data != -1);
                        IrReg reg = dataToIrReg[data];
                        X64StackLoc loc = find_stack_loc(reg);

                        int passingKind;
                        int regbits;
                        ccstate_add_param(&ccstate, irRegInfo[reg].tp,
                                          &passingKind, &regbits);
                        switch (passingKind) {
                        case PASSING_X64REG: emit_mov_64_reg_indirect(regbits, X64_RBP, loc); break;
                        case PASSING_SINGLEPRECISION: emit_movss_xmm_indirect(regbits, X64_RBP, loc); break;
                        case PASSING_DOUBLEPRECISION: emit_movsd_xmm_indirect(regbits, X64_RBP, loc); break;
                        case PASSING_STACK:
                                emit_pop_64(X64_RAX);
                                emit_mov_64_reg_indirect(X64_RAX, X64_RBP, loc);
                                break;
                        default: UNHANDLED_CASE();
                        }
                }
        }

        IrStmt irs = irProcInfo[irp].firstIrStmt;
        for (;
             irs < irStmtCnt && irStmtInfo[irs].proc == irp;
             irs++) {
                irstmtToCodepos[irs] = codeSectionCnt; // correct?
                int kind = irStmtInfo[irs].irStmtKind;
                ASSERT(0 <= kind && kind < NUM_IRSTMT_KINDS);
                irStmtKindToX64asmHandler [kind] (irs);
        }
        if (!(irs > 0 && irStmtInfo[irs-1].irStmtKind == IRSTMT_RETURN))
                emit_function_epilogue(); /* no type checking here */
        end_symbol();
}

void codegen_x64(void)
{
        RESIZE_GLOBAL_BUFFER(irstmtToCodepos, irStmtCnt);
        RESIZE_GLOBAL_BUFFER(irprocToCodepos, irProcCnt);

        /* TODO: do we need an "IrData" type? */
        for (Directive d = 0; d < directiveCnt; d++) {
                if (directiveInfo[d].directiveKind != BUILTINDIRECTIVE_DATA)
                        continue;
                Data data = directiveInfo[d].tData.data;
                Expr expr = directiveInfo[d].tData.optionalInitializerExpr;
                int size = get_type_size(dataInfo[data].tp);

                if (expr == (Expr) -1) {
                        int offset = zerodataSectionCnt;
                        zerodataSectionCnt += size;
                        emit_symdef(dataInfo[data].sym, SECTION_ZERODATA,
                                    offset, size);
                }
                else {
                        int offset = dataSectionCnt;

                        //XXX
                        if (exprInfo[expr].exprKind == EXPR_LITERAL &&
                            exprInfo[expr].tLiteral.literalKind == LITERAL_INTEGER) {
                                Token token = exprInfo[expr].tLiteral.tok;
                                emit64(SECTION_DATA, tokenInfo[token].tInteger.value);
                        }
                        else {
                                // XXX XXX XXX
                                *(int64_t *) &dataSection[offset] = 5;
                        }

                        emit_symdef(dataInfo[data].sym, SECTION_DATA,
                                    offset, size);
                }
        }

        for (IrProc x = 0; x < irProcCnt; x++) {
                DEBUG("Generating code for proc #%d %s\n",
                      x, SS(irProcInfo[x].symbol));
                irprocToCodepos[x] = codeSectionCnt;
                x64asm_proc(x);
        }

        for (int i = 0; i < gotoCnt; i++) {
                IrStmt irs = gotoInfo[i].tgtstmt;
                IrProc irp = irStmtInfo[irs].proc;
                int codepos = gotoInfo[i].codepos;
                emit_relative_relocation(
                        irProcInfo[irp].symbol,
                        irstmtToCodepos[irs] - irprocToCodepos[irp],
                        codepos);
        }
}
