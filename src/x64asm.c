/*
 * First try at register allocator and x64 assembler
 */

#include "defs.h"
#include "api.h"
#include <stdint.h>

typedef uint8_t Imm8;
typedef uint16_t Imm16;
typedef uint32_t Imm32;
typedef uint64_t Imm64;

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

enum {
        X64CMP_LT,
        X64CMP_GT,
        X64CMP_LE,
        X64CMP_GE,
        X64CMP_EQ,
        X64CMP_NE,
};

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

const int cc[] = { /* "calling convention" */
/* XXX: for now, a compile-time switch will do for chosing the calling
 * convention */
#ifdef _MSC_VER
        X64_RCX, X64_RDX, X64_R8, X64_R9,
#else
        X64_RDI, X64_RSI, X64_RDX,
        X64_RCX, X64_R8, X64_R9
#endif
};

INTERNAL
int find_stack_loc(IrReg irreg)
{
        int out = 0;
        for (IrReg reg = irreg;
             reg >= 0 && irRegInfo[reg].proc == irRegInfo[irreg].proc;
             reg--) {
                Type tp = irRegInfo[reg].tp;
                ASSERT(tp != (Type) -1);
                int size = get_type_size(tp);
                if (size <= 0)
                        MSG(lvl_error, "sizeof reg=%d proc=%d tp=%d is %d\n",
                            reg, irRegInfo[reg].proc, tp, size);
                ASSERT(size > 0);
                out += size;
        }
        return -out;
}

INTERNAL UNUSEDFUNC
int is_imm8(Imm64 imm)
{
        return imm < (1u << 8);
}

INTERNAL UNUSEDFUNC
int is_imm16(Imm64 imm)
{
        return imm < (1u << 16);
}

INTERNAL
int is_imm32(Imm64 imm)
{
        return imm < (1ull << 32);
}

/* TODO: The approach with begin_symbol() / end_symbol() is too complicated. We
 * should rather emit the symDefs for procs mostly independently of the machine
 * code generation for the procs, filling in only the size of the proc code
 * after the proc code is generated. */
INTERNAL
void begin_symbol(Symbol sym)
{
        SymDef sd = symDefCnt++;
        RESIZE_GLOBAL_BUFFER(symDefInfo, symDefCnt);
        symDefInfo[sd].symbol = sym;
        symDefInfo[sd].sectionKind = SECTION_CODE;
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
void emit_symbol(Symbol sym, int section, int offset, int size)
{
        ASSERT(symbolInfo[sym].symbolKind == SYMBOL_DATA);
        SymDef sd = symDefCnt++;
        RESIZE_GLOBAL_BUFFER(symDefInfo, symDefCnt);
        symDefInfo[sd].symbol = sym;
        symDefInfo[sd].sectionKind = section;
        symDefInfo[sd].offset = offset;
        symDefInfo[sd].size = size;
}

INTERNAL
void emit_rodata(const void *buf, int size)
{
        int pos = rodataSectionCnt;
        rodataSectionCnt += size;
        RESIZE_GLOBAL_BUFFER(rodataSection, rodataSectionCnt);
        copy_mem(rodataSection + pos, buf, size);
}

INTERNAL UNUSEDFUNC
void emit_data(const void *buf, int size)
{
        int pos = dataSectionCnt;
        dataSectionCnt += size;
        RESIZE_GLOBAL_BUFFER(dataSection, dataSectionCnt);
        copy_mem(dataSection + pos, buf, size);
}

INTERNAL
void emit_bytes(const void *buf, int size)
{
        int pos = codeSectionCnt;
        codeSectionCnt += size;
        RESIZE_GLOBAL_BUFFER(codeSection, codeSectionCnt);
        copy_mem(codeSection + pos, buf, size);
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
        unsigned char bs[] = { BYTE(c, 0), BYTE(c, 1) };
        emit_bytes(bs, LENGTH(bs));
}

void emit32(uint32_t c)
{
        unsigned char bs[] = {
                BYTE(c, 0), BYTE(c, 1), BYTE(c, 2), BYTE(c, 3)
        };
        emit_bytes(bs, LENGTH(bs));
}

void emit64(uint64_t c)
{
        unsigned char bs[] = {
                BYTE(c, 0), BYTE(c, 1), BYTE(c, 2), BYTE(c, 3),
                BYTE(c, 4), BYTE(c, 5), BYTE(c, 6), BYTE(c, 7),
        };
        emit_bytes(bs, LENGTH(bs));
}

void emit_section_relative_relocation(int sectionKind, int sectionPos,
                                      int codePos)
{
        int x = relocCnt++;
        RESIZE_GLOBAL_BUFFER(relocInfo, relocCnt);
        relocInfo[x].symbol = -1;
        relocInfo[x].sectionKind = sectionKind;
        relocInfo[x].addend = sectionPos;
        relocInfo[x].offset = codePos;
}

INTERNAL
int make_modrm_byte(unsigned mod, unsigned reg, unsigned rm)
{
        ASSERT(mod < 4);
        ASSERT(reg < 8);
        ASSERT(rm < 8);
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
        ASSERT(scale < 4);
        ASSERT(r1 < 8);
        ASSERT(r2 < 8);
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
INTERNAL
void emit_modrmreg_and_displacement_bytes(int r1, int r2, long d)
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
                ASSERT(-(1LL << 31) <= d);
                ASSERT(d < (1LL << 31));
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

INTERNAL UNUSEDFUNC
void emit_add_64_reg_indirect(int r1, int r2, long d)
{
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|R|B);
        emit(0x01);
        emit_modrmreg_and_displacement_bytes(r1, r2, d);
}

INTERNAL UNUSEDFUNC
void emit_add_64_indirect_reg(int r1, int r2, long d)
{
        int R = (r2 & ~7) ? REX_R : 0;
        int B = (r1 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|R|B);
        emit(0x01 | 0x02);
        emit_modrmreg_and_displacement_bytes(r2, r1, d);
}

INTERNAL UNUSEDFUNC
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
#if 0  /* disable optimization: seems to operate only on r32 registers */
        if (reg == X64_RAX) {
                /* optimization */
                emit(0x05);
                emit32(imm);
                return;
        }
#endif
        int B = (reg & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|B);
        emit(0x81);
        emit(make_modrm_byte(0x03, 0x00, reg & 7));
        emit32(imm);
}

INTERNAL
void emit_sub_64_reg_reg(int r1, int r2)
{
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|R|B);
        emit(0x2B);
        emit(make_modrm_byte(0x03, r1 & 7, r2 & 7));
}

/* Multiply RAX with given register. Result is in RDX:RAX */
INTERNAL
void emit_mul_64(int r)
{
        int B = (r & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|B);
        emit(0xF7);
        emit(make_modrm_byte(0x03, 0x04, r & 7));
}

INTERNAL
void emit_div_64(int r)
{
        int B = (r & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|B);
        emit(0xF7);
        emit(make_modrm_byte(0x03, 0x06, r & 7));
}

INTERNAL
void emit_cmp_64_reg_reg(int r1, int r2)
{
        int R = (r1 & ~7) ? REX_R : 0;
        int B = (r2 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|R|B);
        emit(0x3B);
        emit(make_modrm_byte(0x03, r1 & 7, r2 & 7));
}

INTERNAL
void emit_inc_64_reg(int r1)
{
        int R = (r1 & ~7) ? REX_R : 0;
        emit(REX_BASE|REX_W|R);
        emit(0xFF);
        emit(make_modrm_byte(0x03, 0x00, r1 & 7));
}

INTERNAL
void emit_dec_64_reg(int r1)
{
        int R = (r1 & ~7) ? REX_R : 0;
        emit(REX_BASE|REX_W|R);
        emit(0xFE);
        emit(make_modrm_byte(0x03, 0x01, r1 & 7));
}

void emit_setcc(int x64CmpKind, int r)
{
        int B = (r & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|B);
        emit(0x0F);
        switch (x64CmpKind) {
        case X64CMP_LT: emit(0x9C); break;
        case X64CMP_GT: emit(0x9F); break;
        case X64CMP_LE: emit(0x9E); break;
        case X64CMP_GE: emit(0x9D); break;
        case X64CMP_EQ: emit(0x94); break;
        case X64CMP_NE: emit(0x95); break;
        default: UNHANDLED_CASE();
        }
        emit(make_modrm_byte(0x03, 0x00, r & 7));
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
INTERNAL UNUSEDFUNC
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
void emit_mov_64_reloc_reg(Symbol symbol, int addend, int r1)
{
        int sectionKind = SECTION_CODE; /* Note: the sectionKind means the
                                           section where the relocation (change)
                                           is made, not where the symbol that is
                                           referenced lives. For now, we only
                                           have relocations in the code section
                                           */
        int B = (r1 & ~7) ? REX_B : 0;
        emit(REX_BASE|REX_W|B);
        emit(0xb8 | r1);
        {
                int offset = codeSectionCnt;
                Reloc reloc = relocCnt++;
                RESIZE_GLOBAL_BUFFER(relocInfo, relocCnt);
                relocInfo[reloc].symbol = symbol;
                relocInfo[reloc].sectionKind = sectionKind;
                relocInfo[reloc].addend = addend;
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
void emit_load_relocatedConstant_stack(
        int sectionKind, int sectionPos, X64StackLoc loc)
{
        emit_mov_64_address_reg(0 /*relocation */, X64_RAX);
        int codePos = codeSectionCnt - 8;  // XXX
        emit_mov_64_reg_stack(X64_RAX, loc);
        emit_section_relative_relocation(sectionKind, sectionPos, codePos);
}

INTERNAL
void emit_load_symaddr_stack(Symbol symbol, X64StackLoc loc)
{
        int r1 = X64_RAX;
        ASSERT(scopeInfo[symbolInfo[symbol].scope].scopeKind == SCOPE_GLOBAL);
        emit_mov_64_reloc_reg(symbol, 0, r1);
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
void emit_local_jump(IrStmt tgtstmt)
{
        emit_mov_64_address_reg(0 /* relocation */, X64_RAX);
        int relocpos = codeSectionCnt - 8; //XXX

        // FF /4, Jump
        emit(0xFF);
        emit(make_modrm_byte(0x03 /* register value */, 0x04, X64_RAX));

        int x = gotoCnt++;
        RESIZE_GLOBAL_BUFFER(gotoInfo, gotoCnt);
        gotoInfo[x].offset = relocpos;
        gotoInfo[x].tgtstmt = tgtstmt;
}

INTERNAL
void emit_local_conditional_jump(X64StackLoc condloc, IrStmt tgtstmt, int isNeg)
{
        emit_mov_64_stack_reg(condloc, X64_RAX);
        // CMP AL, imm8
        emit(0x3c);
        emit(0x00);

        if (isNeg)
                emit(0x75); // JNZ
        else
                emit(0x74); // JZ
        int oldpos = codeSectionCnt;
        emit(-1); // fix later
        emit_local_jump(tgtstmt);
        codeSection[oldpos] = codeSectionCnt - oldpos - 1; // this is the fix
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
void x64asm_loadconstant_irstmt(IrStmt irs)
{
        switch (irStmtInfo[irs].tLoadConstant.irConstantKind) {
        case IRCONSTANT_INTEGER: {
                Imm64 v = irStmtInfo[irs].tLoadConstant.tInteger;
                IrReg irreg = irStmtInfo[irs].tLoadConstant.tgtreg;
                X64StackLoc loc = find_stack_loc(irreg);
                emit_load_constant_stack(v, loc);
                break;
        }
        case IRCONSTANT_STRING: {
                String s = irStmtInfo[irs].tLoadConstant.tString;
                IrReg irreg = irStmtInfo[irs].tLoadConstant.tgtreg;
                X64StackLoc loc = find_stack_loc(irreg);
                int rodataPos = rodataSectionCnt;
                emit_rodata(string_buffer(s), string_length(s) + 1);
                emit_load_relocatedConstant_stack(SECTION_RODATA, rodataPos, loc);
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

INTERNAL
void x64asm_loadsymboladdr_irstmt(IrStmt irs)
{
        Symbol sym = irStmtInfo[irs].tLoadSymbolAddr.sym;
        ASSERT(scopeInfo[symbolInfo[sym].scope].scopeKind == SCOPE_GLOBAL);
        IrReg irreg = irStmtInfo[irs].tLoadSymbolAddr.tgtreg;
        X64StackLoc loc = find_stack_loc(irreg);
        emit_load_symaddr_stack(sym, loc);
}

INTERNAL
void x64asm_loadregaddr_irstmt(IrStmt irs)
{
        IrReg reg = irStmtInfo[irs].tLoadRegAddr.reg;
        IrReg tgtreg = irStmtInfo[irs].tLoadRegAddr.tgtreg;
        X64StackLoc loc = find_stack_loc(reg);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        emit_mov_64_reg_reg(X64_RBP, X64_RAX);
        emit_add_imm32_reg(loc, X64_RAX);
        emit_mov_64_reg_stack(X64_RAX, tgtloc);
}

INTERNAL
void x64asm_load_irstmt(IrStmt irs)
{
        IrReg addrreg = irStmtInfo[irs].tLoad.srcaddrreg;
        IrReg tgtreg = irStmtInfo[irs].tLoad.tgtreg;
        X64StackLoc srcloc = find_stack_loc(addrreg);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        emit_mov_64_stack_reg(srcloc, X64_RAX);
        emit_mov_64_indirect_reg(X64_RAX, X64_RAX, 0);
        emit_mov_64_reg_stack(X64_RAX, tgtloc);
}

INTERNAL
void x64asm_store_irstmt(IrStmt irs)
{
        IrReg addrreg = irStmtInfo[irs].tStore.srcreg;
        IrReg tgtreg = irStmtInfo[irs].tStore.tgtaddrreg;
        X64StackLoc srcloc = find_stack_loc(addrreg);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        emit_mov_64_stack_reg(srcloc, X64_RAX);
        emit_mov_64_stack_reg(tgtloc, X64_RCX);
        emit_mov_64_reg_indirect(X64_RAX, X64_RCX, 0);
}

INTERNAL
void x64asm_regreg_irstmt(IrStmt irs)
{
        IrReg srcreg = irStmtInfo[irs].tRegreg.srcreg;
        IrReg tgtreg = irStmtInfo[irs].tRegreg.tgtreg;
        X64StackLoc srcloc = find_stack_loc(srcreg);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        emit_mov_64_stack_reg(srcloc, X64_RAX);
        emit_mov_64_reg_stack(X64_RAX, tgtloc);
}

INTERNAL
void x64asm_op1_irstmt(IrStmt irs)
{
        IrReg reg = irStmtInfo[irs].tOp1.reg;
        IrReg tgtreg = irStmtInfo[irs].tOp1.tgtreg;
        X64StackLoc loc = find_stack_loc(reg);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        emit_mov_64_stack_reg(loc, X64_RAX);
        switch (irStmtInfo[irs].tOp1.irOp1Kind) {
        case IROP1_INC: emit_inc_64_reg(X64_RAX); break;
        case IROP1_DEC: emit_dec_64_reg(X64_RAX); break;
        default: UNHANDLED_CASE();
        }
        emit_mov_64_reg_stack(X64_RAX, tgtloc);
}

INTERNAL
void x64asm_op2_irstmt(IrStmt irs)
{
        IrReg reg1 = irStmtInfo[irs].tOp2.reg1;
        IrReg reg2 = irStmtInfo[irs].tOp2.reg2;
        IrReg tgtreg = irStmtInfo[irs].tOp2.tgtreg;
        X64StackLoc loc1 = find_stack_loc(reg1);
        X64StackLoc loc2 = find_stack_loc(reg2);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        emit_mov_64_stack_reg(loc1, X64_RAX);
        emit_mov_64_stack_reg(loc2, X64_RBX);
        switch (irStmtInfo[irs].tOp2.irOp2Kind) {
        case IROP2_ADD:
                emit_add_64_reg_reg(X64_RBX, X64_RAX);
                break;
        case IROP2_SUB:
                emit_sub_64_reg_reg(X64_RAX, X64_RBX);
                break;
        case IROP2_MUL:
                emit_mul_64(X64_RBX);
                break;
        case IROP2_DIV:
                // clear rdx before div, with xor %rdx, %rdx
                emit(0x48); emit(0x31); emit(0xD2);
                emit_div_64(X64_RBX);
                break;
        default:
                UNHANDLED_CASE();
        }
        emit_mov_64_reg_stack(X64_RAX, tgtloc);
}

INTERNAL
void x64asm_cmp_irstmt(IrStmt irs)
{
        IrReg reg1 = irStmtInfo[irs].tOp2.reg1;
        IrReg reg2 = irStmtInfo[irs].tOp2.reg2;
        IrReg tgtreg = irStmtInfo[irs].tOp2.tgtreg;
        X64StackLoc loc1 = find_stack_loc(reg1);
        X64StackLoc loc2 = find_stack_loc(reg2);
        X64StackLoc tgtloc = find_stack_loc(tgtreg);
        emit_mov_64_stack_reg(loc1, X64_RAX);
        emit_mov_64_stack_reg(loc2, X64_RBX);
        /*
         * We use ecx here instead of eax as the target. That's
         * because we need to clear the target register before
         * calling setcc. (Setcc only sets the lower 8 bits of
         * the target register). And since clearing itself sets
         * the CPU status flags, we need to clear *before*
         * comparing. Which means we need to use a different
         * register!
         */
        emit(0x31); emit(0xC9); // xor %ecx, %ecx
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
        emit_mov_64_reg_stack(X64_RCX, tgtloc);
}

INTERNAL
void x64asm_call_irstmt(IrStmt irs)
{
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
}

INTERNAL
void x64asm_condgoto_irstmt(IrStmt irs)
{
        IrReg condreg = irStmtInfo[irs].tCondGoto.condreg;
        X64StackLoc condloc = find_stack_loc(condreg);
        IrStmt tgtstmt = irStmtInfo[irs].tCondGoto.tgtstmt;
        int isNeg = irStmtInfo[irs].tCondGoto.isNeg;
        emit_local_conditional_jump(condloc, tgtstmt, isNeg);
}

INTERNAL
void x64asm_goto_irstmt(IrStmt irs)
{
        IrStmt tgtstmt = irStmtInfo[irs].tGoto.tgtstmt;
        emit_local_jump(tgtstmt);
}

INTERNAL
void x64asm_return_irstmt(IrStmt irs)
{
        IrReturnval result = irStmtInfo[irs].tReturn.firstResult;
        if (result != -1) {
                IrReg irreg = irReturnvalInfo[result].resultReg;
                X64StackLoc loc = find_stack_loc(irreg);
                emit_mov_64_stack_reg(loc, X64_RAX);
        }
        /* if it's not the last statement then emit an extra
         * return */
        if (irs+1 < irStmtCnt &&
            irStmtInfo[irs+1].proc == irStmtInfo[irs].proc) {
                emit_function_epilogue();
        }
}

INTERNAL void (*irStmtKindToX64asmHandler[NUM_IRSTMT_KINDS])(IrStmt irs) = {
#define MAKE(x, y) [x] = &y
        MAKE( IRSTMT_LOADCONSTANT,    x64asm_loadconstant_irstmt    ),
        MAKE( IRSTMT_LOADSYMBOLADDR,  x64asm_loadsymboladdr_irstmt  ),
        MAKE( IRSTMT_LOADREGADDR,     x64asm_loadregaddr_irstmt     ),
        MAKE( IRSTMT_LOAD,            x64asm_load_irstmt            ),
        MAKE( IRSTMT_STORE,           x64asm_store_irstmt           ),
        MAKE( IRSTMT_REGREG,          x64asm_regreg_irstmt          ),
        MAKE( IRSTMT_OP1,             x64asm_op1_irstmt             ),
        MAKE( IRSTMT_OP2,             x64asm_op2_irstmt             ),
        MAKE( IRSTMT_CMP,             x64asm_cmp_irstmt             ),
        MAKE( IRSTMT_CALL,            x64asm_call_irstmt            ),
        MAKE( IRSTMT_CONDGOTO,        x64asm_condgoto_irstmt        ),
        MAKE( IRSTMT_GOTO,            x64asm_goto_irstmt            ),
        MAKE( IRSTMT_RETURN,          x64asm_return_irstmt          ),
#undef MAKE
};

INTERNAL
void x64asm_proc(IrProc irp)
{
        Symbol psym = irProcInfo[irp].symbol;
        ASSERT(symbolInfo[psym].symbolKind == SYMBOL_PROC);
        Type tp = symbolInfo[psym].tProc.tp;
        ASSERT(tp != -1);
        ASSERT(typeInfo[tp].typeKind == TYPE_PROC);

        begin_symbol(psym);
        emit_function_prologue();

        {
                int j = 0;
                for (Param i = firstProctypeParam[tp];
                     i < paramCnt && paramInfo[i].proctp == tp;
                     i++, j++) {
                        Symbol sym = paramInfo[i].sym;
                        ASSERT(scopeInfo[symbolInfo[sym].scope].scopeKind
                               == SCOPE_PROC);
                        ASSERT(symbolInfo[sym].symbolKind == SYMBOL_DATA);
                        Data data = symbolInfo[sym].tData.optionaldata;
                        ASSERT(data != -1);
                        IrReg reg = dataToIrReg[data];
                        X64StackLoc loc = find_stack_loc(reg);
                        emit_mov_64_reg_stack(cc[j], loc);
                }
        }

        for (IrStmt irs = irProcInfo[irp].firstIrStmt;
             irs < irStmtCnt && irStmtInfo[irs].proc == irp;
             irs++) {
                irstmtToCodepos[irs] = codeSectionCnt; // correct?
                int kind = irStmtInfo[irs].irStmtKind;
                ASSERT(0 <= kind && kind < NUM_IRSTMT_KINDS);
                irStmtKindToX64asmHandler [kind] (irs);
        }
        emit_function_epilogue();
        end_symbol();
}

void codegen_x64(void)
{
        RESIZE_GLOBAL_BUFFER(irstmtToCodepos, irStmtCnt);
        RESIZE_GLOBAL_BUFFER(irprocToCodepos, irProcCnt);

        /* TODO: do we need an "IrData" type? */
        for (Data x = 0; x < dataCnt; x++) {
                if (scopeInfo[dataInfo[x].scope].scopeKind != SCOPE_GLOBAL)
                        continue;
                int size = get_type_size(dataInfo[x].tp);
                int offset = zerodataSectionCnt;
                zerodataSectionCnt += size;
                emit_symbol(dataInfo[x].sym, SECTION_DATA, size, offset);
        }

        for (IrProc x = 0; x < irProcCnt; x++) {
                DEBUG("Generating code for proc #%d\n", x);
                irprocToCodepos[x] = codeSectionCnt;
                x64asm_proc(x);
        }

        for (int i = 0; i < gotoCnt; i++) {
                IrStmt irs = gotoInfo[i].tgtstmt;
                int offset = gotoInfo[i].offset;
                IrProc irp = irStmtInfo[irs].proc;
                int x = relocCnt++;
                RESIZE_GLOBAL_BUFFER(relocInfo, relocCnt);
                relocInfo[x].symbol = irProcInfo[irp].symbol;
                relocInfo[x].sectionKind = SECTION_CODE;
                relocInfo[x].addend = irstmtToCodepos[irs]
                                                - irprocToCodepos[irp];
                relocInfo[x].offset = offset;
        }
}
