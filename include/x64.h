#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif


enum {
        /* The order is important here: this values (0000...1111 binary) are
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


struct X64StackLocInfo {
        IrReg irreg;
        int offset;
};

DATA int x64StackLocCnt;
DATA struct X64StackLocInfo *x64StackLocInfo;
