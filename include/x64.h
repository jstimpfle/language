#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif

typedef int X64StackLoc;
typedef float X64Float;  /*XXX*/

struct X64StackLocInfo {
        IrReg irreg;
        int offset;
};

DATA int x64StackLocCnt;
DATA struct X64StackLocInfo *x64StackLocInfo;
