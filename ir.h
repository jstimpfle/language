#ifndef API_H_INCLUDED
#error "api.h" must be included first
#endif

typedef int IrReg;
typedef int IrProc;
typedef int IrCall;
typedef int IrCallArg;
typedef int IrCallResult;
typedef int IrStmt;
typedef int IrLabel;

enum {
        IRSTMT_LOADCONSTANT,
        IRSTMT_LOADSYMBOLADDR,
        IRSTMT_LOAD,
        IRSTMT_STORE,
        IRSTMT_CALL,
        IRSTMT_CONDGOTO,
        IRSTMT_GOTO,
};

struct IrCallArgInfo {
        IrCall call;
        IrReg src;
};

struct IrCallResultInfo {
        IrCall call;
        IrReg tgt;
};

struct IrRegInfo {
        IrProc irproc;
        //String name;
        const char *name; //temporarily
        Type tp; //XXX
};

struct IrLoadConstantStmtInfo {
        long long constval;
        IrReg tgtreg;
};

struct IrLoadSymbolAddrStmtInfo {
        Symbol sym;
        IrReg tgtreg;
};

struct IrLoadStmtInfo {
        IrReg srcaddrreg;
        IrReg tgtreg;
};

struct IrStoreStmtInfo {
        IrReg srcreg;
        IrReg tgtaddrreg;
};

struct IrCallStmtInfo {
        IrReg callee;
        IrCallArg firstArg;  // speed-up
        IrCallResult firstResult;  // speed-up
};

struct IrCondGotoStmtInfo {
        IrReg condreg;
        IrStmt tgtstmt;
};

struct IrGotoStmtInfo {
        IrStmt tgtstmt;
};

struct IrStmtInfo {
        IrProc proc;
        int kind;  // IRSTMT_
        union {
                struct IrLoadConstantStmtInfo tLoadConstant;
                struct IrLoadSymbolAddrStmtInfo tLoadSymbolAddr;
                struct IrLoadStmtInfo tLoad;
                struct IrStoreStmtInfo tStore;
                struct IrCallStmtInfo tCall;
                struct IrCondGotoStmtInfo tCondGoto;
                struct IrGotoStmtInfo tGoto;
        };
};

struct IrLabelInfo {
        IrProc proc;
        String name;
};

struct IrProc {
        String name;
        IrStmt firstStmt; // speed-up
};
