#ifndef API_H_INCLUDED
#error "api.h" must be included first
#endif

typedef int IrVar;
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
        IrVar src;
};

struct IrCallResultInfo {
        IrCall call;
        IrVar tgt;
};

struct IrVarInfo {
        IrProc irproc;
        //String name;
        const char *name; //temporarily
        Type tp; //XXX
};

struct IrLoadConstantStmtInfo {
        long long constval;
        IrVar tgtvar;
};

struct IrLoadSymbolAddrStmtInfo {
        Symbol sym;
        IrVar tgtvar;
};

struct IrLoadStmtInfo {
        IrVar srcaddrvar;
        IrVar tgtvar;
};

struct IrStoreStmtInfo {
        IrVar srcvar;
        IrVar tgtaddrvar;
};

struct IrCallStmtInfo {
        IrVar callee;
        IrCallArg firstArg;  // speed-up
        IrCallResult firstResult;  // speed-up
};

struct IrCondGotoStmtInfo {
        IrVar condvar;
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
