#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif


/**
 * Intermediate Representation (IR). This is a lower-level representation of the
 * code than the syntactical one (which is usually called "abstract syntax tree"
 * or "AST"). The essential difference is that Ir code is a "flat"
 * representation. It consists only of sequential statements which is an
 * important step towards real machine code such as x64 machine code.
 *
 * \typedef{IrSymbol} TODO
 *
 * \typedef{IrProc} is the IR equivalent to the \typedef{Proc} from the AST
 * abstraction level. It has a number of parameters and results and has some
 * code (in the form of \typedef{IrStmt}'s).
 *
 * \typedef{IrStmt}. A computation step. Currently must be part of precisely one
 * \typedef{IrProc}.
 *
 * \typedef{IrReg}. An IR register is an approximation to a CPU register.
 * The difference is that we can have an unlimited number of them and they can
 * hold values of any type (TODO). One important part of a machine code
 * generator implementation is to map IR registers to a limited set of real CPU
 * registers.
 *
 * \typedef{IrCallArg} is an argument value for an IR statement of kind
 * IRSTMT_CALL. \typedef{IrCallResult} is a result value for an IR statement of
 * kind IRSTMT_CALL.
 *
 * \typedef{IrReturnval} is a value that is return by an IR statement of kind
 * IRSTMT_RETURN.
 *
 * \typedef{IrLabel} TODO
 */

typedef int IrSymbol;
typedef int IrProc;
typedef int IrReg;
typedef int IrStmt;
typedef int IrCallArg;
typedef int IrCallResult;
typedef int IrReturnval;
typedef int IrLabel;

/**
 * \enum{IrStmtKind} defines the possible kinds of IR statements.
 */

enum IrStmtKind {
        IRSTMT_LOADCONSTANT,
        IRSTMT_LOADSYMBOLADDR,
        IRSTMT_LOADREGADDR,
        IRSTMT_LOAD,
        IRSTMT_STORE,
        IRSTMT_REGREG,
        IRSTMT_CALL,
        IRSTMT_CONDGOTO,
        IRSTMT_GOTO,
        IRSTMT_RETURN,
};

/**
 */

enum IrLoadConstantKind {
        IRCONSTANT_INTEGER,
        IRCONSTANT_STRING,
};

/**
 * \struct{IrRegInfo} is a struct associating important data with a particular
 * IR register.
 *
 * \struct{IrStmtInfo} is how IR statements are represented. It has a union of
 * datatypes for all kinds of IR statements. What kind of statement is valid in
 * the union is designated by the "kind" field which holds a value of type
 * \enum{IrStmtKind}.
 *
 * \struct{IrCallArgInfo} holds a call argument for a particular IR call
 * statement. It associates the call statement with a particular IR register
 * that holds the value of the call argument.
 *
 * \struct{IrCallResultInfo} holds call result information for a particular IR
 * call statement. It associates a the call statement with a particular IR
 * register that holds the call result value.
 *
 * \struct{IrReturnvalInfo} is associated with a IRSTMT_RETURN
 * statement and holds one of the values that should be returned.
 */

struct IrSymbolInfo {
        String name;
};

struct IrRegInfo {
        IrProc proc;

        /* TODO: think about IrReg names and their relation
        to the Symbols on the language level */
        String name;
        Symbol sym;
        Type tp; //XXX
};

struct IrCallArgInfo {
        IrStmt callStmt;
        IrReg srcreg;
};

struct IrCallResultInfo {
        IrStmt callStmt;
        IrReg tgtreg;
};

struct IrReturnvalInfo {
        IrStmt returnStmt;
        IrReg resultReg;
};

struct IrLoadConstantStmtInfo {
        int kind;  // IRCONSTANT_
        union {
                long long tInteger;
                String tString;
        };
        IrReg tgtreg;
};

struct IrLoadSymbolAddrStmtInfo {
        Symbol sym;
        IrReg tgtreg;
};

struct IrLoadRegAddrStmtInfo {
        IrReg reg;
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

struct IrRegregStmtInfo {
        IrReg srcreg;
        IrReg tgtreg;
};

struct IrCallStmtInfo {
        IrReg calleeReg;
        IrCallArg firstIrCallArg;  // speed-up
        IrCallResult firstIrCallResult;  // speed-up
};

struct IrCondGotoStmtInfo {
        IrReg condreg;
        IrStmt tgtstmt;
        /* Jump if the condition is false or true?
         * This flag might lead to complexity... */
        int isNeg;
};

struct IrGotoStmtInfo {
        IrStmt tgtstmt;
};

struct IrReturnStmtInfo {
        IrReturnval firstResult;
};

struct IrStmtInfo {
        IrProc proc;
        int kind;  // IRSTMT_
        union {
                struct IrLoadConstantStmtInfo tLoadConstant;
                struct IrLoadSymbolAddrStmtInfo tLoadSymbolAddr;
                struct IrLoadRegAddrStmtInfo tLoadRegAddr;
                struct IrLoadStmtInfo tLoad;
                struct IrStoreStmtInfo tStore;
                struct IrRegregStmtInfo tRegreg;
                struct IrCallStmtInfo tCall;
                struct IrCondGotoStmtInfo tCondGoto;
                struct IrGotoStmtInfo tGoto;
                struct IrReturnStmtInfo tReturn;
        };
};

struct IrLabelInfo {
        IrStmt stmt;
        String name;
};

struct IrProcInfo {
        Symbol symbol;
        IrStmt firstIrStmt; // speed-up
        IrStmt firstIrReg; // speed-up
};

/* jump sources */
struct IrOrigin {
        IrStmt stmt;
        IrStmt originStmt;
};


/*
 *
 */

DATA IrProc *procToIrProc;
DATA IrReg *dataToIrReg;
DATA IrReg *exprToIrReg;


DATA int irSymbolCnt;
DATA int irRegCnt;
DATA int irStmtCnt;
DATA int irCallArgCnt;
DATA int irCallResultCnt;
DATA int irReturnvalCnt;
DATA int irProcCnt;
DATA int irLabelCnt;
DATA int irOriginCnt;

DATA struct IrSymbolInfo *irSymbolInfo;
DATA struct IrRegInfo *irRegInfo;
DATA struct IrStmtInfo *irStmtInfo;
DATA struct IrCallArgInfo *irCallArgInfo;
DATA struct IrCallResultInfo *irCallResultInfo;
DATA struct IrReturnvalInfo *irReturnvalInfo;
DATA struct IrProcInfo *irProcInfo;
DATA struct IrLabelInfo *irLabelInfo;
DATA struct IrOrigin *irOrigin;
