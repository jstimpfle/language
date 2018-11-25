#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif


/**
 * \enum{TypeKind}: Type kinds. Types are needed to compile efficient machine
 * code. There are built-in and user-defined types. The builtin types are
 * (mostly?) various kinds of integers and pointers (which are integers as
 * well). The user-defined types, such as entities, arrays, or procedures, are
 * made up from other existing types (which may themselves be user-defined, or
 * built-in ones). Pointers to other types are implemented as types of kind
 * TYPE_REFERENCE, and these types contain symbol references, which must resolve
 * to SYMBOL_TYPE symbols.
 */

enum TypeKind {
        TYPE_BASE,
        TYPE_ENTITY,
        TYPE_ARRAY,
        TYPE_POINTER,
        TYPE_PROC,
        TYPE_REFERENCE, // reference another type by name
};

/**
 */

struct BasetypeInfo {
        String name;
        int size;
};

struct EntitytypeInfo {
        String name;
        Type tp;
};

struct ArraytypeInfo {
        Type idxtp;
        Type valuetp;
};

struct PointertypeInfo {
        Type tp;
};

struct ProctypeInfo {
        Type rettp;
        int nparams; // XXX: needed?
        int firstParam; // speed-up
};

struct ParamInfo {
        Type proctp;
        Type tp;
        Symbol sym; // should this be part of the "type"?
        int rank;
};

struct ReftypeInfo {
        Symref ref;
        Type resolvedTp;
};

struct TypeInfo {
        int kind;  // TYPE_?
        union {
                struct BasetypeInfo tBase;
                struct EntitytypeInfo tEntity;
                struct ArraytypeInfo tArray;
                struct PointertypeInfo tPointer;
                struct ProctypeInfo tProc;
                struct ReftypeInfo tRef;
        };
        /* Types can have references to other types, either directly (if
         * kind == tRef) or indirectly (by being a compound of other types that
         * have references). These references can fail to resolve, in which
         * case the type is considered incomplete.
         */
        int isComplete;
};

/**
 */

extern const char *const typeKindString[];

DATA int typeCnt;
DATA int paramCnt;

DATA struct TypeInfo *typeInfo;
DATA struct ParamInfo *paramInfo;
DATA Type *exprType;
DATA Type *procType;