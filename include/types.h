#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif

/**
 * \typedef{Type}: Type of a runtime value.
 * \typedef{Param}: Type of a formal Proc parameter. See also \ref{ParamInfo}
 * \typedef{Structmember}: Member of a Struct type. See also
 * \ref{StructmemberInfo}.
 */

typedef int Type;
typedef int Param;
typedef int Structmember;

/**
 * \enum{TypeKind}: Type kinds. Types are needed to compile efficient machine
 * code. There are built-in and user-defined types. The builtin types are
 * various kinds of integers. The user-defined types, such as entities, arrays,
 * or procedures, are composed of other existing types (which may themselves be
 * user-defined, or built-in ones). Lexical references (by name) to other types
 * are implemented as types of kind TYPE_REFERENCE, and these types contain
 * symbol references, which must resolve to SYMBOL_TYPE symbols.
 */

enum TypeKind {
        TYPE_BASE,
        TYPE_STRUCT,
        TYPE_ENTITY,
        TYPE_ARRAY,
        TYPE_POINTER,
        TYPE_PROC,
        TYPE_REFERENCE, // reference another type by name
        NUM_TYPE_KINDS,
};


/**
 * \enum{BuiltintypeKind}. We have a few builtin types as internal dependencies.
 * These get initialized early in the life of the process. The resulting types
 * are stored in a lookup table, see \ref{builtinType}.
 */

enum BuiltinTypeKind {
        BUILTINTYPE_VOID,
        BUILTINTYPE_INT,
        BUILTINTYPE_CHAR,
        NUM_BUILTINTYPE_KINDS,
};

/**
 */

struct BasetypeInfo {
        String name;
        int size;
};

struct StructtypeInfo {
        String name;
        /* XXX: Really the struct size is backend-specific (basic type sizes,
         * alignment etc) so this probably shouldn't be here */
        int size;
        Structmember firstStructmember;  // speed-up
};

struct StructmemberInfo {
        Type structTp;
        Type memberTp;
        String memberName;
        int offset;
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
};

/**
 * \struct{ParamInfo}: Associates a Proc type with a type of a parameter of
 * that proc. Order of proc parameters is given by their index (~ memory
 * address)
 */

struct ParamInfo {
        Type proctp;
        Type tp;
        Symbol sym;  // parameter name. Should this really be part of the type?
};

struct ReftypeInfo {
        Symref ref;
        Type resolvedTp;
};

struct TypeInfo {
        int typeKind;  // TYPE_?
        union {
                struct BasetypeInfo tBase;
                struct StructtypeInfo tStruct;
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
 * \struct{BasetypeToBeInitialized}: Static information used at program
 * initialization time when base types get registered.
 */

struct BasetypeToBeInitialized {
        const char *name;
        int size;
        Type *builtinTypePtr;
};

extern const struct BasetypeToBeInitialized basetypesToBeInitialized[];
extern const int basetypesToBeInitializedCnt;

extern const char *const typeKindString[NUM_TYPE_KINDS];

DATA Type builtinType[NUM_BUILTINTYPE_KINDS];  // initialized early

DATA int typeCnt;
DATA int structmemberCnt;
DATA int paramCnt;

DATA struct TypeInfo *typeInfo;
DATA struct StructmemberInfo *structmemberInfo;
DATA struct ParamInfo *paramInfo;
DATA Type *exprType;
DATA Type *procType;
DATA Param *firstProctypeParam;
