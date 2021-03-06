#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif


/**
 * \enum{TypeKind}: Type kinds. Types are needed to compile efficient machine
 * code. There are built-in and user-defined types. The builtin types are
 * various kinds of integers. The user-defined types, such as entities, arrays,
 * or procedures, are composed of other existing types (which may themselves be
 * user-defined, or built-in ones). Lexical references (by name) to other types
 * are implemented as types of kind TYPE_REFERENCE, and these types contain
 * symbol references, which must resolve to SYMBOL_TYPE symbols.
 *
 * \enum{BuiltintypeKind}. We have a few builtin types as internal dependencies.
 * These get initialized early in the life of the process. The resulting types
 * are stored in a lookup table, see \ref{builtinType}.
 *
 * \enum{ResolveState}: Types that have references to other types, either
 * directly (if kind == TYPE_REFERENCE) or indirectly (by being a compound of
 * other types that have references), undergo a separate processing phase which
 * we call "type resolution" here. Type resolution happens after parsing and
 * symbol resolution. Each type starts in state RESOLVE_NOTVISITED. When it is
 * first visited the state changes to RESOLVE_PROCESSING. From there, at some
 * point the final state is reached, which can be either of RESOLVE_RESOLVED or
 * RESOLVE_ERROR.
 */

enum TypeKind {
        TYPE_BASE,
        TYPE_STRUCT,
        TYPE_ARRAY,
        TYPE_POINTER,
        TYPE_PROC,
        TYPE_REFERENCE, // reference another type by name
        NUM_TYPE_KINDS,
};

enum BuiltinTypeKind {
        BUILTINTYPE_VOID,  // cannot be materialized
        BUILTINTYPE_COMPOUND,  // ditto
        BUILTINTYPE_INT,
        BUILTINTYPE_CHAR,
        BUILTINTYPE_FLOAT,
        BUILTINTYPE_DOUBLE,
        NUM_BUILTINTYPE_KINDS,
};

enum ResolveState {
        RESOLVE_NOTVISITED,
        RESOLVE_PROCESSING,
        RESOLVE_RESOLVED,
        RESOLVE_ERROR,
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
        int numMembers;
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
        Type valueTp;
        /* If the array is defined by an "array" directive, then the length
         * field is -1 until the constant length expression is evaluated */
        Constant lengthConstant;
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
        int rank;
};

struct ReftypeInfo {
        Symref ref;
        Type resolvedTp;
};

struct TypeInfo {
        int resolveState;  // RESOLVE_??
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

/*
 */

extern const char *const typeKindString[NUM_TYPE_KINDS];
extern const struct BasetypeToBeInitialized basetypesToBeInitialized[];
extern const int basetypesToBeInitializedCnt;

/*
 */

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
DATA int *isTypeInferred;
