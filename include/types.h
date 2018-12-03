#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif

/**
 * \typedef{Type}: Type of a runtime value.
 * \typedef{Param}: Type of a formal Proc parameter. See also \ref{ParamInfo}
 */

typedef int Type;
typedef int Param;

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

/**
 * \struct{ParamInfo}: Associates a Proc type with a type of a parameter of
 * that proc. Order of proc parameters is specified by the Params' rank member.
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
 * \struct{BasetypeToBeInitialized}: Static information used at program
 * initialization time when base types get registered.
 */

struct BasetypeToBeInitialized {
        const char *name;
        int size;
};

extern const struct BasetypeToBeInitialized basetypesToBeInitialized[];
extern const int basetypesToBeInitializedCnt;

/**
 * \data{isExprEvaluated} indicates whether the expression is used in an
 * evaluation context. This is the case if it is not the left operand of an
 * assignment expression ("lvalue") and not the operand of an UNOP_ADDRESSOF
 * operator expression.
 *
 * Note that in general, expressions that are not evaluated are composed of
 * sub-expressions that *are* evaluated. For example, the expression
 *
 *    foo[i] = 3
 *
 * is an assignment expression. The sub-expression foo[i] is not evaluated, but
 * in turn its subexpression, the index i, must be evaluated nevertheless.
 *
 * \data{exprType} maps the expression to its type, taking into account the
 * value of \data{isExprEvaluated}. If the expression is not evaluated, the
 * type is a pointer type.
 */

extern const char *const typeKindString[];

DATA int typeCnt;
DATA int paramCnt;

DATA struct TypeInfo *typeInfo;
DATA struct ParamInfo *paramInfo;
DATA char *isExprEvaluated;
DATA Type *exprType;
DATA Type *procType;
