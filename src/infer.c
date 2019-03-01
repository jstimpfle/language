#include "defs.h"
#include "api.h"

INTERNAL void infer_stmt(Stmt stmt);

void infer_constant(Constant constant)
{
        /* Some constants' values are immediately known. */
        if (constantInfo[constant].constantKind != CONSTANT_EXPRESSION)
                return;

        Expr expr = constantInfo[constant].tExpr;
        /* The remaining constants (of kind CONSTANT_EXPRESSION) are currently
         * not explicitly typed. (I think we should require all constants to be
         * typed explicitly in the future). Because they are not typed, we need
         * to find out through type checking what kind of value to compute. */
        check_expr_type(expr);
        Type tp = exprType[expr];

        if (type_equal(tp, builtinType[BUILTINTYPE_INT])) {
                constantValue[constant].valueKind = VALUE_INTEGER;
                constantValue[constant].tInteger = fold_integer_expr(expr);
        }
        else if (type_equal(tp, pointer_type(builtinType[BUILTINTYPE_CHAR]))) {
                constantValue[constant].valueKind = VALUE_STRING;
                constantValue[constant].tString = fold_string_expr(expr);
        }
        else {
                UNHANDLED_CASE();
        }
}

INTERNAL
void infer_base_type(Type tp)
{
        (void) tp;
        return;
}

INTERNAL
void infer_struct_type(Type tp)
{
        ASSERT(0 <= tp && tp < typeCnt);
        ASSERT(typeInfo[tp].typeKind == TYPE_STRUCT);
        DEBUG("Infer struct %s\n", string_buffer((typeInfo[tp].tStruct.name)));

        int size = 0;
        Structmember firstMember = typeInfo[tp].tStruct.firstStructmember;
        int numMembers = typeInfo[tp].tStruct.numMembers;
        for (Structmember m = firstMember; m < firstMember + numMembers; m++) {
                structmemberInfo[m].offset = size;
                int membersize = get_type_size(structmemberInfo[m].memberTp);
                if (membersize == 0) {
                        FATAL("Incomplete type: "
                              "Size of member %s is not yet known!\n",
                              string_buffer(structmemberInfo[m].memberName));
                }
                size += membersize;
                DEBUG("Add size %d to struct %d to give %d\n", membersize, tp, size);
        }
        typeInfo[tp].tStruct.size = size;
}

INTERNAL
void infer_array_type(Type tp)
{
        ASSERT(0 <= tp && tp < typeCnt);
        ASSERT(typeInfo[tp].typeKind == TYPE_ARRAY);
        infer_constant(typeInfo[tp].tArray.lengthConstant);
}

INTERNAL
void infer_pointer_type(Type tp)
{
        infer_type(typeInfo[tp].tPointer.tp);
}

INTERNAL
void infer_proc_type(Type tp)
{
        (void) tp;
        return;
}

INTERNAL
void infer_reference_type(Type tp)
{
        infer_type(referenced_type(tp));
}

INTERNAL
void infer_data(Data data)
{
        infer_type(dataInfo[data].tp);
}

INTERNAL
void infer_if_stmt(Stmt stmt)
{
        infer_stmt(stmtInfo[stmt].tIf.ifbody);
}

INTERNAL
void infer_ifelse_stmt(Stmt stmt)
{
        infer_stmt(stmtInfo[stmt].tIfelse.ifbody);
        infer_stmt(stmtInfo[stmt].tIfelse.elsebody);
}

INTERNAL
void infer_for_stmt(Stmt stmt)
{
        infer_stmt(stmtInfo[stmt].tFor.forbody);
}

INTERNAL
void infer_while_stmt(Stmt stmt)
{
        infer_stmt(stmtInfo[stmt].tWhile.whilebody);
}

INTERNAL
void infer_range_stmt(Stmt stmt)
{
        infer_stmt(stmtInfo[stmt].tRange.rangebody);
}

INTERNAL
void infer_return_stmt(Stmt stmt)
{
        (void) stmt;
}

INTERNAL
void infer_expr_stmt(Stmt stmt)
{
        (void) stmt;
}

INTERNAL
void infer_compound_stmt(Stmt stmt)
{
        int first = stmtInfo[stmt].tCompound.firstChildStmtIdx;
        int c = stmtInfo[stmt].tCompound.numStatements;
        for (int child = first; child < first + c; child++)
                infer_stmt(childStmtInfo[child].child);
}

INTERNAL
void infer_data_stmt(Stmt stmt)
{
        infer_data(stmtInfo[stmt].tData.data);
}

INTERNAL
void infer_macro_stmt(Stmt stmt)
{
        (void) stmt;
}

INTERNAL
void infer_ignore_stmt(Stmt stmt)
{
        infer_stmt(stmtInfo[stmt].tIgnore);
}

INTERNAL
void infer_constant_directive(Directive directive)
{
        infer_constant(directiveInfo[directive].tConstant);
}

INTERNAL
void infer_struct_directive(Directive directive)
{
        infer_type(directiveInfo[directive].tStruct.tp);
}

INTERNAL
void infer_data_directive(Directive directive)
{
        infer_data(directiveInfo[directive].tData.data);
}

INTERNAL
void infer_proc_directive(Directive directive)
{
        Proc proc = directiveInfo[directive].tProc;
        infer_stmt(procInfo[proc].body);
}


INTERNAL
void (*const typeKindToInferFunc[NUM_TYPE_KINDS])(Type tp) = {
#define MAKE(tk, f) [tk] = &(f)
        MAKE( TYPE_BASE,      infer_base_type ),
        MAKE( TYPE_STRUCT,    infer_struct_type ),
        MAKE( TYPE_ARRAY,     infer_array_type ),
        MAKE( TYPE_POINTER,   infer_pointer_type ),
        MAKE( TYPE_PROC,      infer_proc_type ),
        MAKE( TYPE_REFERENCE, infer_reference_type ),
#undef MAKE
};

INTERNAL
void (*const stmtKindToInferFunc[NUM_STMT_KINDS])(Stmt stmt) = {
#define MAKE(sk, f) [sk] = &(f)
        MAKE( STMT_IF,       infer_if_stmt ),
        MAKE( STMT_IFELSE,   infer_ifelse_stmt ),
        MAKE( STMT_FOR,      infer_for_stmt ),
        MAKE( STMT_WHILE,    infer_while_stmt ),
        MAKE( STMT_RANGE,    infer_range_stmt ),
        MAKE( STMT_RETURN,   infer_return_stmt ),
        MAKE( STMT_EXPR,     infer_expr_stmt ),
        MAKE( STMT_COMPOUND, infer_compound_stmt ),
        MAKE( STMT_DATA,     infer_data_stmt ),
        MAKE( STMT_MACRO,    infer_macro_stmt ),
        MAKE( STMT_IGNORE,   infer_ignore_stmt ),
#undef MAKE
};

INTERNAL
void (*const directiveKindToInferFunc[NUM_BUILTINDIRECTIVE_KINDS])(Directive d) = {
#define MAKE(dk, f) [dk] = &(f)
        MAKE( BUILTINDIRECTIVE_CONSTANT, infer_constant_directive ),
        MAKE( BUILTINDIRECTIVE_STRUCT,   infer_struct_directive ),
        MAKE( BUILTINDIRECTIVE_DATA,     infer_data_directive ),
        MAKE( BUILTINDIRECTIVE_PROC,     infer_proc_directive ),
#undef MAKE
};

void infer_type(Type tp)
{
        ASSERT(0 <= tp && tp < typeCnt);
        if (isTypeInferred[tp])
                return;
        int typeKind = typeInfo[tp].typeKind;
        ASSERT(0 <= typeKind && typeKind < NUM_TYPE_KINDS);
        typeKindToInferFunc[typeKind](tp);
        isTypeInferred[tp] = 1;
}

INTERNAL
void infer_stmt(Stmt stmt)
{
        ASSERT(0 <= stmt && stmt < stmtCnt);
        int stmtKind = stmtInfo[stmt].stmtKind;
        ASSERT(0 <= stmtKind && stmtKind < NUM_STMT_KINDS);
        stmtKindToInferFunc[stmtKind](stmt);
}

INTERNAL
void infer_directive(Directive d)
{
        ASSERT(0 <= d && d < directiveCnt);
        int directiveKind = directiveInfo[d].directiveKind;
        /* XXX, we only have builtin directives currently.
         * Should we remove this flexibility altogether? */
        ASSERT(0 <= directiveKind && directiveKind < NUM_BUILTINDIRECTIVE_KINDS);
        if (directiveKindToInferFunc[directiveKind])
                directiveKindToInferFunc[directiveKind](d);
}

void infer_constants_and_types(void)
{
        ASSERT(globalBufferAlloc[BUFFER_constantValue].cap >= constantCnt);
        ASSERT(globalBufferAlloc[BUFFER_exprType].cap == 0);
        RESIZE_GLOBAL_BUFFER(exprType, exprCnt);

        /* Mark as not-yet-typechecked */
        for (Expr x = 0; x < exprCnt; x++)
                exprType[x] = (Type) -1;

        /* mark as unprocessed. We will infer constants and types in order of
         * definition. If an earlier defined constant depends on a later defined
         * constant, we will detect this dependency problem by looking if that
         * constant is already processed. */
        for (Constant constant = 0; constant < constantCnt; constant++)
                if (constantInfo[constant].constantKind == CONSTANT_EXPRESSION)
                        constantValue[constant].valueKind = -1;

        RESIZE_GLOBAL_BUFFER(isTypeInferred, typeCnt);
        for (Type tp = 0; tp < typeCnt; tp++)
                isTypeInferred[tp] = 0;

        for (int i = 0; i < directiveCnt; i++)
                infer_directive(i);

        DEBUG("Infer types of all procedure bodies..\n");
        for (Proc p = 0; p < procCnt; p++)
                check_stmt_types(procInfo[p].body);
}
