#include "defs.h"
#include "api.h"

INTERNAL
void infer_constant(Directive directive)
{
        Constant constant = directiveInfo[directive].tConstant;
        ASSERT(constantInfo[constant].constantKind == CONSTANT_EXPRESSION);

        Expr expr = constantInfo[constant].tExpr;
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
void infer_array(Directive directive)
{
        Data data = directiveInfo[directive].tArray.data;
        Expr lengthExpr = directiveInfo[directive].tArray.lengthExpr;
        DEBUG("Infer array %s\n", SS(dataInfo[data].sym));
        Type tp = dataInfo[data].tp;
        ASSERT(typeInfo[tp].typeKind == TYPE_ARRAY);
        typeInfo[tp].tArray.length = fold_integer_expr(lengthExpr);
        DEBUG("Array length is %d\n", (int) typeInfo[tp].tArray.length);
}

INTERNAL
void infer_struct(Directive directive)
{
        Type tp = directiveInfo[directive].tStruct.tp;
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

void infer_constants_and_types(void)
{
        ASSERT(globalBufferAlloc[BUFFER_constantValue].cap >= constantCnt);
        ASSERT(globalBufferAlloc[BUFFER_exprType].cap == 0);
        RESIZE_GLOBAL_BUFFER(exprType, exprCnt);

        /* Mark as not-yet-typechecked */
        for (Expr x = 0; x < exprCnt; x++)
                exprType[x] = (Type) -1;

        /* mark as unprocessed. We will infer constants and types in order of
         * definition. If an earlier defined constant depends on a later default
         * constant, we will detect this dependency problem by looking if that
         * constant is already processed. */
        for (Constant constant = 0; constant < constantCnt; constant++)
                if (constantInfo[constant].constantKind == CONSTANT_EXPRESSION)
                        constantValue[constant].valueKind = -1;

        /* Mark as "incomplete" type. TODO: Should we simply have an
         * "isComplete" buffer? */
        for (Type tp = 0; tp < typeCnt; tp++)
                if (typeInfo[tp].typeKind == TYPE_STRUCT)
                        typeInfo[tp].tStruct.size = -1;

        for (int i = 0; i < directiveCnt; i++) {
                switch (directiveInfo[i].directiveKind) {
                case BUILTINDIRECTIVE_CONSTANT:
                        infer_constant(i);
                        break;
                case BUILTINDIRECTIVE_ARRAY:
                        infer_array(i);
                        break;
                case BUILTINDIRECTIVE_STRUCT:
                        infer_struct(i);
                        break;
                default:
                        break;
                }
        }

        DEBUG("Infer types of all procedure bodies..\n");
        for (Proc p = 0; p < procCnt; p++)
                check_stmt_types(procInfo[p].body);
}
