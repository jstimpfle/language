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
        DEBUG("Infer array %s\n", SS(dataInfo[data].sym));
        Expr lengthExpr = directiveInfo[directive].tArray.lengthExpr;
        Type tp = dataInfo[data].tp;
        ASSERT(typeInfo[tp].typeKind == TYPE_ARRAY);
        //typeInfo[tp].tArray.valueTp = check_expr_type(
        typeInfo[tp].tArray.length = fold_integer_expr(lengthExpr);
        DEBUG("Array length is %d\n", (int) typeInfo[tp].tArray.length);
}

INTERNAL
void infer_struct(Directive directive)
{
        Type tp = directiveInfo[directive].tStruct.tp;
        ASSERT(0 <= tp && tp < typeCnt);

        int size = 0;
        Structmember first = typeInfo[tp].tStruct.firstStructmember;
        for (Structmember m = first;
             m < structmemberCnt && structmemberInfo[m].structTp == tp;
             m++)
        {
                structmemberInfo[m].offset = size;
                int membersize = get_type_size(structmemberInfo[m].memberTp);
                if (size == 0) {
                        FATAL("Incomplete type: "
                              "Size of member %s is not yet known!\n",
                              string_buffer(structmemberInfo[m].memberName));
                }
                size += membersize;
                DEBUG("Add size %d to struct %d to give %d\n", size, tp, typeInfo[tp].tStruct.size);
        }
        typeInfo[tp].tStruct.size = 0;
}

void infer_constants_and_types(void)
{
        RESIZE_GLOBAL_BUFFER(constantValue, constantCnt);

        /* mark as unprocessed: we need recursion to process unprocessed
         * constants immediately. Note that cycles should not happen when
         * recursing since that case should have been caught in the type
         * checking phase already. */
        for (Constant constant = 0; constant < constantCnt; constant++)
                if (constantInfo[constant].constantKind == CONSTANT_EXPRESSION)
                        constantValue[constant].valueKind = -1;

        ASSERT(globalBufferAlloc[BUFFER_exprType].cap == 0);
        RESIZE_GLOBAL_BUFFER(exprType, exprCnt);

        /* XXX: must make sure there are no cycles.  In normal expressions,
         * there is no need for this.  But since constants are not typed we need
         * to manually detect and break cycles. We do this by setting the
         * exprType to -3 when never visited and to -2 when being processed.  As
         * usual it is -1 when type checking failed and >= 0 if the expression
         * was successfully resolved to a type */
        for (Constant constant = 0; constant < constantCnt; constant++)
                if (constantInfo[constant].constantKind == CONSTANT_EXPRESSION)
                        exprType[constantInfo[constant].tExpr] = (Type) -3;

        for (Type tp = 0; tp < typeCnt; tp++)
                if (typeInfo[tp].typeKind == TYPE_STRUCT)
                        typeInfo[tp].tStruct.size = 0;


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

        for (Proc p = 0; p < procCnt; p++)
                check_stmt_types(procInfo[p].body);
}
