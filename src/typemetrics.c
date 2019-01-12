#include "defs.h"
#include "api.h"

Type referenced_type(Type t)
{
        while (typeInfo[t].typeKind == TYPE_REFERENCE)
                t = typeInfo[t].tRef.resolvedTp;
        return t;
}

int get_type_size(Type tp)
{
        tp = referenced_type(tp);
        switch (typeInfo[tp].typeKind) {
        case TYPE_BASE:    return typeInfo[tp].tBase.size;
        case TYPE_STRUCT:  return typeInfo[tp].tStruct.size;
        case TYPE_POINTER: return 8; //XXX
        case TYPE_ARRAY: return typeInfo[tp].tArray.length *
                                get_type_size(typeInfo[tp].tArray.valueTp);
        case TYPE_PROC:
                /* XXX: this is a hack I put in here to handle normal function
                 * calls. We have the syntax foo(3) where foo could be either
                 * a proc symbol or a variable holding a function pointer.
                 *
                 * These two cases should likely be unified in a less hackish
                 * way, but for now we just say the proc is pointer-sized, to
                 * work around the special casing in compile.c
                 */
                return 8;
        default:
                UNHANDLED_CASE();
        }
}

/* let's just put this here since we don't have a good place */
void print_type(Type tp)
{
        switch (typeInfo[tp].typeKind) {
        case TYPE_BASE:
                outs(string_buffer(typeInfo[tp].tBase.name));
                break;
        case TYPE_REFERENCE: {
                Symref ref = typeInfo[tp].tRef.ref;
                String name = symrefInfo[ref].name;
                outs(string_buffer(name));
                break;
        }
        case TYPE_STRUCT:
                outs(string_buffer(typeInfo[tp].tStruct.name));
                break;
        case TYPE_POINTER:
                outs("^");
                print_type(typeInfo[tp].tPointer.tp);
                break;
        case TYPE_PROC: {
                print_type(typeInfo[tp].tProc.rettp);
                outs("(");
                int first = firstProctypeParam[tp];
                int nparams = typeInfo[tp].tProc.nparams;
                for (int i = 0; i < nparams; i++) {
                        Type ptp = paramInfo[first + i].tp;
                        if (i > 0)
                                outs(", ");
                        print_type(ptp);
                }
                outs(")");
                break;
        }
        default:
                UNHANDLED_CASE();
        }
}

/* another one */
Type pointer_type(Type t)
{
        // TODO: cache pointer-to version of this type
        Type r = typeCnt++;
        RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
        typeInfo[r].typeKind = TYPE_POINTER;
        typeInfo[r].tPointer.tp = t;
        typeInfo[r].isComplete = typeInfo[t].isComplete; //XXX?
        return r;
}

int type_equal(Type a, Type b)
{
        if (!typeInfo[a].isComplete)
                return 0;
        if (!typeInfo[b].isComplete)
                return 0;
        a = referenced_type(a);
        b = referenced_type(b);
        ASSERT(a != -1);
        ASSERT(b != -1);
        /* TODO: remove this special case as soon as we have pointer type
         * interning (which means that there should be only one instance for
         * each pointer-of-type type) */
        if (typeInfo[a].typeKind == TYPE_POINTER &&
            typeInfo[b].typeKind == TYPE_POINTER)
                return type_equal(typeInfo[a].tPointer.tp,
                                  typeInfo[b].tPointer.tp);
        return a == b;
}
