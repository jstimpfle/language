#include "defs.h"
#include "api.h"

Type referenced_type(Type t)
{
        while (typeInfo[t].kind == TYPE_REFERENCE)
                t = typeInfo[t].tRef.resolvedTp;
        return t;
}

int get_type_size(Type tp)
{
        tp = referenced_type(tp);
        switch (typeInfo[tp].kind) {
        case TYPE_BASE:    return typeInfo[tp].tBase.size;
        case TYPE_STRUCT:  return typeInfo[tp].tStruct.size;
        case TYPE_POINTER: return 8; //XXX
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
