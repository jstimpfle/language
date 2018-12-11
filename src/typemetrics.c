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
        default:
                UNHANDLED_CASE();
        }
}
