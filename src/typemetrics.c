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
                outs(SS(symrefToSym[ref]));
                break;
        }
        case TYPE_STRUCT:
                outs(string_buffer(typeInfo[tp].tStruct.name));
                break;
        case TYPE_POINTER:
                outs("^");
                print_type(typeInfo[tp].tPointer.tp);
                break;
        case TYPE_PROC:
                outs("(procedure)");
                break;
        default:
                UNHANDLED_CASE();
        }
}
