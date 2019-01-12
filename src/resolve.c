#include "defs.h"
#include "api.h"

void ensure_there_are_no_symbol_collisions(void)
{
        for (Symbol sym = 1; sym < symbolCnt; sym++) {
                if (symbolInfo[sym].scope == symbolInfo[sym - 1].scope &&
                    symbolInfo[sym].name == symbolInfo[sym - 1].name) {
                        // TODO better message
                        FATAL("Symbol already defined in this scope: %s\n",
                              SS(sym));
                }
        }
}

INTERNAL
Symbol find_first_matching_symbol_in_scope(String name, Scope scope)
{
        // DEBUG("Try to find symbol %s in scope %d\n", string_buffer(name), scope);
        for (; scope != -1; scope = scopeInfo[scope].parentScope) {
                Symbol first = scopeInfo[scope].firstSymbol;
                Symbol last = first + scopeInfo[scope].numSymbols;

                while (first < last) {
                        Symbol mid = first + (last - first) / 2;
                        if (symbolInfo[mid].name < name)
                                first = mid + 1;
                        else
                                last = mid;
                }

                if (first < symbolCnt && symbolInfo[first].name == name)
                        return first;
        }
        return (Symbol) -1;
}

void resolve_symbol_references(void)
{
        RESIZE_GLOBAL_BUFFER(symrefToSym, symrefCnt);
        int bad = 0;
        for (Symref ref = 0; ref < symrefCnt; ref++) {
                String name = symrefInfo[ref].name;
                Scope refScope = symrefInfo[ref].refScope;
                Symbol sym = find_first_matching_symbol_in_scope(name, refScope);
                if (sym == (Symbol) -1) {
                        MSG_AT_TOK(lvl_error, symrefToToken[ref],
                                   "unresolved symbol reference %s\n",
                                   string_buffer(name));
                        bad = 1;
                }
                symrefToSym[ref] = sym;
        }
        if (bad) {
                FATAL("Symbol resolution failed. Terminating early.\n");
        }

        RESIZE_GLOBAL_BUFFER(isSymbolExported, symbolCnt);
        for (Symbol sym = 0; sym < symbolCnt; sym++)
                isSymbolExported[sym] = 0;
        for (Export x = 0; x < exportCnt; x++) {
                Symref ref = exportInfo[x].ref;
                Symbol sym = symrefToSym[ref];
                isSymbolExported[sym] = 1;
        }
}

INTERNAL
void resolve_ref_type(Type t)
{
        ASSERT(0 <= t && t < typeCnt);
        if (typeInfo[t].isComplete >= 0) {
                /* already processed */
                return;
        }
        if (typeInfo[t].isComplete == -1) {
                MSG(lvl_warn, "Type #%d: cyclic type reference\n", t);
                typeInfo[t].isComplete = 0;
                return;
        }
        ASSERT(typeInfo[t].isComplete == -2);
        typeInfo[t].isComplete = -1;

        const int UNASSIGNED = -42;
        int isComplete = UNASSIGNED;

        switch (typeInfo[t].typeKind) {
        case TYPE_BASE:
                isComplete = 1;
                break;
        case TYPE_STRUCT:
                isComplete = 1;
                for (Structmember m = typeInfo[t].tStruct.firstStructmember;
                     m < structmemberCnt && structmemberInfo[m].structTp == t;
                     m++) {
                        Type mt = structmemberInfo[m].memberTp;
                        resolve_ref_type(mt);
                        isComplete = isComplete & typeInfo[mt].isComplete;
                }
                break;
        case TYPE_ENTITY:
                resolve_ref_type(typeInfo[t].tEntity.tp);
                isComplete = typeInfo[typeInfo[t].tEntity.tp].isComplete;
                break;
        case TYPE_ARRAY:
                resolve_ref_type(typeInfo[t].tArray.valueTp);
                // TODO: XXX: This doesn't make sense. At this stage the
                // length is never known.
                isComplete = typeInfo[typeInfo[t].tArray.valueTp].isComplete;
                break;
        case TYPE_POINTER:
                resolve_ref_type(typeInfo[t].tPointer.tp);
                isComplete = typeInfo[typeInfo[t].tPointer.tp].isComplete;
                break;
        case TYPE_PROC: {
                Type rettp = typeInfo[t].tProc.rettp;
                resolve_ref_type(rettp);
                isComplete = isComplete && typeInfo[rettp].isComplete;
                for (Param i = firstProctypeParam[t];
                     i < paramCnt && paramInfo[i].proctp == t;
                     i++) {
                        Type pt = paramInfo[i].tp;
                        resolve_ref_type(pt);
                        isComplete = isComplete && typeInfo[pt].isComplete;
                }
                break;
        }
        case TYPE_REFERENCE: {
                isComplete = 0;
                Symbol sym = symrefToSym[typeInfo[t].tRef.ref];
                if (sym != -1 && symbolInfo[sym].symbolKind == SYMBOL_TYPE) {
                        Type symtp = symbolInfo[sym].tType;
                        if (symtp != -1) {
                                resolve_ref_type(symtp);
                                isComplete = typeInfo[symtp].isComplete;
                        }
                        typeInfo[t].tRef.resolvedTp = symtp;
                }
                break;
        }
        default:
                UNHANDLED_CASE();
        }
        ASSERT(isComplete == 0 || isComplete == 1);
        typeInfo[t].isComplete = isComplete;
}

void resolve_type_references(void)
{
        for (Type tp = 0; tp < typeCnt; tp++)
                if (typeInfo[tp].typeKind == TYPE_STRUCT)
                        typeInfo[tp].tStruct.firstStructmember = structmemberCnt;
        for (Structmember m = structmemberCnt; m --> 0;) {
                Type tp = structmemberInfo[m].structTp;
                ASSERT(typeInfo[tp].typeKind == TYPE_STRUCT);
                typeInfo[tp].tStruct.firstStructmember = m;
        }
        /* isComplete -2 means "TO DO" */
        /* isComplete -1 means "currently resolving" */
        for (Type t = 0; t < typeCnt; t++)
                typeInfo[t].isComplete = -2;
        for (Type t = 0; t < typeCnt; t++)
                if (typeInfo[t].typeKind == TYPE_REFERENCE)
                        typeInfo[t].tRef.resolvedTp = -1;
        for (Type t = 0; t < typeCnt; t++)
                if (typeInfo[t].isComplete == -2)
                        resolve_ref_type(t);
        for (Type t = 0; t < typeCnt; t++) {
                ASSERT(typeInfo[t].isComplete == 1 ||
                       typeInfo[t].isComplete == 0);
        }
}
