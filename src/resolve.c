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
                Symbol lo = scopeInfo[scope].firstSymbol;
                Symbol hi = lo + scopeInfo[scope].numSymbols;
                Symbol onePastEnd = hi;

                while (lo < hi) {
                        Symbol mid = lo + (hi - lo) / 2;
                        if (symbolInfo[mid].name < name)
                                lo = mid + 1;
                        else
                                hi = mid;
                }

                if (lo < onePastEnd && symbolInfo[lo].name == name)
                        return lo;
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
        if (typeInfo[t].resolveState == RESOLVE_PROCESSING) {
                MSG(lvl_error, "Type #%d: cyclic type reference\n", t);
                typeInfo[t].resolveState = RESOLVE_ERROR;
                return;
        }
        if (typeInfo[t].resolveState != RESOLVE_NOTVISITED) {
                return; /* already processed */
        }

        typeInfo[t].resolveState = RESOLVE_PROCESSING;

        enum { UNASSIGNED = -42 };
        int resolveState = UNASSIGNED;
        switch (typeInfo[t].typeKind) {
        case TYPE_BASE:
                resolveState = RESOLVE_RESOLVED;
                break;
        case TYPE_STRUCT:
                resolveState = RESOLVE_RESOLVED;

                Structmember firstMember = typeInfo[t].tStruct.firstStructmember;
                int numMembers = typeInfo[t].tStruct.numMembers;
                for (Structmember m = firstMember;
                     m < firstMember + numMembers;
                     m++) {
                        Type mt = structmemberInfo[m].memberTp;
                        resolve_ref_type(mt);
                        if (typeInfo[mt].resolveState != RESOLVE_RESOLVED)
                                resolveState = RESOLVE_ERROR;
                }
                break;
        case TYPE_ARRAY:
                resolve_ref_type(typeInfo[t].tArray.valueTp);
                // TODO: XXX: This doesn't make sense. At this stage the
                // length is never known.
                resolveState = typeInfo[typeInfo[t].tArray.valueTp].resolveState;
                break;
        case TYPE_POINTER:
                resolve_ref_type(typeInfo[t].tPointer.tp);
                resolveState = typeInfo[typeInfo[t].tPointer.tp].resolveState;
                break;
        case TYPE_PROC: {
                Type rettp = typeInfo[t].tProc.rettp;
                resolve_ref_type(rettp);
                resolveState = typeInfo[rettp].resolveState;
                for (Param i = firstProctypeParam[t];
                     i < paramCnt && paramInfo[i].proctp == t;
                     i++) {
                        Type pt = paramInfo[i].tp;
                        resolve_ref_type(pt);
                        if (typeInfo[pt].resolveState != RESOLVE_RESOLVED)
                                resolveState = RESOLVE_ERROR;
                }
                break;
        }
        case TYPE_REFERENCE: {
                resolveState = RESOLVE_ERROR;
                Symbol sym = symrefToSym[typeInfo[t].tRef.ref];
                if (sym != -1 && symbolInfo[sym].symbolKind == SYMBOL_TYPE) {
                        Type symtp = symbolInfo[sym].tType;
                        if (symtp != -1) {
                                resolve_ref_type(symtp);
                                resolveState = typeInfo[symtp].resolveState;
                        }
                        typeInfo[t].tRef.resolvedTp = symtp;
                }
                break;
        }
        default:
                UNHANDLED_CASE();
        }
        ASSERT(resolveState == RESOLVE_RESOLVED ||
               resolveState == RESOLVE_ERROR);
        typeInfo[t].resolveState = resolveState;
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
        for (Type t = 0; t < typeCnt; t++)
                typeInfo[t].resolveState = RESOLVE_NOTVISITED;
        for (Type t = 0; t < typeCnt; t++)
                if (typeInfo[t].typeKind == TYPE_REFERENCE)
                        typeInfo[t].tRef.resolvedTp = -1;
        for (Type t = 0; t < typeCnt; t++)
                if (typeInfo[t].resolveState == RESOLVE_NOTVISITED)
                        resolve_ref_type(t);
        int bad = 0;
        for (Type t = 0; t < typeCnt; t++) {
                ASSERT(typeInfo[t].resolveState == RESOLVE_RESOLVED ||
                       typeInfo[t].resolveState == RESOLVE_ERROR);
                if (typeInfo[t].resolveState != RESOLVE_RESOLVED) {
                        outs("unresolved type: ");
                        print_type(t); outs("\n");
                        bad = 1;
                }
        }
        if (bad)
                FATAL("Errors detected during type resolution!\n");
}
