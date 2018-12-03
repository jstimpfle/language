#include "defs.h"
#include "api.h"

const char *const extsymname[NUM_EXTSYMS] = {
#define MAKE(name) [EXTSYM_##name] = #name
        MAKE( print64 ),
        MAKE( prints ),
#undef MAKE
};

INTERNAL
Symbol find_symbol_in_scope(String name, Scope scope)
{
        for (; scope != -1; scope = scopeInfo[scope].parentScope) {
                Symbol first = scopeInfo[scope].firstSymbol;
                Symbol last = first + scopeInfo[scope].numSymbols;
                for (Symbol i = first; i < last; i++) {
                        if (symbolInfo[i].name == name) {
                                return i;
                        }
                }
        }
        return -1;
}

INTERNAL
int compare_Symbol(const void *a, const void *b)
{
        const Symbol *x = a;
        const Symbol *y = b;
        return symbolInfo[*x].scope - symbolInfo[*y].scope;
}

INTERNAL
int compare_ParamInfo(const void *a, const void *b)
{
        const struct ParamInfo *x = a;
        const struct ParamInfo *y = b;
        if (x->proctp != y->proctp)
                return x->proctp - y->proctp;
        return x->rank - y->rank;
}

INTERNAL
int compare_ChildStmtInfo(const void *a, const void *b)
{
        const struct ChildStmtInfo *x = a;
        const struct ChildStmtInfo *y = b;
        if (x->parent != y->parent)
                return x->parent - y->parent;
        return x->rank - y->rank;
}

INTERNAL
int compare_CallArgInfo(const void *a, const void *b)
{
        const struct CallArgInfo *x = a;
        const struct CallArgInfo *y = b;
        if (x->callExpr != y->callExpr)
                return x->callExpr - y->callExpr;
        return x->rank - y->rank;
}

void resolve_symbol_references(void)
{
        DEBUG("Add external symbols\n");
        for (int i = 0; i < NUM_EXTSYMS; i++) {
                const char *name = extsymname[i];
                DEBUG("Add external symbol %s\n", name);

                Type tp = typeCnt++;
                Symbol sym = symbolCnt++;

                RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
                typeInfo[tp].kind = TYPE_PROC;
                typeInfo[tp].tProc.rettp = (Type) 0; //XXX
                typeInfo[tp].tProc.nparams = 0;

                RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
                symbolInfo[sym].name = intern_cstring(name);
                symbolInfo[sym].scope = (Scope) 0;
                symbolInfo[sym].kind = SYMBOL_PROC;  //XXX or sth like SYMBOL_UNDEFINED?
                symbolInfo[sym].tProc.tp = tp;
                symbolInfo[sym].tProc.optionalproc = -1;

                extsymToSymbol[i] = sym;
        }

        {
                /* permute Symbol array so they are grouped by defining scope */
                /* TODO: this kind of renaming should be abstracted */
                Symbol *order;
                Symbol *newname;
                struct Alloc orderAlloc;
                struct Alloc newnameAlloc;
                BUF_INIT(&order, &orderAlloc);
                BUF_INIT(&newname, &newnameAlloc);
                BUF_RESERVE(&order, &orderAlloc, symbolCnt);
                BUF_RESERVE(&newname, &newnameAlloc, symbolCnt);
                for (Symbol i = 0; i < symbolCnt; i++)
                        order[i] = i;
                sort_array(order, symbolCnt, sizeof *order,
                           compare_Symbol);
                for (Symbol i = 0; i < symbolCnt; i++)
                        newname[order[i]] = i;
                for (int i = 0; i < NUM_EXTSYMS; i++)
                        extsymToSymbol[i] = newname[extsymToSymbol[i]];
                for (Data i = 0; i < dataCnt; i++)
                        dataInfo[i].sym = newname[dataInfo[i].sym];
                for (Array i = 0; i < arrayCnt; i++)
                        arrayInfo[i].sym = newname[arrayInfo[i].sym];
                for (Proc i = 0; i < procCnt; i++)
                        procInfo[i].sym = newname[procInfo[i].sym];
                for (Param i = 0; i < paramCnt; i++)
                        paramInfo[i].sym = newname[paramInfo[i].sym];
                for (Symbol i = 0; i < symbolCnt; i++) {
                        Symbol j = newname[i];
                        while (j != i) {
                                struct SymbolInfo tmp = symbolInfo[i];
                                symbolInfo[i] = symbolInfo[j];
                                symbolInfo[j] = tmp;
                                Symbol next = newname[j];
                                newname[j] = j;
                                j = next;
                        }
                }
                BUF_EXIT(&order, &orderAlloc);
                BUF_EXIT(&newname, &newnameAlloc);
        }

        /* sort paramInfo array and fix symbols */
        sort_array(paramInfo, paramCnt, sizeof *paramInfo,
                   compare_ParamInfo);

        RESIZE_GLOBAL_BUFFER(firstProctypeParam, typeCnt);
        for (Param param = paramCnt; param --> 0;) {
                Type proctp = paramInfo[param].proctp;
                ASSERT(0 <= proctp && proctp < typeCnt);
                ASSERT(typeInfo[proctp].kind == TYPE_PROC);
                firstProctypeParam[proctp] = param;
        }

        sort_array(childStmtInfo, childStmtCnt, sizeof *childStmtInfo,
                   compare_ChildStmtInfo);
        sort_array(callArgInfo, callArgCnt, sizeof *callArgInfo,
                   compare_CallArgInfo);

        for (int i = childStmtCnt; i --> 0;) {
                Stmt parent = childStmtInfo[i].parent;
                ASSERT(stmtInfo[parent].kind == STMT_COMPOUND);
                stmtInfo[parent].tCompound.numStatements++;
                stmtInfo[parent].tCompound.firstChildStmtIdx = i;
        }

        for (int i = callArgCnt; i --> 0;) {
                Expr callee = callArgInfo[i].callExpr;
                ASSERT(exprInfo[callee].kind == EXPR_CALL);
                exprInfo[callee].tCall.nargs++;
                exprInfo[callee].tCall.firstArgIdx = i;
        }

        for (Symbol i = symbolCnt; i --> 0;) {
                scopeInfo[symbolInfo[i].scope].numSymbols++;
                scopeInfo[symbolInfo[i].scope].firstSymbol = i;
        }

        RESIZE_GLOBAL_BUFFER(symrefToSym, symrefCnt);
        int bad = 0;
        for (Symref ref = 0; ref < symrefCnt; ref++) {
                String name = symrefInfo[ref].name;
                Scope refScope = symrefInfo[ref].refScope;
                Symbol sym = find_symbol_in_scope(name, refScope);
                if (sym < 0) {
                        MSG_AT_TOK(lvl_error, symrefToToken[ref],
                                   "unresolved symbol reference %s\n",
                                   string_buffer(name));
                        bad = 1;
                }
                symrefToSym[ref] = sym;
        }
        if (bad) {
                MSG(lvl_info, "Symbol resolution failed. Terminating early.\n");
                cleanup();
                exit_program(1);
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

        const int UNASSIGNED = -42;
        int isComplete = UNASSIGNED;

        switch (typeInfo[t].kind) {
        case TYPE_BASE:
                isComplete = 1;
                break;
        case TYPE_ENTITY:
                resolve_ref_type(typeInfo[t].tEntity.tp);
                isComplete = typeInfo[typeInfo[t].tEntity.tp].isComplete;
                break;
        case TYPE_ARRAY:
                resolve_ref_type(typeInfo[t].tArray.idxtp);
                resolve_ref_type(typeInfo[t].tArray.valuetp);
                isComplete =
                        typeInfo[typeInfo[t].tArray.idxtp].isComplete &&
                        typeInfo[typeInfo[t].tArray.valuetp].isComplete;
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
                typeInfo[t].isComplete = -1;
                Symbol sym = symrefToSym[typeInfo[t].tRef.ref];
                if (sym != -1 && symbolInfo[sym].kind == SYMBOL_TYPE) {
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
        /* isComplete -2 means "TO DO" */
        /* isComplete -1 means "currently resolving" */
        for (Type t = 0; t < typeCnt; t++)
                typeInfo[t].isComplete = -2;
        for (Type t = 0; t < typeCnt; t++)
                if (typeInfo[t].kind == TYPE_REFERENCE)
                        typeInfo[t].tRef.resolvedTp = -1;
        for (Type t = 0; t < typeCnt; t++)
                if (typeInfo[t].isComplete == -2)
                        resolve_ref_type(t);
        for (Type t = 0; t < typeCnt; t++) {
                ASSERT(typeInfo[t].isComplete == 1 ||
                       typeInfo[t].isComplete == 0);
        }
}
