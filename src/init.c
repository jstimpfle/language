#include "defs.h"
#include "api.h"

void init_data(void)
{
        /* initialize constant strings */

        for (int i = 0; i < LENGTH(stringsToBeInterned); i++) {
                int idx = stringsToBeInterned[i].constant;
                const char *str = stringsToBeInterned[i].string;
                constStr[idx] = intern_cstring(str);
        }

        /* initialize global scope */
        globalScope = scopeCnt++;
        RESIZE_GLOBAL_BUFFER(scopeInfo, scopeCnt);
        scopeInfo[globalScope].parentScope = -1;
        scopeInfo[globalScope].firstSymbol = -1;
        scopeInfo[globalScope].numSymbols = 0;
        scopeInfo[globalScope].kind = SCOPE_GLOBAL;

        /* initialize base types */
        ASSERT(scopeCnt > 0);
        for (int i = 0; i < basetypesToBeInitializedCnt; i++) {
                String name = intern_cstring(basetypesToBeInitialized[i].name);
                int size = basetypesToBeInitialized[i].size;
                Type tp = typeCnt++;
                RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
                typeInfo[tp].kind = TYPE_BASE;
                typeInfo[tp].tBase.name = name;
                typeInfo[tp].tBase.size = size;

                Symbol x = symbolCnt++;
                RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
                symbolInfo[x].name = name;
                symbolInfo[x].scope = globalScope;
                symbolInfo[x].kind = SYMBOL_TYPE;
                symbolInfo[x].tType = tp;

                *basetypesToBeInitialized[i].builtinTypePtr = tp;
        }
}
