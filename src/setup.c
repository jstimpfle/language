#include "defs.h"
#include "api.h"

void setup_program(void)
{
        /* initialize constant strings */
        for (int i = 0; i < NUM_CONSTSTR_KINDS; i++) {
                const char *str = constStrToCstring[i];
                constStr[i] = intern_cstring(str);
        }

        /* initialize global scope */
        globalScope = scopeCnt++;
        RESIZE_GLOBAL_BUFFER(scopeInfo, scopeCnt);
        scopeInfo[globalScope].parentScope = -1;
        scopeInfo[globalScope].firstSymbol = -1;
        scopeInfo[globalScope].numSymbols = 0;
        scopeInfo[globalScope].scopeKind = SCOPE_GLOBAL;

        /* initialize base types */
        ASSERT(scopeCnt > 0);
        for (int i = 0; i < basetypesToBeInitializedCnt; i++) {
                String name = intern_cstring(basetypesToBeInitialized[i].name);
                int size = basetypesToBeInitialized[i].size;
                Type tp = typeCnt++;
                RESIZE_GLOBAL_BUFFER(typeInfo, typeCnt);
                typeInfo[tp].typeKind = TYPE_BASE;
                typeInfo[tp].tBase.name = name;
                typeInfo[tp].tBase.size = size;

                Symbol x = symbolCnt++;
                RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
                RESIZE_GLOBAL_BUFFER(symbolToToken, symbolCnt);
                symbolInfo[x].name = name;
                symbolInfo[x].scope = globalScope;
                symbolInfo[x].symbolKind = SYMBOL_TYPE;
                symbolInfo[x].tType = tp;
                symbolToToken[x] = (Token) -1;

                *basetypesToBeInitialized[i].builtinTypePtr = tp;
        }

        /* initialize top-level (directive) parsers */
        for (int i = 0; i < builtinDirectiveKindCnt; i++) {
                int kind = directiveKindCnt++;
                RESIZE_GLOBAL_BUFFER(directiveKindInfo, directiveKindCnt);
                directiveKindInfo[kind].keyword = constStr[
                                builtinDirectiveKindInfo[i].constStrKind];
                directiveKindInfo[kind].parser =
                                builtinDirectiveKindInfo[i].parser;
        }
}
