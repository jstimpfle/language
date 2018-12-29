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
        scopeInfo[globalScope].scopeKind = SCOPE_GLOBAL;

        /* initialize builtin macros */
        {
        /*
         * sizeof(x) macro that creates a EXPR_SIZEOF expression.
         */
        /*
         * XXX TODO:  Remove this code as soon as possible. It's too much code,
         * and it's duplication of the idiosyncrasies of the internal
         * representation of macros. We need
         *
         * 1) a textual syntax for EXPR_SIZEOF that is distinct
         * from normal function calls. This is the easy part. Maybe something
         * like "#sizeof EXPRESSION"
         *
         * 2) And then to define the macro as "macro sizeof(x) = #sizeof x" we
         * also need a way to parse anonymous baked-in modules from internal
         * data. This part still requires some work - we cannot even parse
         * multiple external files yet.
         *
         * Instead of requiring the hard part we could also avoid having a
         * sizeof(x) macro altogether, and just use the specialized syntax.
         */
        Scope macroScope = scopeCnt++;
        Symbol macroSymbol = symbolCnt++;
        Symbol paramSymbol = symbolCnt++;
        Macro sizeofMacro = macroCnt++;
        MacroParam param = macroParamCnt++;
        Symref symref = symrefCnt++;
        Expr symrefExpr = exprCnt++;
        Expr macroExpr = exprCnt++;
        RESIZE_GLOBAL_BUFFER(scopeInfo, scopeCnt);
        RESIZE_GLOBAL_BUFFER(symbolInfo, symbolCnt);
        RESIZE_GLOBAL_BUFFER(macroInfo, macroCnt);
        RESIZE_GLOBAL_BUFFER(macroParamInfo, macroParamCnt);
        RESIZE_GLOBAL_BUFFER(symrefInfo, symrefCnt);
        RESIZE_GLOBAL_BUFFER(exprInfo, exprCnt);
        scopeInfo[macroScope].parentScope = globalScope;
        scopeInfo[macroScope].firstSymbol = -1;  //XXX ???
        scopeInfo[macroScope].numSymbols = 0;  // XXX ???
        scopeInfo[macroScope].scopeKind = SCOPE_MACRO;
        scopeInfo[macroScope].tMacro = sizeofMacro;
        symbolInfo[macroSymbol].name = intern_cstring("sizeof");
        symbolInfo[macroSymbol].scope = globalScope;
        symbolInfo[macroSymbol].symbolKind = SYMBOL_MACRO;
        symbolInfo[macroSymbol].tMacro = sizeofMacro;
        symbolInfo[paramSymbol].name = intern_cstring("x");
        symbolInfo[paramSymbol].scope = macroScope;
        symbolInfo[paramSymbol].symbolKind = SYMBOL_MACROPARAM;
        symbolInfo[paramSymbol].tMacroParam = param;
        macroInfo[sizeofMacro].symbol = macroSymbol;
        macroInfo[sizeofMacro].scope = macroScope;
        macroInfo[sizeofMacro].expr = macroExpr;
        macroInfo[sizeofMacro].firstMacroParam = param;
        macroInfo[sizeofMacro].nparams = 1;
        macroParamInfo[param].macro = sizeofMacro;
        macroParamInfo[param].name = intern_cstring("x");
        symrefInfo[symref].name = intern_cstring("x");
        symrefInfo[symref].refScope = macroScope;
        /* XXX symrefToToken[symref] not set here :(. This is begging for a
         * crash. Another argument why we should just parse the macro from a
         * baked-in "text file". */
        exprInfo[symrefExpr].proc = -1;
        exprInfo[symrefExpr].exprKind = EXPR_SYMREF;
        exprInfo[symrefExpr].tSymref.ref = symref;
        exprInfo[macroExpr].proc = -1;
        exprInfo[macroExpr].exprKind = EXPR_SIZEOF;
        exprInfo[macroExpr].tSizeof.expr = symrefExpr;
        }

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
                symbolInfo[x].name = name;
                symbolInfo[x].scope = globalScope;
                symbolInfo[x].symbolKind = SYMBOL_TYPE;
                symbolInfo[x].tType = tp;

                *basetypesToBeInitialized[i].builtinTypePtr = tp;
        }
}
