#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif

/**
 * \struct{MacroBoundArg}: Binding of an argument (expression) to a formal macro
 * parameter.
 */

struct MacroBoundArg {
        MacroParam macroParam;
        Expr expr;
};

DATA int macroBoundArgCnt;
DATA struct MacroBoundArg *macroBoundArg;
