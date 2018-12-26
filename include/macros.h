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
