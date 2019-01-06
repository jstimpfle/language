#include "defs.h"
#include "api.h"

INTERNAL void fold_constant(Constant constant);

INTERNAL
long long fold_integer_expr(Expr x)
{
        int exprKind = exprInfo[x].exprKind;
        if (exprKind == EXPR_LITERAL) {
                Token tok = exprInfo[x].tLiteral.tok;
                ASSERT(tokenInfo[tok].tokenKind == TOKEN_INTEGER);
                return tokenInfo[tok].tInteger.value;
        }
        else if (exprKind == EXPR_UNOP) {
                int unopKind = exprInfo[x].tUnop.unopKind;
                Expr expr = exprInfo[x].tUnop.expr;
                if (unopKind == UNOP_POSITIVE)
                        return fold_integer_expr(expr);
                else if (unopKind == UNOP_NEGATIVE) {
                        return - fold_integer_expr(expr);
                }
                else if (unopKind == UNOP_INVERTBITS ||
                         unopKind == UNOP_NOT) {
                        UNHANDLED_CASE();
                }
                else {
                        FATAL_ERROR_AT_EXPR(expr,
                                "Invalid constant expression: "
                                "%s expressions are not supported\n",
                                unopInfo[unopKind].str);
                }
        }
        else if (exprKind == EXPR_BINOP) {
                int binopKind = exprInfo[x].tBinop.binopKind;
                Expr e1 = exprInfo[x].tBinop.expr1;
                Expr e2 = exprInfo[x].tBinop.expr2;
                long long v1 = fold_integer_expr(e1);
                long long v2 = fold_integer_expr(e2);
                if (binopKind == BINOP_PLUS)
                        return v1 + v2;
                else if (binopKind == BINOP_MINUS)
                        return v1 - v2;
                else if (binopKind == BINOP_MUL)
                        return v1 * v2;
                else if (binopKind == BINOP_DIV)
                        return v1 / v2;
                else
                        UNHANDLED_CASE();
        }
        else if (exprKind == EXPR_SYMREF) {
                Symref ref = exprInfo[x].tSymref.ref;
                Symbol symbol = symrefToSym[ref];
                ASSERT(symbol != (Symbol) -1);
                int symbolKind = symbolInfo[symbol].symbolKind;
                if (symbolKind != SYMBOL_CONSTANT)
                        FATAL_ERROR_AT_EXPR(x,
                                "Reference to contant expected, "
                                "but found %s\n", symbolKindString[symbolKind]);

                Constant constant = symbolInfo[symbol].tConstant;
                fold_constant(constant);

                ASSERT(constantValue[constant].constantKind == CONSTANT_INTEGER);
                return constantValue[constant].tInteger;
        }
        else {
                FATAL_ERROR_AT_EXPR(x, "Unhandled case: %s\n",
                                    exprKindString[exprKind]);
                UNHANDLED_CASE();
        }
}

INTERNAL
String fold_string_expr(Expr x)
{
        if (exprInfo[x].exprKind == EXPR_LITERAL) {
                ASSERT(exprInfo[x].tLiteral.literalKind == LITERAL_STRING);
                return exprInfo[x].tLiteral.tString;
        }
        else {
                UNHANDLED_CASE();
        }
}

INTERNAL
void fold_constant(Constant constant)
{
        if (constantValue[constant].constantKind >= 0)
                return; /* already folded */

        Expr x = constantInfo[constant].expr;
        Type tp = exprType[x];
        if (type_equal(tp, builtinType[BUILTINTYPE_INT])) {
                constantValue[constant].tInteger = fold_integer_expr(x);
                constantValue[constant].constantKind = CONSTANT_INTEGER;
        }
        else if (type_equal(tp, pointer_type(builtinType[BUILTINTYPE_CHAR]))) {
                constantValue[constant].tString = fold_string_expr(x);
                constantValue[constant].constantKind = CONSTANT_STRING;
        }
        else {
                UNHANDLED_CASE();
        }
}

void fold_constants(void)
{
        RESIZE_GLOBAL_BUFFER(constantValue, constantCnt);

        /* mark as unprocessed: we need recursion to process unprocessed
         * constants immediately. Note that cycles should not happen when
         * recursing since that case should have been caught in the type
         * checking phase already. */
        for (Constant constant = 0; constant < constantCnt; constant++)
                constantValue[constant].constantKind = -1;

        for (Constant constant = 0; constant < constantCnt; constant++)
                fold_constant(constant);
}
