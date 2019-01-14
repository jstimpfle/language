#include "defs.h"
#include "api.h"

long long fold_integer_expr(Expr x)
{
        DEBUG("fold integer expression\n");
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
                                unopKindString[unopKind]);
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
                                "but %s is a %s\n", SS(symbol),
                                symbolKindString[symbolKind]);

                Constant constant = symbolInfo[symbol].tConstant;
                if (constantValue[constant].valueKind == -1)
                        FATAL_ERROR_AT_EXPR(x,
                                "Value of constant %s not known yet.\n",
                                SS(symbol));

                // TODO: I believe by now this must be a check, not an ASSERT
                ASSERT(constantValue[constant].valueKind == VALUE_INTEGER);
                return constantValue[constant].tInteger;
        }
        else if (exprKind == EXPR_SIZEOF) {
                Expr subexpr = exprInfo[x].tSizeof.expr;
                // TODO: is this the right way? Do we still need to check?
                check_expr_type(subexpr);
                Type tp = exprType[subexpr];
                ASSERT(tp != (Type) -1);  // XXX not sure
                long long size = get_type_size(tp);
                if (size == -1)
                        FATAL_ERROR_AT_EXPR(x,
                                "The size of this type is not yet known. "
                                "Try changing the order of declarations.\n");
                return size;
        }
        else if (exprKind == EXPR_LENGTHOF) {
                Expr subexpr = exprInfo[x].tSizeof.expr;
                // TODO: is this the right way? Do we still need to check?
                check_expr_type(subexpr);
                Type tp = exprType[subexpr];
                ASSERT(tp != (Type) -1);  // XXX not sure
                /* I think this should have been caught during an earlier type
                   check. */
                ASSERT(typeInfo[tp].typeKind == TYPE_ARRAY);
                long long length = typeInfo[tp].tArray.length;
                if (length == -1)
                        FATAL_ERROR_AT_EXPR(x,
                                "The array length is not yet known. "
                                "Try changing the order of declarations.\n");
                return length;
        }
        else {
                FATAL_ERROR_AT_EXPR(x, "Unhandled case: %s\n",
                                    exprKindString[exprKind]);
                UNHANDLED_CASE();
        }
}

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
