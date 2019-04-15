#include "defs.h"
#include "api.h"

/* offset may be 1 past the end of file (i.e., equal to file size) */
int compute_lineno(File file, int offset)
{
        int line = 1;
        for (int i = 0; i < offset; i++)
                if (fileInfo[file].buf[i] == '\n')
                        line++;
        return line;
}

/* offset may be 1 past the end of file (i.e., equal to file size) */
int compute_colno(File file, int offset)
{
        int column = 1;
        for (int i = 0; i < offset; i++)
                if (fileInfo[file].buf[i] == '\n')
                        column = 1;
                else
                        column++;
        return column;
}

void compute_file_location_of_symbol(
        Symbol symbol, const char **filename, int *lineno, int *colno)
{
        Token token = symbolToToken[symbol];
        if (token == (Token)-1) {
                *filename = "(Internal symbol)";
                *lineno = -1;
                *colno = -1;
        }
        else {
                ASSERT(0 <= token && token < tokenCnt);
                File file = tokenInfo[token].file;
                int offset = tokenInfo[token].offset;
                *filename = string_buffer(fileInfo[file].filepath);
                *lineno = compute_lineno(file, offset);
                *colno = compute_colno(file, offset);
        }
}

Token find_expr_token(Expr x)
{
        Token token = -1;
        for (;;) {
                switch (exprInfo[x].exprKind) {
                case EXPR_LITERAL:
                        token = exprInfo[x].tLiteral.tok;
                        break;
                case EXPR_SYMREF:
                        token = symrefToToken[exprInfo[x].tSymref.ref];
                        break;
                case EXPR_UNOP:
                        token = exprInfo[x].tUnop.tok;
                        break;
                case EXPR_BINOP:
                        x = exprInfo[x].tBinop.expr1;
                        continue;
                case EXPR_MEMBER:
                        x = exprInfo[x].tMember.expr;
                        continue;
                case EXPR_SUBSCRIPT:
                        x = exprInfo[x].tSubscript.expr1;
                        continue;
                case EXPR_CALL:
                        x = exprInfo[x].tCall.callee;
                        continue;
                case EXPR_COMPOUND:
                        token = exprInfo[x].tCompound.initialToken;
                        break;
                case EXPR_COMPILERVALUE:
                        token = exprInfo[x].tCompilervalue.hashtok;
                        break;
                case EXPR_COMPILERCALL:
                        token = exprInfo[x].tCompilercall.hashtok;
                        break;
                default:
                        MSG("expr kind error: %s\n",
                                exprKindString[exprInfo[x].exprKind]);
                        UNHANDLED_CASE();
                }
                break;
        }
        ASSERT(token != -1);
        return token;
}

void find_expr_position(Expr x, File *file, int *offset)
{
        Token token = find_expr_token(x);
        *file = tokenInfo[token].file;
        *offset = tokenInfo[token].offset;
}
