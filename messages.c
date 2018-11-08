#include "defs.h"
#include "api.h"

/* offset may be 1 past the end of file (i.e., equal to file size) */
INTERNAL
int compute_lineno(File file, int offset)
{
        int i;
        int line = 1;

        for (i = 0; i < offset; i++)
                if (fileInfo[file].buf[i] == '\n')
                        line++;
        return line;
}

/* offset may be 1 past the end of file (i.e., equal to file size) */
INTERNAL
int compute_colno(File file, int offset)
{
        int i;
        int column = 1;

        for (i = 0; i < offset; i++)
                if (fileInfo[file].buf[i] == '\n')
                        column = 1;
                else
                        column++;
        return column;
}

INTERNAL
void find_expr_position(Expr x, File *file, int *offset)
{
        // TODO: should this be added as hard data to ExprInfo?
        Token tok = -1;
        for (;;) {
                switch (exprInfo[x].kind) {
                case EXPR_LITERAL:
                        tok = exprInfo[x].tLiteral.tok;
                        break;
                case EXPR_SYMREF:
                        tok = symrefInfo[exprInfo[x].tSymref.ref].tok;
                        break;
                case EXPR_UNOP:
                        tok = exprInfo[x].tUnop.tok;
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
                default:
                        UNHANDLED_CASE();
                }
                break;
        }
        assert(tok != -1);
        *file = tokenInfo[tok].file;
        *offset = tokenInfo[tok].offset;
}

void _msg_at_v(const char *srcfilename, int srcline,
        const char *lvl, File file, int offset,
        const char *fmt, va_list ap)
{
        const char *filepath = string_buffer(fileInfo[file].filepath);
        int line = compute_lineno(file, offset);
        int col = compute_colno(file, offset);
        _msg_begin(srcfilename, srcline, lvl);
        _msg_printf("At %s %d:%d: ", filepath, line, col);
        _msg_printfv(fmt, ap);
        _msg_end();
}

void _msg_at(const char *srcfilename, int srcline,
        const char *lvl, File file, int offset,
        const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        _msg_at_v(srcfilename, srcline, lvl, file, offset, fmt, ap);
        va_end(ap);
}

void _msg_at_tok(const char *srcfilename, int srcline,
        const char *lvl, Token tok,
        const char *fmt, ...)
{
        File file = tokenInfo[tok].file;
        int offset = tokenInfo[tok].offset;
        va_list ap;
        va_start(ap, fmt);
        _msg_at_v(srcfilename, srcline, lvl, file, offset, fmt, ap);
        va_end(ap);
}

void _msg_at_expr(const char *srcfilename, int srcline,
        const char *lvl, Expr expr,
        const char *fmt, ...)
{
        va_list ap;
        int file;
        int offset;

        find_expr_position(expr, &file, &offset);
        va_start(ap, fmt);
        _msg_at_v(srcfilename, srcline, lvl, file, offset, fmt, ap);
        va_end(ap);
}
