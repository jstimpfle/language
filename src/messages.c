#include "defs.h"
#include "api.h"
#include <stdarg.h>
#include <stdio.h>

void _msg_begin(const char *srcfilename, int srcline,
                const char *loglevel)
{
        fprintf(stdout, "%s:%d:\t", srcfilename, srcline);
        fprintf(stdout, "%s: ", loglevel);
}

void _msg_printfv(const char *fmt, va_list ap)
{
        vfprintf(stdout, fmt, ap);
}

void _msg_printf(const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        _msg_printfv(fmt, ap);
        va_end(ap);
}

void _msg_end(void)
{
        fflush(stdout);
}

void _vmsg(const char *srcfilename, int srcline,
           const char *loglevel, const char *fmt, va_list ap)
{
        _msg_begin(srcfilename, srcline, loglevel);
        _msg_printfv(fmt, ap);
        _msg_end();
}

void _msg(const char *srcfilename, int srcline,
          const char *loglevel, const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        _vmsg(srcfilename, srcline, loglevel, fmt, ap);
        va_end(ap);
}

void _vmsg_at(const char *srcfilename, int srcline,
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
        _vmsg_at(srcfilename, srcline, lvl, file, offset, fmt, ap);
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
        _vmsg_at(srcfilename, srcline, lvl, file, offset, fmt, ap);
        va_end(ap);
}

void _msg_at_expr(const char *srcfilename, int srcline,
        const char *lvl, Expr expr,
        const char *fmt, ...)
{
        va_list ap;
        File file;
        int offset;

        find_expr_position(expr, &file, &offset);
        va_start(ap, fmt);
        _vmsg_at(srcfilename, srcline, lvl, file, offset, fmt, ap);
        va_end(ap);
}
