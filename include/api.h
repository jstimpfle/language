#ifdef API_H_INCLUDED
  #error Header api.h included twice!!!
#endif
#define API_H_INCLUDED 1


#ifdef DATA_IMPL
#define DATA
#else
#define DATA extern
#endif

/*
 *
 */

#include <stuff.h>
#include <token.h>
#include <operators.h>
typedef int Type; // needed in syntax. TODO think about dependencies
#include <syntax.h>
#include <macros.h>
#include <types.h>
#include <ir.h>
#include <codegen.h>
#include <x64.h>
#include <cmdline.h>


/* init.c */
void init_data(void);


/*
 * io.c
 */

NORETURN void exit_program(int exitcode);

void read_whole_file(File file);
void clear_mem(void *ptr, int size);
void copy_mem(void *dst, const void *src, int size);
int compare_mem(const void *m1, const void *m2, int size);
void sort_array(void *ptr, int nelems, int elemsize,
                int (*compare)(const void*, const void*));

#define LENGTH(a) ((int) (sizeof (a) / sizeof (a)[0]))
#define SORT(a, n, cmp) sort_array((a), (n), sizeof *(a), (cmp))
#define CLEAR(x) clear_mem(&(x), sizeof (x))
#define COPY(x, y) copy_mem((x), (y), sizeof (x))

int cstr_length(const char *s);
int cstr_compare(const char *s1, const char *m2);

static inline int cstr_equal(const char *a, const char *b)
{
        return cstr_compare(a, b) == 0;
}

void outc(char c);
void outs(const char *s);
void outf(const char *fmt, ...);
void NORETURN _abort(void);
void NORETURN _abort_on_failed_assertion(const char * assertion,
                 const char * file, unsigned int line, const char * function);

#define MSG(lvl, fmt, ...) _msg(__FILE__, __LINE__, (lvl), (fmt), ##__VA_ARGS__)
#define ABORT() _abort()
#define DEBUG(...) do { \
        if (doDebug) \
                _msg(__FILE__, __LINE__, "DEBUG", __VA_ARGS__); \
} while (0)

#define ASSERT(x) do { \
        if (!(x)) \
                _abort_on_failed_assertion(#x, __FILE__, __LINE__, __func__); \
} while (0)


/*
* messages.c
*/

void _msg(const char *filename, int line,
        const char *loglevel, const char *fmt, ...);
void _msg_begin(const char *srcfilename, int srcline,
        const char *loglevel);
void _msg_printf(const char *fmt, ...);
void _msg_end(void);

void _msg_at(const char *srcfilename, int srcline,
        const char *lvl, File file, int offset,
        const char *fmt, ...);
void _msg_at_tok(const char *srcfilename, int srcline,
        const char *lvl, Token tok,
        const char *fmt, ...);
void _msg_at_expr(const char *srcfilename, int srcline,
        const char *lvl, Expr expr,
        const char *fmt, ...);

void NORETURN _fatal(const char *filename, int line, const char *fmt, ...);

#define MSG_AT(...) _msg_at(__FILE__, __LINE__, __VA_ARGS__)
#define MSG_AT_TOK(...) _msg_at_tok(__FILE__, __LINE__, __VA_ARGS__)
#define MSG_AT_EXPR(...) _msg_at_expr(__FILE__, __LINE__, __VA_ARGS__)

#define PARSE_LOG() do { \
        if (doDebug) \
                MSG_AT(lvl_debug, currentFile, currentOffset, \
                       "%s()\n", __func__); \
} while (0)

#define FATAL_PARSE_ERROR_AT(...) do { \
        MSG_AT(lvl_fatal, __VA_ARGS__); \
        ABORT(); \
} while (0)

#define FATAL_PARSE_ERROR_AT_TOK(...) do { \
        MSG_AT_TOK(lvl_fatal, __VA_ARGS__); \
        ABORT(); \
} while (0)

#define FATAL(fmt, ...) _fatal(__FILE__, __LINE__, (fmt), ##__VA_ARGS__)
#define UNHANDLED_CASE() FATAL("Unhandled case!\n");


/* memory.c */

void _buf_init(void **ptr, struct Alloc *alloc, int elsize,
               const char *UNUSED file, int UNUSED line);
void _buf_exit(void **ptr, struct Alloc *alloc, int elsize,
               const char *UNUSED file, int UNUSED line);
void _buf_reserve(void **ptr, struct Alloc *alloc, int nelems, int elsize,
                  int clear, const char *UNUSED file, int UNUSED line);

#define BUF_INIT(buf, alloc) \
        _buf_init((void**)(buf), (alloc), sizeof **(buf), __FILE__, __LINE__);

#define BUF_EXIT(buf, alloc) \
        _buf_exit((void**)(buf), (alloc), sizeof **(buf), __FILE__, __LINE__);

#define BUF_RESERVE(buf, alloc, cnt) \
        _buf_reserve((void**)(buf), (alloc), (cnt), sizeof **(buf), 0, \
                     __FILE__, __LINE__);

#define BUF_RESERVE_Z(buf, alloc, cnt) \
        _buf_reserve((void**)(buf), (alloc), (cnt), sizeof **(buf), 1, \
                     __FILE__, __LINE__);

void _resize_global_buffer(int buf, int nelems, int clear);
void _resize_global_buffer_dbg(int buf, int nelems, int clear,
                               const char *filename, int line);
#define RESIZE_GLOBAL_BUFFER(bufname, nelems) \
        _resize_global_buffer(BUFFER_##bufname, (nelems), 0)


/*
 * str.c
 */

static inline const char *string_buffer(String s)
{
        return &strbuf[stringInfo[s].pos];
}

static inline int string_length(String s)
{
        return stringInfo[s+1].pos - stringInfo[s].pos - 1;
}

static inline const char *SS(Symbol sym)
{
        return string_buffer(symbolInfo[sym].name);
}

static inline const char *SRS(Symref ref)
{
        return string_buffer(symrefInfo[ref].name);
}

static inline const char *TS(Token tok)
{
        return string_buffer(tokenInfo[tok].tWord.string);
}

String intern_string(const void *buf, int len);
String intern_cstring(const char *str);

/* lex.c */
Token lex_token(void);

/* parse.c */
void initialize_pseudo_constant_data(void);
void parse_global_scope(void);

/* pprint.c */
void prettyprint(void);

/* resolve.c */
void resolve_symbol_references(void);
void resolve_type_references(void);

/* macroexpand.c */
void expand_macros(void);

/* typemetrics.c */
Type referenced_type(Type tp);//XXX try to get rid of this
int get_type_size(Type tp);

/* typecheck.c */
void check_types(void);

/* compile.c */
void compile_to_IR(void);

/* irprint.c */
void irprint(void);

/* x64asm.c */
void codegen_x64(void);

/* elf64.c */
void write_elf64_object(const char *outfilepath);

/* pe64.c */
void write_pe64_object(const char *outfilepath);

/* main.c */
void cleanup(void);
