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
#include <syntax.h>
#include <macros.h>
#include <types.h>
#include <ir.h>
#include <codegen.h>
#include <x64.h>
#include <cmdline.h>


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
int cstr_equal(const char *a, const char *b);

void outc(char c);
void outs(const char *s);
void outf(const char *fmt, ...);
void NORETURN _abort(void);

#define ABORT() _abort()

/*
 * sourcefind.c
 */

int compute_lineno(File file, int offset);
int compute_colno(File file, int offset);
void compute_file_location_of_symbol(Symbol symbol, const char **filename, int *lineno, int *colno);
Token find_expr_token(Expr x);  /* find "first token of expression" */
void find_expr_position(Expr x, File *file, int *offset);

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

#define MSG(...) _msg(__FILE__, __LINE__, __VA_ARGS__)
#define MSG_AT(...) _msg_at(__FILE__, __LINE__, __VA_ARGS__)
#define MSG_AT_TOK(...) _msg_at_tok(__FILE__, __LINE__, __VA_ARGS__)
#define MSG_AT_EXPR(...) _msg_at_expr(__FILE__, __LINE__, __VA_ARGS__)

#define DEBUG(...) do { \
        if (doDebug) \
                MSG(lvl_debug, __VA_ARGS__); \
} while (0)

#define PARSE_LOG() do { \
        if (doDebug) \
                MSG_AT(lvl_debug, currentFile, currentOffset, \
                       "%s()\n", __func__); \
} while (0)

#define FATAL(...) do { \
        MSG(lvl_fatal, __VA_ARGS__); \
        exit_program(1); \
} while (0)

#define FATAL_ERROR_AT(...) do { \
        MSG_AT(lvl_fatal, __VA_ARGS__); \
        exit_program(1); \
} while (0)

#define FATAL_ERROR_AT_TOK(...) do { \
        MSG_AT_TOK(lvl_fatal, __VA_ARGS__); \
        exit_program(1); \
} while (0)

#define FATAL_ERROR_AT_EXPR(...) do { \
        MSG_AT_EXPR(lvl_fatal, __VA_ARGS__); \
        exit_program(1); \
} while (0)

#define FATAL_LEX_ERROR(...) \
        FATAL_ERROR_AT(currentFile, currentOffset, __VA_ARGS__)

#define FATAL_PARSE_ERROR(...) \
        FATAL_ERROR_AT_TOK((tokenCnt-1), __VA_ARGS__)

#define FATAL_PARSE_ERROR_AT_TOK(...) \
        FATAL_ERROR_AT_TOK(__VA_ARGS__)

#define ASSERT(x) do { \
        if (!(x)) { \
                MSG(lvl_internalerror, "Assertion failed: %s\n", #x); \
                ABORT(); \
        } \
} while (0)

#define ASSERT_FMT(x, ...) do { \
        if (!(x)) { \
                MSG(lvl_internalerror, ##__VA_ARGS__); \
                ABORT(); \
        } \
} while (0)

#define UNHANDLED_CASE() ASSERT_FMT(0, "Unhandled case!\n")


/* memory.c */

DATA long long DBG_totalbytesalloced;
DATA long long DBG_totalbytesrealloced;

void _buf_init(void **ptr, struct Alloc *alloc, int elsize,
               const char *file, int line);
void _buf_exit(void **ptr, struct Alloc *alloc, int elsize,
               const char *file, int line);
void _buf_reserve(void **ptr, struct Alloc *alloc, int nelems, int elsize,
                  int clear, const char *file, int line);

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

const char *string_buffer(String s);
int string_length(String s);
const char *SS(Symbol sym);
const char *SRS(Symref ref);
const char *TS(Token tok);

String intern_string(const void *buf, int len);
String intern_cstring(const char *str);

/* setup.c */
void setup_program(void);

/* teardown.c */
void teardown_program(void);

/* lex.c */
Token lex_token(void);

/* parse.c */
void parse_file(File file);
void fixup_parsed_data(void);

/* pprint.c */
void prettyprint(void);

/* resolve.c */
void ensure_there_are_no_symbol_collisions(void);
void resolve_symbol_references(void);
void resolve_type_references(void);

/* macroexpand.c */
void expand_macros(void);

/* typemetrics.c */
Type referenced_type(Type tp);//XXX try to get rid of this
int get_type_size(Type tp);
int get_struct_offset(Type tp, String memberName);
Type get_type_behind_pointer(Type tp);
void print_type(Type tp);
Type pointer_type(Type t);
Type string_type(void);
int type_equal(Type a, Type b);

/* typecheck.c */
Type check_expr_type(Expr x);
void check_stmt_types(Stmt stmt);

/* constantfold.c */
String fold_string_expr(Expr x);
long long fold_integer_expr(Expr x);

/* infer.c */
void infer_constants_and_types(void);
void infer_type(Type tp);
void infer_constant(Constant constant);

/* ifacegen.c */
void generate_interface_file(void);

/* compile.c */
void compile_to_IR(void);

/* irprint.c */
void irprint(void);

/* codegen.c */
void begin_symbol(Symbol sym);
void end_symbol(void);
Symdef emit_symdef(Symbol sym, int sectionKind, int offset, int size);
void emit_relative_relocation(Symbol symbol, int addend, int codepos);
void emit_relocation(Symbol symbol, int codepos);
void emit_section_relative_relocation(int sectionKind, int addend, int codepos);
void emit_bytes(int sectionKind, const void *buf, int size);

/* x64asm.c */
void codegen_x64(void);

/* elf64.c */
void write_elf_file(const char *outfilepath);

/* pecoff.c */
void write_pecoff_file(const char *outfilepath);
