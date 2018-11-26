/* ELF-64 writer. Lots of comments pulled from the spec ("ELF-64 Object File
 * Format, Version 1.5 Draft 2"). We try to be independent of the rest of the
 * infrastructure in the project, so this file will be reusable as a baseline in
 * other projects with only small changes. */

#include "defs.h"
#include "api.h"
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

/* Table 1. ELF-64 Data Types */
typedef unsigned char Elf64_Uchar;
typedef uint64_t Elf64_Addr;
typedef uint64_t Elf64_Off;
typedef uint16_t Elf64_Half;
typedef uint32_t Elf64_Word;
typedef int32_t Elf64_Sword;
typedef uint64_t Elf64_Xword;
typedef int64_t Elf64_Sxword;

/* Figure 2. Elf-64 File Header (must be at position 0 in file) */
struct Elf64_Ehdr {
        /* e_ident identifies the file as an ELF object file, and provides
         * information about the data representation of the object file
         * structures. The bytes of this array that have defined meanings are
         * detailed below. The remaining bytes are reserved for future use, and
         * should be set to zero. Each byte of the array is indexed symbolically
         * using the names in the Table 2. */
        /* e_ident[EI_MAG0] through e_ident[EI_MAG3] contain a "magic number,"
         * identifying the file as an ELF object file. They contain the
         * characters '\x7f', 'E', 'L', and 'F', respectively. */
        /* e_ident[EI_CLASS] identifies the class of the object file, or its
         * capacity. Table 3 lists the possible values.
         *
         * This document describes the structures for ELFCLASS64.
         *
         * The class of the ELF file is independent of the data model assumed by
         * the object code. The EI_CLASS field identifies the file format; a
         * processor-specific flag in the e_flags field, described below, may be
         * used to identify the application's data model if the processor
         * supports multiple models. */
        /* e_ident[EI_DATA] specifies the data encoding of the object file data
         * structures. Table 4 lists the encodings defined for ELF-64. */
        /* e_ident[EI_VERSION] identifies the version of the object file format.
         * Currently, this field has the value EV_CURRENT, which is defined with
         * the value 1. */
        /* e_ident[EI_ABIVERSION] identifies the version of the ABI for which
         * the object is prepared. This field is used to distinguish among
         * incompatible versions of an ABI. The interpretation of this version
         * number is dependent on the ABI identified by the EI_OSABI field.
         *
         * For applications conforming to the System V ABI, third edition, this
         * field should contain 0. */
        /* e_type identifies the object file type. The processor-independent
         * values for this field are listed in Table 6. */
        /* e_machine identifies the target architecture. These values are define
         * in the processor-specific supplements. (XXX: where is this???) */
        /* e_version identifies the version of the object file format.
         * Currently, this field has the value EV_CURRENT, which is defined with
         * the value 1. */
        /* e_entry contains the virtual address of the program entry point. If
         * there is no entry point, this field contains zero. */
        /* e_phoff contains the file offset, in bytes, of the program header
         * table. */
        /* e_shoff contains the file offset, in bytes, of the section header
         * table. */
        /* e_flags contains processor specific flags */
        /* e_ehsize contains the size, in bytes, of the ELF header. */
        /* e_phentsize contains the size, in bytes, of a program header table */
        /* e_phnum contains the number of entries in the program header table */
        /* e_shentsize contains the size, in bytes, of a section header table */
        /* e_shnum contains the number of entries in the section header table */
        /* e_shstrndx contains the section header table index of the section
         * containing the section name string table If there is no section name
         * string table, this field has the value SHN_UNDEF. */
        unsigned char   e_ident[16];    /* ELF identification */
        Elf64_Half      e_type;         /* Object file type */
        Elf64_Half      e_machine;      /* Machine type */
        Elf64_Word      e_version;      /* Object file version */
        Elf64_Addr      e_entry;        /* Entry point address */
        Elf64_Off       e_phoff;        /* Program header offset */
        Elf64_Off       e_shoff;        /* Section header offset */
        Elf64_Word      e_flags;        /* Processor-specific flags */
        Elf64_Half      e_ehsize;       /* ELF header size */
        Elf64_Half      e_phentsize;    /* Size of program header entries */
        Elf64_Half      e_phnum;        /* Number of program header entries */
        Elf64_Half      e_shentsize;    /* Size of section header entries */
        Elf64_Half      e_shnum;        /* Number of section header entries */
        Elf64_Half      e_shstrndx;     /* Index of .shstrtab section */
};

/* Figure 3. ELF-64 Section header */
struct Elf64_Shdr {
        /* sh_name contains the offset, in bytes, to the section name, relative
         * to the start of the section name string table. */
        /* sh_type identifies the section type. Table 8 lists the
         * processor-independent values for this field. */
        /* sh_flags identifies the attributes of the section. Table 9 lists the
         * processor independent values for these flags. */
        /* sh_addr contains the virtual address of the beginning of the section
         * in memory. If the section is not allocated to the memory image of the
         * program, this field should be zero. */
        /* sh_offset contains the offset, in bytes, of the beginning of the
         * section contents in the file. */
        /* sh_size contains the size, in bytes, of the section. Except for
         * SHT_NOBITS sections, this is the amount of space occupied in the
         * file. */
        /* sh_link contains the section index of an associated section. This
         * field is used for several purposes, depending on the type of section,
         * as explained in Table 10. */
        /* sh_link contains extra information about the section. This field is
         * used for several purposes, depending on the type of section, as
         * explained in Table 11. */
        /* sh_addralign contains the required alignment of the section. This
         * field must be a power of two. */
        /* sh_entsize contains the size, in bytes, of each entry, for sections
         * that contain fixed-size entries. Otherwise, this field contains zero.
         */
        Elf64_Word      sh_name;        /* Section name */
        Elf64_Word      sh_type;        /* Section type */
        Elf64_Xword     sh_flags;       /* Section attributes */
        Elf64_Addr      sh_addr;        /* Virtual address in memory */
        Elf64_Off       sh_offset;      /* Offset in file */
        Elf64_Xword     sh_size;        /* Size of section */
        Elf64_Word      sh_link;        /* Link to other section */
        Elf64_Word      sh_info;        /* Miscellaneous information */
        Elf64_Xword     sh_addralign;   /* Address alignment boundary */
        Elf64_Xword     sh_entsize;     /* Size of entries, if section has table */
};

/* Figure 4. ELF-64 Symbol table entry */
struct Elf64_Sym {
        /* st_name contains the offset, in bytes, to the symbol name, relative
         * to the start of the symbol string table. If this field contains zero,
         * the symbol has no name. */
        /* st_info contains the symbol type and its binding attributes (that is,
         * its scope). The binding attributes are contained in the high-order
         * four bits of the eight-bit byte, and the symbol type is contained in
         * the low-order four bits. The processor-independent binding attributes
         * are listed in Table 14, and the processor-independent values for
         * symbol type are listed in Table 15. */
        /* st_other is reserved for future use; must be zero. */
        /* st_shndx contains the section index of the section in which the
         * symbol is "defined." For undefined symbols, this field contains
         * SHN_UNDEF; for absolute symbols, it contains SHN_ABS; and for common
         * symbols, it contains, SHN_COMMON. */
        /* st_value contains the value of the symbol. This may be an absolute
         * value or a relocatable address.
         *
         * In relocatable files, this field contains the alignment constraint
         * for common symbols, and a section-relative offset for defined
         * relocatable symbols.
         *
         * In executable and shared object files, this field contains a virtual
         * address for defined relocatable symbols. */
        /* st_size contains the size associated with the symbol. If a symbol
         * does not have an associated size, or the size is unknown, this field
         * contains zero. */
        Elf64_Word      st_name;        /* Symbol name */
        unsigned char   st_info;        /* Type and Binding attributes */
        unsigned char   st_other;       /* Reserved */
        Elf64_Half      st_shndx;       /* Section table index */
        Elf64_Addr      st_value;       /* Symbol value */
        Elf64_Xword     st_size;        /* Size of object (e.g., common) */
};

/* Figure 5. ELF-64 Relocation Entries (Elf64_Rel and Elf64_Rela) */
struct Elf64_Rel {
        /* r_offset indicates the location at which the relocation should be
         * applied. For a relocatable file, this is the offset, in bytes, from
         * the beginning of the section to the beginning of the storage unit
         * being relocated. For an executable or shared object, this is the
         * virtual address of the storage unit being relocated. */
        /* r_info contains both a symbol table index and a relocation type. The
         * symbol table index identifies the symbol whose value should be used
         * in the relocation. Relocation types are processor specific. The
         * symbol table index is obtained by applying the ELF64_R_SYM macro to
         * this field, and the relocation type is obtained by applying the
         * ELF64_R_TYPE macro to this field. The ELF64_R_INFO macro combines a
         * symbol table index and a relocation type to produce a value for this
         * field. */
        /* r_addend (only struct Elf64_Rela) specifies a constant addend used to
         * compute the value to be stored in the relocated field. */
        Elf64_Addr      r_offset;       /* Address of reference */
        Elf64_Xword     r_info;         /* Symbol index and type of relocation */
};

struct Elf64_Rela {
        Elf64_Addr      r_offset;       /* Address of reference */
        Elf64_Xword     r_info;         /* Symbol index and type of relocation */
        Elf64_Sxword    r_addend;       /* Constant part of expression */
};

/* Figure 6. ELF-64 Program Header Table Entry */
struct Elf64_Phdr {
        /* p_type identifies the type of segment. The processor-independent
         * segment types are shown in Table 16. */
        /* p_flags contains the segment attributes. The processor-independent
         * flags are shown in Table 17. The top eight bits are reserved for
         * processor-specific use, and the next eight bits are reserved for
         * environment-specific use. */
        /* p_offset contains the offset, in bytes, of the segment from the
         * beginning of the file. */
        /* p_vaddr contains the virtual address of the segment in memory. */
        /* p_paddr is reserved for systems with physical addressing. */
        /* p_filesz contains the size, in bytes, of the file image of the
         * segment. */
        /* p_memsz contains the size, in bytes, of the memory image of the
         * segment */
        /* p_align specifies the alignment constraint for the segment. Must be a
         * power of two. The values of p_offset and p_vaddr must be congruent
         * modulo the alignment. */
        Elf64_Word      p_type;         /* Type of segment */
        Elf64_Word      p_flags;        /* Segment attributes */
        Elf64_Off       p_offset;       /* Offset in file */
        Elf64_Addr      p_vaddr;        /* Virtual address in memory */
        Elf64_Addr      p_paddr;        /* Reserved */
        Elf64_Xword     p_filesz;       /* Size of segment in file */
        Elf64_Xword     p_memsz;        /* Size of segment in memory */
        Elf64_Xword     p_align;        /* Alignment of segment */
};

/* (macros mentioned above) */
#define ELF64_R_SYM(i)  ((i) >> 32)
#define ELF64_R_TYPE(i) ((i) & 0xffffffffL)
#define ELF64_R_INFO(s, t) (((Elf64_Xword) (s) << 32) + ((t) & 0xffffffffL))

/* Table 2. Names for the individual bytes in (struct Elf64_Ehdr).e_ident */
#define EI_MAG0         0       /* File identification */
#define EI_MAG1         1       /* (ditto) */
#define EI_MAG2         2       /* (ditto) */
#define EI_MAG3         3       /* (ditto) */
#define EI_CLASS        4       /* File class */
#define EI_DATA         5       /* Data encoding */
#define EI_VERSION      6       /* File version */
#define EI_OSABI        7       /* OS/ABI identifation */
#define EI_ABIVERSION   8       /* ABI version */
#define EI_PAD          9       /* Start of padding bytes */
#define EI_NIDENT       16      /* Size of e_ident[] */

/* Table 3. Object File Classes, e_ident[EI_CLASS] */
#define ELFCLASS32      1       /* 32-bit objects */
#define ELFCLASS64      2       /* 64-bit objects */

/* Table 4. Data Encodings, e_ident[EI_DATA] */
#define ELFDATA2LSB     1       /* Object file data structures are little-endian */
#define ELFDATA2MSB     2       /* Object file data structures are big-endian */

/* (No table number) Possible values for (struct Elf64_Ehdr).eident[EI_VERSION] */
#define EV_CURRENT 1

/* Table 5. Operating System and ABI Identifiers, e_ident[EI_OSABI] */
#define ELFOSABI_SYSV           0       /* System V ABI */
#define ELFOSABI_HPUX           1       /* HP-UX operating system */
#define ELFOSABI_STANDALONE     255     /* Standalone (embedded) application */

/* Table 6. Possible values for (struct Elf64_Ehdr).e_type */
#define ET_NONE         0       /* No file type */
#define ET_REL          1       /* Relocatable object file */
#define ET_EXEC         2       /* Executable file */
#define ET_DYN          3       /* Shared object file */
#define ET_CORE         4       /* Core file */
#define ET_LOOS         0xFE00  /* Environment-specific use */
#define ET_HIOS         0xFEFF  /* (ditto) */
#define ET_LOPROC       0xFF00  /* Processor-specific use */
#define ET_HIPROC       0xFFFF  /* (ditto) */

/* Table 7. Special Section Indices */
#define SHN_UNDEF       0       /* Used to mark an undefined or meaningless
                                   section reference */
#define SHN_LOPROC      0xFF00  /* Processor-specific use */
#define SHN_HIPROC      0xFF1F  /* (ditto) */
#define SHN_LOOS        0xFF20  /* Environment-specific use */
#define SHN_HIOS        0xFF3F  /* (ditto) */
#define SHN_ABS         0xFFF1  /* Indicates that the corresponding reference is
                                   an absolute value */
#define SHN_COMMON      0xFFF2  /* Indicates a symbol that has been declared as
                                   a common block (Fortran COMMON or C tentative
                                   declaration) */

/* Table 8. Section Types, sh_type */
#define SHT_NULL        0       /* Marks an unused section header */
#define SHT_PROGBITS    1       /* Contains information defined by the program
                                */
#define SHT_SYMTAB      2       /* Contains a linker symbol table */
#define SHT_STRTAB      3       /* Contains a string table */
#define SHT_RELA        4       /* Contains "Rela" type relocation entries */
#define SHT_HASH        5       /* Contains a symbol hash table */
#define SHT_DYNAMIC     6       /* Contains dynamic linking table */
#define SHT_NOTE        7       /* Contains note information */
#define SHT_NOBITS      8       /* Contains uninitialized space; does not occupy
                                   any space in the file */
#define SHT_REL         9       /* Contains "Rel" type relocation entries */
#define SHT_SHLIB       10      /* Reserved */
#define SHT_DYNSYM      11      /* Contains a dynamic loader symbol table */
#define SHT_LOOS        0x60000000      /* Environment specific use */
#define SHT_HIOS        0x6FFFFFFF      /* (ditto) */
#define SHT_LOPROC      0x70000000      /* Processor specific use */
#define SHT_HIPROC      0x7FFFFFFF      /* (ditto) */

/* Table 9. Section Attributes, sh_flags */
#define SHF_WRITE       0x1     /* Section contains writable data */
#define SHF_ALLOC       0x2     /* Section is allocated in memory image of
                                   program */
#define SHF_EXECINSTR   0x4     /* Section contains executable instructions */
#define SHF_MASKOS      0x0F000000      /* Environment-specific use */
#define SHF_MASKPROC    0xF0000000      /* Processor-specific use */

/* Table 10. Use of the sh_link Field
SECTION TYPE            ASSOCIATED SECTION
============================================
SHT_DYNAMIC             String table used by entries in this section
SHT_HASH                Symbol table to which the hash table applies
SHT_REL                 Symbol table referenced by relocations
SHT_RELA                (ditto)
SHT_SYMTAB              String table used by entries in this section
SHT_DYNSYM              (ditto)
Other                   SHN_UNDEF
*/

/* Table 11. Use of the sh_info Field
SECTION TYPE            sh_info
=================================
SHT_REL                 Section index of section to which the relocations apply
SHT_RELA                (ditto)
SHT_SYMTAB              Index of first non-local symbol (i.e., number of local symbols)
SHT_DYNSYM              (ditto)
Other                   0
*/


/* Table 14. Symbol Bindings */
#define STB_LOCAL       0       /* Not visible outside the object file */
#define STB_GLOBAL      1       /* Global symbol, visible to all object files */
#define STB_WEAK        2       /* Global scope, but with lower precedence than
                                   global symbols */
#define STB_LOOS        10      /* Environment-specific use */
#define STB_HIOS        12      /* (ditto) */
#define STB_LOPROC      13      /* Processor-specific use */
#define STB_HIPROC      15      /* (ditto) */

/* Table 15. Symbol Types */
#define STT_NOTYPE      0       /* No type specified (e.g., an absolute symbol) */
#define STT_OBJECT      1       /* Data object */
#define STT_FUNC        2       /* Function entry point */
#define STT_SECTION     3       /* Symbol is associated with a section */
#define STT_FILE        4       /* Source file associated with the object file */
#define STT_LOOS        10      /* Environment-specific use */
#define STT_HIOS        12      /* (ditto) */
#define STT_LOPROC      13      /* Processor-specific use */
#define STT_HIPROC      15      /* (ditto) */

/* Table 16. Segment Types, p_type */
#define PT_NULL         0       /* Unused entry */
#define PT_LOAD         1       /* Loadable segment */
#define PT_DYNAMIC      2       /* Dynamic linking tables */
#define PT_INTERP       3       /* Program interpreter path name */
#define PT_NOTE         4       /* Note sections */
#define PT_SHLIB        5       /* Reserved */
#define PT_PHDR         6       /* Program header table */
#define PT_LOOS         0x60000000      /* Environment-specific use */
#define PT_HIOS         0x6FFFFFFF      /* (ditto) */
#define PT_LOPROC       0x70000000      /* Processor-specific use */
#define PT_HIPROC       0x7FFFFFFF      /* (ditto) */

/* Table 17. Segment Attributes, p_flags */
#define PF_X            0x1     /* Execute permission */
#define PF_W            0x2     /* Write permission */
#define PF_R            0x4     /* Read permission */
#define PF_MASKOS       0x00FF0000      /* These flag bits are reserved for
                                           environment-specific use */
#define PF_MASKPROC     0xFF000000      /* These flag bits are reserved for
                                           processor-specific use */


/* Taken from somewhere else: x64 Relocation Types */
/*      Name            Value      Field    Calculation (S=Symbol, A=Addend) */
#define R_AMD64_NONE        0   /* None     None */
#define R_AMD64_64          1   /* word64   S + A  */
#define R_AMD64_PC32        2   /* word32   S + A - P  */
#define R_AMD64_GOT32       3   /* word32   G + A  */
#define R_AMD64_PLT32       4   /* word32   L + A - P  */
#define R_AMD64_COPY        5   /* None     Refer to the explanation following
                                            this table.   */
#define R_AMD64_GLOB_DAT    6   /* word64   S  */
#define R_AMD64_JUMP_SLOT   7   /* word64   S  */
#define R_AMD64_RELATIVE    8   /* word64   B + A  */
#define R_AMD64_GOTPCREL    9   /* word32   G + GOT + A - P  */
#define R_AMD64_32         10   /* word32   S + A  */
#define R_AMD64_32S        11   /* word32   S + A  */
#define R_AMD64_16         12   /* word16   S + A  */
#define R_AMD64_PC16       13   /* word16   S + A - P  */
#define R_AMD64_8          14   /* word8    S + A  */
#define R_AMD64_PC8        15   /* word8    S + A - P  */
#define R_AMD64_PC64       24   /* word64   S + A - P  */
#define R_AMD64_GOTOFF64   25   /* word64   S + A - GOT  */
#define R_AMD64_GOTPC32    26   /* word32   GOT + A + P  */
#define R_AMD64_SIZE32     32   /* word32   Z + A  */
#define R_AMD64_SIZE64     33   /* word64   Z + A  */

void write_Elf64_Uchar(Elf64_Uchar x, FILE *f)
{
        fputc(x, f);
}

/* I *hope* this is portable serialization. To make sure we're serializing in
 * little-endian order, we're writing byte-by-byte. And since shifting signed
 * integers is implementation defined, we cast to the unsigned type uint64_t.
 * Which also means that we can't serialize values that are not representable in
 * uint64_t. */
#define DEFINE_WRITE_FUNCTION(n, t) \
void n(t x, FILE *f) \
{ \
        uint64_t y = x; \
        ASSERT((t) y == x); \
        for (size_t i = 0; i < sizeof (x); i++) { \
                fputc(y, f); \
                y >>= 8; \
        } \
}

DEFINE_WRITE_FUNCTION(write_Elf64_Addr, Elf64_Addr)
DEFINE_WRITE_FUNCTION(write_Elf64_Off, Elf64_Off)
DEFINE_WRITE_FUNCTION(write_Elf64_Half, Elf64_Half)
DEFINE_WRITE_FUNCTION(write_Elf64_Word, Elf64_Word)
DEFINE_WRITE_FUNCTION(write_Elf64_Sword, Elf64_Sword)
DEFINE_WRITE_FUNCTION(write_Elf64_Xword, Elf64_Xword)
DEFINE_WRITE_FUNCTION(write_Elf64_Sxword, Elf64_Sxword)

void write_Elf64_Ehdr(const struct Elf64_Ehdr *h, FILE *f)
{
        fwrite(h->e_ident, sizeof h->e_ident, 1, f);
        write_Elf64_Half(h->e_type, f);
        write_Elf64_Half(h->e_machine, f);
        write_Elf64_Word(h->e_version, f);
        write_Elf64_Addr(h->e_entry, f);
        write_Elf64_Off(h->e_phoff, f);
        write_Elf64_Off(h->e_shoff, f);
        write_Elf64_Word(h->e_flags, f);
        write_Elf64_Half(h->e_ehsize, f);
        write_Elf64_Half(h->e_phentsize, f);
        write_Elf64_Half(h->e_phnum, f);
        write_Elf64_Half(h->e_shentsize, f);
        write_Elf64_Half(h->e_shnum, f);
        write_Elf64_Half(h->e_shstrndx, f);
}

void write_Elf64_Shdr(const struct Elf64_Shdr *s, FILE *f)
{
        write_Elf64_Word(s->sh_name, f);
        write_Elf64_Word(s->sh_type, f);
        write_Elf64_Xword(s->sh_flags, f);
        write_Elf64_Addr(s->sh_addr, f);
        write_Elf64_Off(s->sh_offset, f);
        write_Elf64_Xword(s->sh_size, f);
        write_Elf64_Word(s->sh_link, f);
        write_Elf64_Word(s->sh_info, f);
        write_Elf64_Xword(s->sh_addralign, f);
        write_Elf64_Xword(s->sh_entsize, f);
}

void write_Elf64_Sym(const struct Elf64_Sym *esym, FILE *f)
{
        write_Elf64_Word(esym->st_name, f);
        write_Elf64_Uchar(esym->st_info, f);
        write_Elf64_Uchar(esym->st_other, f);
        write_Elf64_Half(esym->st_shndx, f);
        write_Elf64_Addr(esym->st_value, f);
        write_Elf64_Xword(esym->st_size, f);
}

void write_Elf64_Rela(const struct Elf64_Rela *rela, FILE *f)
{
        write_Elf64_Addr(rela->r_offset, f);
        write_Elf64_Xword(rela->r_info, f);
        write_Elf64_Sxword(rela->r_addend, f);
}

struct ElfStringTable {
        char *buf;
        size_t size;
        size_t allocated_size;
        size_t numstrings;
};

void init_ElfStringTable(struct ElfStringTable *t)
{
        t->buf = NULL;
        t->size = 0;
        t->allocated_size = 0;
        t->numstrings = 0;
}

void exit_ElfStringTable(struct ElfStringTable *t)
{
        free(t->buf);
        memset(t, 0, sizeof *t);
}

size_t append_to_ElfStringTable(struct ElfStringTable *t, const char *str)
{
        size_t len = strlen(str);
        if (t->size + len + 1 > t->allocated_size) {
                size_t new_allocated_size = (t->size + len + 1) * 2;
                char *buf = realloc(t->buf, new_allocated_size);
                if (!buf) {
                        fprintf(stderr, "ERROR: OOM!\n");
                        abort();
                }
                t->buf = buf;
                t->allocated_size = new_allocated_size;
        }
        size_t result = t->size;
        memcpy(t->buf + t->size, str, len + 1);
        t->size += len + 1;
        t->numstrings++;
        return result;
}

/* The sections that we use here. TODO: .reltext, .relatext (and same for bss,
 * data) for relocations. The order is important. It is the order of appearance
 * in the object file that we write. */
enum {
        ES_DUMMY,
        ES_SYMTAB,
        ES_TEXT,
        ES_BSS,
        ES_RODATA,
        ES_DATA,
        ES_RELATEXT,
        ES_STRTAB,
        ES_SHSTRTAB,
        NUM_ESS,
};


/* Map the section identifiers from codegen.h to the ES_* sections here */
const int sectionToElfsection[NUM_SECTIONS] = {
        [SECTION_CODE] = ES_TEXT,
        [SECTION_DATA] = ES_DATA,
        [SECTION_RODATA] = ES_RODATA,
        [SECTION_ZERODATA] = ES_BSS,
};

const char *const sectionNames[NUM_ESS] = {
        [ES_DUMMY   ] = "",
        [ES_SYMTAB  ] = ".symtab",
        [ES_TEXT    ] = ".text",
        [ES_BSS     ] = ".bss",
        [ES_RODATA  ] = ".rodata",
        [ES_DATA    ] = ".data",
        [ES_RELATEXT] = ".rela.text",
        [ES_STRTAB  ] = ".strtab",
        [ES_SHSTRTAB] = ".symstrtab",
};

/* for each ELF section we also make an ELF symbol so we can reference it */
int elfsectionToElfsym[NUM_ESS];


void write_elf64_object(const char *outfilepath)
{
        struct ElfStringTable shstrtabStrings;
        struct ElfStringTable strtabStrings;

        struct Elf64_Sym *elfsym;
        struct Elf64_Rela *relaText;
        int *symbolToElfsym;

        struct Alloc elfsymAlloc;
        struct Alloc relaTextAlloc;
        struct Alloc symbolToElfsymAlloc;

        int elfsymCnt = 0;
        int relaTextCnt = 0;

        init_ElfStringTable(&shstrtabStrings);
        init_ElfStringTable(&strtabStrings);
        BUF_INIT(&elfsym, &elfsymAlloc);
        BUF_INIT(&relaText, &relaTextAlloc);
        BUF_INIT(&symbolToElfsym, &symbolToElfsymAlloc);

        /* Make sure that string index 0 points to an empty string */
        append_to_ElfStringTable(&shstrtabStrings, "");
        append_to_ElfStringTable(&strtabStrings, "");

        /*
         * Make symbol strings and symbol table
         */
        BUF_RESERVE(&symbolToElfsym, &symbolToElfsymAlloc, symbolCnt);
        for (int i = 0; i < symbolCnt; i++)
                symbolToElfsym[i] = -1;
        /* one dummy symbol required */
        {
                int x = elfsymCnt++;
                BUF_RESERVE(&elfsym, &elfsymAlloc, elfsymCnt);
                CLEAR(elfsym[x]);
        }
        /* section symbols. For some reason the STB_LOCAL symbols must come
         * first */
        for (int i = 0; i < NUM_ESS; i++) {
                int x = elfsymCnt++;
                BUF_RESERVE(&elfsym, &elfsymAlloc, elfsymCnt);
                elfsym[x].st_name = 0;  // (empty/no) string
                elfsym[x].st_info = (STB_LOCAL << 4) | STT_SECTION;
                elfsym[x].st_other = 0;
                elfsym[x].st_shndx = i;
                elfsym[x].st_value = 0;
                elfsym[x].st_size = 0;
                elfsectionToElfsym[i] = x;
        }
        /* defined symbols */
        for (int i = 0; i < symDefCnt; i++) {
                int sttKind;  // STT_
                int shndx;
                int value = symDefInfo[i].offset;
                int size = symDefInfo[i].size;
                Symbol sym = symDefInfo[i].symbol;
                int sectionKind = symDefInfo[i].kind;  // SECTION_
                switch (sectionKind) {
                case SECTION_CODE:
                        ASSERT(symbolInfo[sym].kind == SYMBOL_PROC);
                        ASSERT(symbolInfo[sym].tProc.optionalproc != -1);
                        sttKind = STT_FUNC;
                        shndx = ES_TEXT; // symbol references the .text section
                        break;
                case SECTION_ZERODATA:
                        ASSERT(symbolInfo[sym].kind == SYMBOL_DATA);
                        ASSERT(symbolInfo[sym].tData.optionaldata == -1);
                        sttKind = STT_OBJECT;
                        shndx = ES_BSS;
                        break;
                case SECTION_RODATA:
                        ASSERT(symbolInfo[sym].kind == SYMBOL_DATA);
                        ASSERT(symbolInfo[sym].tData.optionaldata != -1);
                        sttKind = STT_OBJECT;
                        shndx = ES_RODATA;
                        break;
                case SECTION_DATA:
                        ASSERT(symbolInfo[sym].kind == SYMBOL_DATA);
                        ASSERT(symbolInfo[sym].tData.optionaldata != -1);
                        sttKind = STT_OBJECT;
                        shndx = ES_DATA;
                        break;
                default:
                        UNHANDLED_CASE();
                }

                int x = elfsymCnt++;
                BUF_RESERVE(&elfsym, &elfsymAlloc, elfsymCnt);
                elfsym[x].st_name = append_to_ElfStringTable(
                                                &strtabStrings, SS(sym));
                elfsym[x].st_info = (STB_GLOBAL << 4) | sttKind;
                elfsym[x].st_other = 0;
                elfsym[x].st_shndx = shndx;
                elfsym[x].st_value = value;
                elfsym[x].st_size = size;
                symbolToElfsym[sym] = x;
        }
        /* undefined symbols */
        for (Symbol sym = 0; sym < symbolCnt; sym++) {
                if (scopeInfo[symbolInfo[sym].scope].kind != SCOPE_GLOBAL)
                        continue;
                int sttKind;
                if (symbolInfo[sym].kind == SYMBOL_PROC) {
                        if (symbolInfo[sym].tProc.optionalproc != -1)
                                continue;  // the proc is defined
                        sttKind = STT_FUNC;
                }
                else if (symbolInfo[sym].kind == SYMBOL_DATA) {
                        if (symbolInfo[sym].tData.optionaldata != -1)
                                continue;  // the data is defined
                        sttKind = STT_OBJECT;
                }
                else {
                        continue;
                }
                int x = elfsymCnt++;
                BUF_RESERVE(&elfsym, &elfsymAlloc, elfsymCnt);
                elfsym[x].st_name = append_to_ElfStringTable(
                                                &strtabStrings, SS(sym));
                elfsym[x].st_info = (STB_GLOBAL << 4) | sttKind;
                elfsym[x].st_other = 0;
                elfsym[x].st_shndx = SHN_UNDEF; // undefined symbol
                elfsym[x].st_value = 0;
                elfsym[x].st_size = 0;
                symbolToElfsym[sym] = x;
        }

        /*
         * initialize Relocations
         */

        for (int i = 0; i < relocCnt; i++) {
                Symbol sym = relocInfo[i].symbol;
                int kind = relocInfo[i].kind;
                Elf64_Sxword addend = relocInfo[i].addend;
                int offset = relocInfo[i].offset;

                int esym;
                if (sym == -1) {
                        int esec = sectionToElfsection[kind];
                        esym = elfsectionToElfsym[esec];
                }
                else {
                        esym = symbolToElfsym[sym];
                        if (esym == -1)
                                FATAL("There is a relocation for symbol %s which is not in .symtab (is this an internal error?)\n", SS(sym));
                }
                int x = relaTextCnt++;
                BUF_RESERVE(&relaText, &relaTextAlloc, relaTextCnt);
                relaText[x].r_offset = offset;
                relaText[x].r_info = ELF64_R_INFO(esym, R_AMD64_64);
                relaText[x].r_addend = addend;
        }

        /*
         * initialize File header
         */
        struct Elf64_Ehdr eh = {0};

        eh.e_ident[EI_MAG0] = 0x7f;
        eh.e_ident[EI_MAG1] = 'E';
        eh.e_ident[EI_MAG2] = 'L';
        eh.e_ident[EI_MAG3] = 'F';
        eh.e_ident[EI_CLASS] = ELFCLASS64;
        eh.e_ident[EI_DATA] = ELFDATA2LSB;
        eh.e_ident[EI_VERSION] = EV_CURRENT;
        eh.e_ident[EI_OSABI] = ELFOSABI_SYSV;
        eh.e_ident[EI_ABIVERSION] = 0;
        eh.e_type = ET_REL;
        eh.e_machine = 62;  // code for AMD64 (not listed in the spec)
        eh.e_version = EV_CURRENT;
        eh.e_entry = 0;
        eh.e_phoff = 0;
        eh.e_shoff = sizeof (struct Elf64_Ehdr);  // XXX
        eh.e_flags = 0;
        eh.e_ehsize = sizeof (struct Elf64_Ehdr);  // XXX
        eh.e_phentsize = 0;
        eh.e_phnum = 0;
        eh.e_shentsize = sizeof (struct Elf64_Shdr);  // XXX
        eh.e_shnum = NUM_ESS;
        eh.e_shstrndx = NUM_ESS - 1; /* .shstrtab comes last */

        /*
         * Initialize section headers
         */
        struct Elf64_Shdr sh[NUM_ESS] = {0};

        for (int i = 0; i < NUM_ESS; i++)
                sh[i].sh_name = append_to_ElfStringTable(
                                        &shstrtabStrings, sectionNames[i]);

        sh[ES_SYMTAB  ].sh_type = SHT_SYMTAB;
        sh[ES_TEXT    ].sh_type = SHT_PROGBITS;
        sh[ES_BSS     ].sh_type = SHT_NOBITS;
        sh[ES_RODATA  ].sh_type = SHT_PROGBITS;
        sh[ES_DATA    ].sh_type = SHT_PROGBITS;
        sh[ES_RELATEXT].sh_type = SHT_RELA;
        sh[ES_STRTAB  ].sh_type = SHT_STRTAB;
        sh[ES_SHSTRTAB].sh_type = SHT_STRTAB;

        sh[ES_SYMTAB  ].sh_size = elfsymCnt * sizeof (struct Elf64_Sym);  // XXX sizeof?
        sh[ES_TEXT    ].sh_size = codeSectionCnt;
        sh[ES_BSS     ].sh_size = zerodataSectionCnt;
        sh[ES_RELATEXT].sh_size = relaTextCnt * sizeof (struct Elf64_Rela);  // ditto
        sh[ES_RODATA  ].sh_size = rodataSectionCnt;
        sh[ES_DATA    ].sh_size = dataSectionCnt;
        sh[ES_STRTAB  ].sh_size = strtabStrings.size;
        sh[ES_SHSTRTAB].sh_size = shstrtabStrings.size;

        /* XXX: Alignment? */
        sh[ES_SYMTAB  ].sh_offset = eh.e_shoff + eh.e_shnum * sizeof (struct Elf64_Shdr);
        sh[ES_TEXT    ].sh_offset = sh[ES_SYMTAB  ].sh_offset + sh[ES_SYMTAB  ].sh_size;
        sh[ES_BSS     ].sh_offset = sh[ES_TEXT].sh_offset; // special case: the sh_size and the offset in the file do not correlate
        sh[ES_RODATA  ].sh_offset = sh[ES_TEXT    ].sh_offset + sh[ES_TEXT    ].sh_size;
        sh[ES_DATA    ].sh_offset = sh[ES_RODATA  ].sh_offset + sh[ES_RODATA  ].sh_size;
        sh[ES_RELATEXT].sh_offset = sh[ES_DATA    ].sh_offset + sh[ES_DATA    ].sh_size;
        sh[ES_STRTAB  ].sh_offset = sh[ES_RELATEXT].sh_offset + sh[ES_RELATEXT].sh_size;
        sh[ES_SHSTRTAB].sh_offset = sh[ES_STRTAB  ].sh_offset + sh[ES_STRTAB  ].sh_size;

        /* XXX: ??? */
        sh[ES_SYMTAB  ].sh_addralign = 8;
        sh[ES_TEXT    ].sh_addralign = 8;
        sh[ES_BSS     ].sh_addralign = 8;
        sh[ES_RODATA  ].sh_addralign = 8;
        sh[ES_DATA    ].sh_addralign = 8;
        sh[ES_RELATEXT].sh_addralign = 8;
        sh[ES_STRTAB  ].sh_addralign = 1;
        sh[ES_SHSTRTAB].sh_addralign = 1;

        sh[ES_SYMTAB].sh_flags = SHF_ALLOC;
        sh[ES_TEXT  ].sh_flags = SHF_ALLOC | SHF_EXECINSTR;
        sh[ES_BSS   ].sh_flags = SHF_ALLOC | SHF_WRITE;
        sh[ES_RODATA].sh_flags = SHF_ALLOC;
        sh[ES_DATA  ].sh_flags = SHF_ALLOC | SHF_WRITE;

        sh[ES_SYMTAB  ].sh_entsize = sizeof (struct Elf64_Sym);  // ???
        sh[ES_RELATEXT].sh_entsize = sizeof (struct Elf64_Rela);  // ???

        sh[ES_SYMTAB  ].sh_link = ES_STRTAB;  // index of .strtab
        sh[ES_RELATEXT].sh_link = ES_SYMTAB;  // index of .symtab

        /* XXX "Index of first non-local symbol (i.e., number of local symbols)"
         */
        sh[ES_SYMTAB].sh_info = elfsectionToElfsym[NUM_ESS-1] + 1;

        sh[ES_RELATEXT].sh_info = ES_TEXT;  // index of .text

        /*
        fprintf(stderr, "the size of a section header is %d\n", (int) sizeof (struct Elf64_Shdr));
        fprintf(stderr, "start of section header table is at %d\n", (int) eh.e_shoff);
        fprintf(stderr, "start / size of .text section is %d / %d\n", (int) texthdr.sh_offset, (int) texthdr.sh_size);
        fprintf(stderr, "start / size of .symtab section is %d / %d\n", (int) symtabhdr.sh_offset, (int) symtabhdr.sh_size);
        fprintf(stderr, "start / size of .strtab section is %d / %d\n", (int) strtabhdr.sh_offset, (int) strtabhdr.sh_size);
        fprintf(stderr, "start / size of .shstrtab section is %d / %d\n", (int) shstrtabhdr.sh_offset, (int) shstrtabhdr.sh_size);
        */

        /*
         * Serialize to object file on the filesystem.
         */

        FILE *f = fopen(outfilepath, "wb");
        if (f == NULL) {
                fprintf(stderr, "Failed to open file %s\n", outfilepath);
                abort();
        }

        write_Elf64_Ehdr(&eh, f);
        for (int i = 0; i < NUM_ESS; i++)
                write_Elf64_Shdr(&sh[i], f);

        /* .symtab */
        for (int i = 0; i < elfsymCnt; i++)
                write_Elf64_Sym(&elfsym[i], f);
        /* .text */
        fwrite(codeSection, codeSectionCnt, 1, f);
        fwrite(rodataSection, rodataSectionCnt, 1, f);
        fwrite(dataSection, dataSectionCnt, 1, f);
        /* .rela.text */
        for (int i = 0; i < relaTextCnt; i++)
                write_Elf64_Rela(&relaText[i], f);
        /* .strtab */
        fwrite(strtabStrings.buf, strtabStrings.size, 1, f);
        /* .shstrtab */
        fwrite(shstrtabStrings.buf, shstrtabStrings.size, 1, f);

        fflush(f);
        if (ferror(f)) {
                fprintf(stderr, "I/O error writing to %s\n", outfilepath);
                abort();
        }
        fclose(f);

        exit_ElfStringTable(&shstrtabStrings);
        exit_ElfStringTable(&strtabStrings);
        BUF_EXIT(&elfsym, &elfsymAlloc);
        BUF_EXIT(&relaText, &relaTextAlloc);
        BUF_EXIT(&symbolToElfsym, &symbolToElfsymAlloc);
}
