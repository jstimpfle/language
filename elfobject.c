/* Implementation of ELF64. Closely following the spec, with lots of comments
 * pulled from it ("ELF-64 Object File Format, Version 1.5 Draft 2"). We try to
 * be independent of the rest of the infrastructure in the project, so this file
 * will be reusable as a baseline in other projects with only small changes. */

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
         * in memory. If the section is not allocated to the emory image of the
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

/* Symbol table symbol */
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


void initialize_Elf64_Ehdr(struct Elf64_Ehdr *h)
{
        memset(h, 0, sizeof *h);

        h->e_ident[EI_MAG0] = 0x7f;
        h->e_ident[EI_MAG1] = 'E';
        h->e_ident[EI_MAG2] = 'L';
        h->e_ident[EI_MAG3] = 'F';
        h->e_ident[EI_CLASS] = ELFCLASS64;
        h->e_ident[EI_DATA] = ELFDATA2LSB;
        h->e_ident[EI_VERSION] = EV_CURRENT;
        h->e_ident[EI_OSABI] = ELFOSABI_SYSV;
        h->e_ident[EI_ABIVERSION] = 0;
        h->e_type = ET_REL;
        h->e_machine = 62;  // XXX: this should mean AMD64. I picked this up from somewhere else.
        h->e_version = EV_CURRENT;
        h->e_entry = 0;
        h->e_phoff = 0;  // set later
        h->e_shoff = 0;  // set later
        h->e_flags = 0;  // XXX: ???
        h->e_ehsize = sizeof *h;  // XXX: ???
        h->e_phentsize = 0;  // XXX: ???
        h->e_phnum = 0;  // increased later
        h->e_shentsize = sizeof (struct Elf64_Shdr);  // XXX: ?
        h->e_shnum = 0;  // increased later
        h->e_shstrndx = 0;  // set later
}

void initialize_Elf64_Shdr(struct Elf64_Shdr *sh)
{
        memset(sh, 0, sizeof *sh);
        sh->sh_addralign = 1;
}

typedef unsigned char Elf64_Uchar;
typedef uint64_t Elf64_Addr;
typedef uint64_t Elf64_Off;
typedef uint16_t Elf64_Half;
typedef uint32_t Elf64_Word;
typedef int32_t Elf64_Sword;
typedef uint64_t Elf64_Xword;
typedef int64_t Elf64_Sxword;

void write_Elf64_Uchar(Elf64_Uchar x, FILE *f)
{
        fputc(x, f);
}

void write_Elf64_Addr(Elf64_Addr x, FILE *f)
{
        // XXX: endianness?
        fwrite(&x, sizeof x, 1, f);
}

void write_Elf64_Off(Elf64_Off x, FILE *f)
{
        // XXX: endianness?
        fwrite(&x, sizeof x, 1, f);
}

void write_Elf64_Half(Elf64_Half x, FILE *f)
{
        // XXX: endianness?
        fwrite(&x, sizeof x, 1, f);
}

void write_Elf64_Word(Elf64_Word x, FILE *f)
{
        // XXX: endianness?
        fwrite(&x, sizeof x, 1, f);
}

void write_Elf64_Sword(Elf64_Sword x, FILE *f)
{
        // XXX: endianness?
        fwrite(&x, sizeof x, 1, f);
}

void write_Elf64_Xword(Elf64_Xword x, FILE *f)
{
        // XXX: endianness?
        fwrite(&x, sizeof x, 1, f);
}

void write_Elf64_Sxword(Elf64_Sxword x, FILE *f)
{
        // XXX: endianness?
        fwrite(&x, sizeof x, 1, f);
}

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

void write_elf64_object(const char *outfilepath)
{
        FILE *f = fopen(outfilepath, "wb");
        if (f == NULL) {
                fprintf(stderr, "Failed to open file %s\n", outfilepath);
                abort();
        }

        struct ElfStringTable myheaderstrings;
        struct ElfStringTable mystrings;

        struct Elf64_Ehdr ehdr;
        struct Elf64_Shdr dummyhdr;  // the first section header is SHN_UNDEF
        struct Elf64_Shdr symtabhdr;  // header for .symtab section (symbol table)
        struct Elf64_Shdr texthdr;  // header for .text section
        struct Elf64_Shdr strtabhdr;  // header for table for most strings
        struct Elf64_Shdr shstrtabhdr;  // header for table for string section names (referenced by e_shstrndx)

        init_ElfStringTable(&myheaderstrings);
        init_ElfStringTable(&mystrings);

        initialize_Elf64_Ehdr(&ehdr);
        initialize_Elf64_Shdr(&dummyhdr);
        initialize_Elf64_Shdr(&symtabhdr);
        initialize_Elf64_Shdr(&texthdr);
        initialize_Elf64_Shdr(&strtabhdr);
        initialize_Elf64_Shdr(&shstrtabhdr);

        ehdr.e_shoff = sizeof (struct Elf64_Shdr);  // XXX
        ehdr.e_shnum = 5;
        ehdr.e_shstrndx = 4; /* .shstrtab comes last */

        /* Make sure that string index 0 points to an empty string */
        append_to_ElfStringTable(&myheaderstrings, "");
        append_to_ElfStringTable(&mystrings, "");

        dummyhdr.sh_name = 0;  // empty/missing string
        symtabhdr.sh_name = append_to_ElfStringTable(&myheaderstrings, ".symtab");
        texthdr.sh_name = append_to_ElfStringTable(&myheaderstrings, ".text");
        strtabhdr.sh_name = append_to_ElfStringTable(&myheaderstrings, ".strtab");
        shstrtabhdr.sh_name = append_to_ElfStringTable(&myheaderstrings, ".shstrtab");

        for (int i = 0; i < symDefCnt; i++)
                append_to_ElfStringTable(&mystrings, SS(symDefInfo[i].symbol));

        dummyhdr.sh_type = SHT_NULL;
        symtabhdr.sh_type = SHT_SYMTAB;
        texthdr.sh_type = SHT_PROGBITS;
        strtabhdr.sh_type = SHT_STRTAB;
        shstrtabhdr.sh_type = SHT_STRTAB;

        dummyhdr.sh_size = 0;
        symtabhdr.sh_size = (symDefCnt + 1) * sizeof (struct Elf64_Sym);  // XXX sizeof? symDefCnt + 1, because there is one dummy symbol upfront
        texthdr.sh_size = codeSectionCnt;
        strtabhdr.sh_size = mystrings.size;
        shstrtabhdr.sh_size = myheaderstrings.size;

        /* XXX: Alignment? */
        dummyhdr.sh_offset = ehdr.e_shoff + ehdr.e_shnum * sizeof (struct Elf64_Shdr);
        symtabhdr.sh_offset = dummyhdr.sh_offset + dummyhdr.sh_size;
        texthdr.sh_offset = symtabhdr.sh_offset + symtabhdr.sh_size;
        strtabhdr.sh_offset = texthdr.sh_offset + texthdr.sh_size;
        shstrtabhdr.sh_offset = strtabhdr.sh_offset + strtabhdr.sh_size;

        symtabhdr.sh_flags = SHF_ALLOC; /* ??? needed ??? */
        symtabhdr.sh_entsize = sizeof (struct Elf64_Sym);  // ???
        symtabhdr.sh_link = 3;  // index of strtabhdr
        symtabhdr.sh_info = 1;  /* XXX not sure what to put here. See Table 11,
                                   "Index of first non-local symbol (i.e.,
                                   number of local symbols)" */

        /*
        fprintf(stderr, "the size of a section header is %d\n", (int) sizeof (struct Elf64_Shdr));
        fprintf(stderr, "start of section header table is at %d\n", (int) ehdr.e_shoff);
        fprintf(stderr, "start / size of .text section is %d / %d\n", (int) texthdr.sh_offset, (int) texthdr.sh_size);
        fprintf(stderr, "start / size of .symtab section is %d / %d\n", (int) symtabhdr.sh_offset, (int) symtabhdr.sh_size);
        fprintf(stderr, "start / size of .strtab section is %d / %d\n", (int) strtabhdr.sh_offset, (int) strtabhdr.sh_size);
        fprintf(stderr, "start / size of .shstrtab section is %d / %d\n", (int) shstrtabhdr.sh_offset, (int) shstrtabhdr.sh_size);
        */

        /* Write global Elf header and section headers */
        write_Elf64_Ehdr(&ehdr, f);
        write_Elf64_Shdr(&dummyhdr, f);
        write_Elf64_Shdr(&symtabhdr, f);
        write_Elf64_Shdr(&texthdr, f);
        write_Elf64_Shdr(&strtabhdr, f);
        write_Elf64_Shdr(&shstrtabhdr, f);
        //fprintf(stderr, "after section headers, the file is %d large\n", (int) ftell(f));

        /* write .symtab section */
        {
                /* one fake symbol required */
                struct Elf64_Sym esym = {0};
                memset(&esym, 0, sizeof esym);
                write_Elf64_Sym(&esym, f);
        }
        for (int i = 0; i < symDefCnt; i++) {
                struct Elf64_Sym esym = {0};
                esym.st_name = 1; //XXX see string append to "mystrings" above
                esym.st_info = (STB_GLOBAL << 4) | STT_FUNC;
                esym.st_other = 0;
                esym.st_shndx = SHN_ABS; //XXX???
                esym.st_value = 0; //XXX
                esym.st_size = symDefInfo[i].size;
                esym.st_size = 0; //XXX
                write_Elf64_Sym(&esym, f);
        }
        //fprintf(stderr, "after .symtab, the file is %d large\n", (int) ftell(f));

        /* write .text section */
        fwrite(codeSection, codeSectionCnt, 1, f);
        //fprintf(stderr, "after .text, the file is %d large\n", (int) ftell(f));

        /* write .strtab section */
        fwrite(mystrings.buf, mystrings.size, 1, f);
        //fprintf(stderr, "after .strtab, the file is %d large\n", (int) ftell(f));

        /* write .shstrtab section */
        fwrite(myheaderstrings.buf, myheaderstrings.size, 1, f);
        //fprintf(stderr, "after .shstrtab, the file is %d large\n", (int) ftell(f));

        if (ferror(f)) {
                fprintf(stderr, "I/O error writing to %s\n", outfilepath);
                abort();
        }
        fclose(f);

        exit_ElfStringTable(&myheaderstrings);
        exit_ElfStringTable(&mystrings);
}
