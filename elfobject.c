/* Implementation of ELF64. Closely following the spec, with lots of comments
 * pulled from it. We try to be independent of the rest of the infrastructure in
 * the project, so this file will be reusable as a baseline in other projects
 * with only small changes. */

#include "defs.h"
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

typedef uint64_t Elf64_Addr;
typedef uint64_t Elf64_Off;
typedef uint16_t Elf64_Half;
typedef uint32_t Elf64_Word;
typedef int32_t Elf64_Sword;
typedef uint64_t Elf64_Xword;
typedef int64_t Elf64_Sxword;

/* Elf file header (must be at position 0 in file) */
struct Elf64_Ehdr {
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
        Elf64_Half      e_shstrndx;     /* Section name string table index */
};

/* Section header */
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

/* Names for the individual bytes in (struct Elf64_Ehdr).e_ident */
enum {
        EI_MAG0 = 0,
        EI_MAG1 = 1,
        EI_MAG2 = 2,
        EI_MAG3 = 3,
        EI_CLASS = 4,
        EI_DATA = 5,
        EI_VERSION = 6,
        EI_OSABI = 7,
        EI_ABIVERSION = 8,
        EI_PAD = 9,
        EI_NIDENT = 16,
};

/* Possible values for (struct Elf64_Ehdr).eident[EI_CLASS]
 * and also for (struct Elf64_Ehdr).e_version. */
enum {
        ELFCLASS32 = 1,
        ELFCLASS64 = 2,
};

/* Possible values for (struct Elf64_Ehdr).eident[EI_DATA] */
enum {
        ELFDATA2LSB = 1,  /* Object file data structures are little-endian */
        ELFDATA2MSB = 2,  /* Object file data structures are big-endian */
};

/* Possible values for (struct Elf64_Ehdr).eident[EI_VERSION] */
enum {
        EV_CURRENT = 1,
};

/* Possible values for (struct Elf64_Ehdr).eident[EI_OSABI] */
enum {
        ELFOSABI_SYSV = 0,              /* System V ABI */
        ELFOSABI_HPUX = 1,              /* HP-UX operating system */
        ELFOSABI_STANDALONE = 255,      /* Standalone (embedded) application */
};

/* Table 6. Possible values for (struct Elf64_Ehdr).e_type */
#define ET_NONE 0               /* No file type */
#define ET_REL 1                /* Relocatable object file */
#define ET_EXEC 2               /* Executable file */
#define ET_DYN 3                /* Shared object file */
#define ET_CORE 4               /* Core file */
#define ET_LOOS 0xFE00          /* Environment-specific use */
#define ET_HIOS 0xFEFF          /* (ditto) */
#define ET_LOPROC 0xFF00        /* Processor-specific use */
#define ET_HIPROC 0xFFFF        /* (ditto) */

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


void initialize_elf64_header(struct Elf64_Ehdr *h)
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
        /*
         * e_ident[EI_ABIVERSION] identifies the version of the ABI for which
         * the object is prepared. This field is used to distinguish among
         * incompatible versions of an ABI. The interpretation of this version
         * number is dependent on the ABI identified by the EI_OSABI field.
         *
         * For applications conforming to the System V ABI, third edition, this
         * field should contain 0.
         */
        h->e_ident[EI_ABIVERSION] = 0;
        /* e_type identifies the object file type. The processor-independent
         * values for this field are listed in Table 6. */
        h->e_type = ET_REL;
        /* e_machine identifies the target architecture. These values are define
         * in the processor-specific supplements. (XXX: where is this???) */
        h->e_machine = 62;  // XXX: this should mean AMD64. I picked this up from somewhere else.
        /* e_version identifies the version of the object file format.
         * Currently, this field has the value EV_CURRENT, which is defined with
         * the value 1. */
        h->e_version = EV_CURRENT;
        /* e_entry contains the virtual address of the program entry point. If
         * there is no entry point, this field contains zero. */
        h->e_entry = 0;
        /* e_phoff contains the file offset, in bytes, of the program header
         * table. */
        h->e_phoff = 0;  // set later
        /* e_shoff contains the file offset, in bytes, of the section header
         * table. */
        h->e_shoff = 0;  // set later
        /* e_flags contains processor specific flags */
        h->e_flags = 0;  // XXX: ???
        /* e_ehsize contains the size, in bytes, of the ELF header. */
        h->e_ehsize = sizeof *h;  // XXX: ???
        /* e_phentsize contains the size, in bytes, of a program header table */
        h->e_phentsize = 0;  // XXX: ???
        /* e_phnum contains the number of entries in the program header table */
        h->e_phnum = 0;  // increased later
        /* e_shentsize contains the size, in bytes, of a section header table */
        h->e_shentsize = sizeof (struct Elf64_Shdr);  // XXX: ?
        /* e_shnum contains the number of entries in the section header table */
        h->e_shnum = 0;  // increased later
        /* e_shstrndx contains the section header table index of the section
         * containing the section name string table If there is no section name
         * string table, this field has the value SHN_UNDEF. */
        h->e_shstrndx = 0;  // set later
}

void initialize_elf64_section_header(struct Elf64_Shdr *sh)
{
        memset(sh, 0, sizeof *sh);

}

void write_elf64_header(struct Elf64_Ehdr *h, FILE *f)
{
        fwrite(h, sizeof *h, 1, f);  // XXX Evil
}

void write_elf64_section_header(struct Elf64_Shdr *s, FILE *f)
{
        fwrite(s, sizeof *s, 1, f);  // XXX Evil
}

struct MyStringTable {
        char *buf;
        size_t size;
        size_t allocated_size;
        size_t numstrings;
};

void init_MyStringTable(struct MyStringTable *t)
{
        t->buf = NULL;
        t->size = 0;
        t->allocated_size = 0;
        t->numstrings = 0;
}

void exit_MyStringTable(struct MyStringTable *t)
{
        free(t->buf);
        memset(t, 0, sizeof *t);
}

size_t append_to_MyStringTable(struct MyStringTable *t, char *str)
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

        struct MyStringTable myheaderstrings;
        struct MyStringTable mystrings;

        struct Elf64_Ehdr ehdr;
        struct Elf64_Shdr dummyhdr;  // the first section header is SHN_UNDEF
        struct Elf64_Shdr shstrhdr;  // global string section header (referenced by e_shstrndx)
        struct Elf64_Shdr texthdr;  // header for .text section

        init_MyStringTable(&myheaderstrings);
        init_MyStringTable(&mystrings);

        initialize_elf64_header(&ehdr);
        initialize_elf64_section_header(&shstrhdr);
        initialize_elf64_section_header(&texthdr);

        memset(&dummyhdr, 0, sizeof dummyhdr);

        ehdr.e_shoff = sizeof (struct Elf64_Shdr);  // XXX ?
        ehdr.e_shnum = 3;  // for now: dummy, .shstrtab, .text.
        ehdr.e_shstrndx = 1; /* XXX I think this should be 1 since 0 is
                                SHN_UNDEF and we will try to by convention put
                                the string table first. This is still subject to
                                change. */

        // important. Otherwise dummyhdr.sh_name == 0 == shstrhdr.sh_name, so we
        // have two sections named .shstrtab, which is BAD
        dummyhdr.sh_name = append_to_MyStringTable(&myheaderstrings, "");

        shstrhdr.sh_name = append_to_MyStringTable(&myheaderstrings, ".shstrtab");
        shstrhdr.sh_type = SHT_STRTAB;
        shstrhdr.sh_offset = ehdr.e_shoff + ehdr.e_shnum * sizeof (struct Elf64_Shdr); //XXX ???
        shstrhdr.sh_size = myheaderstrings.size;

        texthdr.sh_name = append_to_MyStringTable(&myheaderstrings, ".text");
        texthdr.sh_type = SHT_PROGBITS;

        write_elf64_header(&ehdr, f);
        write_elf64_section_header(&dummyhdr, f);
        write_elf64_section_header(&shstrhdr, f);
        write_elf64_section_header(&texthdr, f);

        fprintf(stderr, "start of section header table is at %d\n", (int) ehdr.e_shoff);
        fprintf(stderr, "start of shstrtab is at %d\n", (int) shstrhdr.sh_offset);

        fwrite(myheaderstrings.buf, myheaderstrings.size, 1, f);

        if (ferror(f)) {
                fprintf(stderr, "I/O error writing to %s\n", outfilepath);
                abort();
        }
        fclose(f);

        exit_MyStringTable(&myheaderstrings);
        exit_MyStringTable(&mystrings);
}
