/* PE-64 object file writer */

#include "defs.h"
#include "api.h"
#include <stdio.h>

typedef char  PED_BYTE;   // 1 byte
typedef short PED_WORD;   // 2 bytes
typedef long  PED_DWORD;  // 4 bytes


/*
   3.3.1. Machine Types

   The Machine field has one of the following values, defined below, which
   specify its machine (CPU) type. An image file can be run only on the
   specified machine, or a system emulating it.
 */

#define IMAGE_FILE_MACHINE_UNKNOWN     0x0  /* Contents assumed to be applicable
                                               to any machine type. */
#define IMAGE_FILE_MACHINE_ALPHA     0x184  /* Alpha AXP ™. */
#define IMAGE_FILE_MACHINE_ARM       0x1c0
#define IMAGE_FILE_MACHINE_ALPHA64   0x284  /* Alpha AXP ™ 64-bit. */
#define IMAGE_FILE_MACHINE_I386      0x14c  /* Intel 386 or later, and
                                               compatible processors. */
#define IMAGE_FILE_MACHINE_AMD64    0x8664
#define IMAGE_FILE_MACHINE_IA64      0x200  /* Intel IA64™ */
#define IMAGE_FILE_MACHINE_M68K      0x268  /* Motorola 68000 series. */
#define IMAGE_FILE_MACHINE_MIPS16    0x266
#define IMAGE_FILE_MACHINE_MIPSFPU   0x366  /* MIPS with FPU */
#define IMAGE_FILE_MACHINE_MIPSFPU16 0x466  /* MIPS16 with FPU */
#define IMAGE_FILE_MACHINE_POWERPC   0x1f0  /* Power PC, little endian. */
#define IMAGE_FILE_MACHINE_R3000     0x162
#define IMAGE_FILE_MACHINE_R4000     0x166  /* MIPS® little endian. */
#define IMAGE_FILE_MACHINE_R10000    0x168
#define IMAGE_FILE_MACHINE_SH3       0x1a2  /* Hitachi SH3 */
#define IMAGE_FILE_MACHINE_SH4       0x1a6  /* Hitachi SH4 */
#define IMAGE_FILE_MACHINE_THUMB     0x1c2

/*
   3.3.2. Characteristics

   The Characteristics field contains flags that indicate attributes of the
   object or image file. The following flags are currently defined:
 */

#define IMAGE_FILE_RELOCS_STRIPPED         0x0001  /* Image only, Windows CE,
                                                      Windows NT and above.
                                                      Indicates that the file
                                                      does not contain base
                                                      relocations and must
                                                      therefore be loaded at its
                                                      preferred base address. If
                                                      the base address is not
                                                      available, the loader
                                                      reports an error.
                                                      Operating systems running
                                                      on top of MS-DOS (Win32s
                                                      ™) are generally not able
                                                      to use the preferred base
                                                      address and so cannot run
                                                      these images.  However,
                                                      beginning with version
                                                      4.0, Windows will use an
                                                      application’s preferred
                                                      base address. The default
                                                      behavior of the linker is
                                                      to strip base relocations
                                                      from EXEs. */
#define IMAGE_FILE_EXECUTABLE_IMAGE        0x0002  /* Image only. Indicates that
                                                      the image file is valid
                                                      and can be run.  If this
                                                      flag is not set, it
                                                      generally indicates a
                                                      linker error. */
#define IMAGE_FILE_LINE_NUMS_STRIPPED      0x0004  /* COFF line numbers have
                                                      been removed. */
#define IMAGE_FILE_LOCAL_SYMS_STRIPPED     0x0008  /* COFF symbol table entries
                                                      for local symbols have
                                                      been removed. */
#define IMAGE_FILE_AGGRESSIVE_WS_TRIM      0x0010  /* Aggressively trim working
                                                      set. */
#define IMAGE_FILE_LARGE_ADDRESS_AWARE     0x0020  /* App can handle > 2gb
                                                      addresses. */
#define IMAGE_FILE_16BIT_MACHINE           0x0040  /* Use of this flag is
                                                      reserved for future use.
                                                      */
#define IMAGE_FILE_BYTES_REVERSED_LO       0x0080  /* Little endian: LSB
                                                      precedes MSB in memory. */
#define IMAGE_FILE_32BIT_MACHINE           0x0100  /* Machine based on
                                                      32-bit-word architecture.
                                                      */
#define IMAGE_FILE_DEBUG_STRIPPED          0x0200  /* Debugging information
                                                      removed from image file.
                                                      */
#define IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP 0x0400  /* If image is on removable
                                                      media, copy and run from
                                                      swap file. */
#define IMAGE_FILE_SYSTEM                  0x1000  /* The image file is a system
                                                      file, not a user program.
                                                      */
#define IMAGE_FILE_DLL                     0x2000  /* The image file is a
                                                      dynamic-link library
                                                      (DLL). Such files are
                                                      considered executable
                                                      files for almost all
                                                      purposes, although they
                                                      cannot be directly run. */
#define IMAGE_FILE_UP_SYSTEM_ONLY          0x4000  /* File should be run only on
                                                      a UP machine. */
#define IMAGE_FILE_BYTES_REVERSED_HI       0x8000  /* Big endian: MSB precedes
                                                      LSB in memory. */

struct PE_FileHeader {
        PED_DWORD  PEH_Signature;  //XXX seems unused for .obj files??

        PED_WORD   PEH_Machine;               /* IMAGE_FILE_MACHINE_?? */

        PED_WORD   PEH_NumberOfSections;      /* Number of sections in the
                                                 object file */

        PED_DWORD  PEH_TimeDataStamp;         /* Unix timestamp */

        PED_DWORD  PEH_PointerToSymbolTable;  /* The offset of the symbol table,
                                                 in bytes, or zero if no PE
                                                 symbol table exists. */
        PED_DWORD  PEH_NumberOfSymbols;       /* The number of entries in the
                                                 symbol table. This data can be
                                                 used to locate the string
                                                 table, which immediately
                                                 follows the symbol table. This
                                                 value should be zero for an
                                                 image because COFF debugging
                                                 information is deprecated. */
        PED_WORD  PEH_SizeOfOptionalHeader;

        PED_WORD  PEH_Characteristics;        /* See 3.3.2 Characteristics */
};


/* Fields of Optional Header */
struct PE_OptionalHeader {
        PED_WORD   PEO_Magic;
        PED_BYTE   PEO_MajorLinkerVersion;
        PED_BYTE   PEO_MinorLinkerVersion;
        PED_DWORD  PEO_SizeOfCode;
        PED_DWORD  PEO_SizeOfInitializedData;
        PED_DWORD  PEO_SizeOfUninitializedData;
        PED_DWORD  PEO_AddressOfEntryPoint;
        PED_DWORD  PEO_BaseOfCode;
        PED_DWORD  PEO_BaseOfData;
        PED_DWORD  PEO_ImageBase;
        PED_DWORD  PEO_SectionAlignment;
        PED_DWORD  PEO_FileAlignment;
        PED_WORD   PEO_MajorOperatingSystemVersion;
        PED_WORD   PEO_MinorOperatingSystemVersion;
        PED_WORD   PEO_MajorImageVersion;
        PED_WORD   PEO_MinorImageVersion;
        PED_WORD   PEO_MajorSubsystemVersion;
        PED_WORD   PEO_MinorSubsystemVersion;
        PED_DWORD  PEO_Reserved1;
        PED_DWORD  PEO_SizeOfImage;
        PED_DWORD  PEO_SizeOfHeaders;
        PED_DWORD  PEO_CheckSum;
        PED_WORD   PEO_Subsystem;
        PED_WORD   PEO_DllCharacteristics;
        PED_DWORD  PEO_SizeOfStackReserve;
        PED_DWORD  PEO_SizeOfStackCommit;
        PED_DWORD  PEO_SizeOfHeapReserve;
        PED_DWORD  PEO_SizeOfHeapCommit;
        PED_DWORD  PEO_LoaderFlags;
        PED_DWORD  PEO_NumberOfRvaAndSizes;
};

struct PE_SectionHeader {
        char PES_Name[8]; /* An 8-byte, null-padded ASCII string. There is no
                             terminating null if the string is exactly eight
                             characters long. For longer names, this field
                             contains a slash (/) followed by ASCII
                             representation of a decimal number: this number is
                             an offset into the string table. Executable images
                             do not use a string table and do not support
                             section names longer than eight characters. Long
                             names in object files will be truncated if emitted
                             to an executable file. */
        PED_DWORD PES_VirtualSize;  /* Total size of the section when loaded
                                       into memory.  If this value is greater
                                       than Size of Raw Data, the section is
                                       zero-padded. This field is valid only for
                                       executable images and should be set to 0
                                       for object files. */
        PED_DWORD PES_VirtualAddress;  /* For executable images this is the
                                          address of the first byte of the
                                          section, when loaded into memory,
                                          relative to the image base. For object
                                          files, this field is the address of
                                          the first byte before relocation is
                                          applied; for simplicity, compilers
                                          should set this to zero. Otherwise, it
                                          is an arbitrary value that is
                                          subtracted from offsets during
                                          relocation. */
        PED_DWORD PES_SizeOfRawData;  /* Size of the section (object file) or
                                         size of the initialized data on disk
                                         (image files). For executable image,
                                         this must be a multiple of
                                         FileAlignment from the optional header.
                                         If this is less than VirtualSize the
                                         remainder of the section is zero
                                         filled. Because this field is rounded
                                         while the VirtualSize field is not it
                                         is possible for this to be greater than
                                         VirtualSize as well. When a section
                                         contains only uninitialized data, this
                                         field should be 0. */
        PED_DWORD PES_PointerToRawData;  /* File pointer to section’s first page
                                            within the COFF file. For executable
                                            images, this must be a multiple of
                                            FileAlignment from the optional
                                            header. For object files, the value
                                            should be aligned on a four- byte
                                            boundary for best performance. When
                                            a section contains only
                                            uninitialized data, this field
                                            should be 0. */
        PED_DWORD PES_PointerToRelocations;  /* File pointer to beginning of
                                                relocation entries for the
                                                section. Set to 0 for executable
                                                images or if there are no
                                                relocations. */
        PED_DWORD PES_PointerToLinenumbers;  /* File pointer to beginning of
                                                line-number entries for the
                                                section. Set to 0 if there are
                                                no COFF line numbers. */
        PED_WORD PES_NumberOfRelocations;  /* Number of relocation entries for
                                              the section. Set to 0 for
                                              executable images. */
        PED_WORD PES_NumberOfLinenumbers;  /* Number of line-number entries for
                                              the section. */
        PED_DWORD PES_Characteristics;  /* Flags describing section’s
                                           characteristics. See Section 4.1,
                                           “Section Flags,” for more
                                           information. */
};

#define IMAGE_SIZEONDISK_OF_FileHeader     20  /* NOTE: first element not
                                                  written */
#define IMAGE_SIZEONDISK_OF_SectionHeader  40
#define IMAGE_SIZEONDISK_OF_Reloc          10

/*
   4.1 Section Flags

   The Section Flags field indicates characteristics of the section.
 */

#define IMAGE_SCN_TYPE_REG     0x00000000  /* Reserved for future use. */
#define IMAGE_SCN_TYPE_DSECT   0x00000001  /* Reserved for future use. */
#define IMAGE_SCN_TYPE_NOLOAD  0x00000002  /* Reserved for future use. */
#define IMAGE_SCN_TYPE_GROUP   0x00000004  /* Reserved for future use. */
#define IMAGE_SCN_TYPE_NO_PAD  0x00000008  /* Section should not be padded to
                                              next boundary. This is obsolete
                                              and replaced by
                                              IMAGE_SCN_ALIGN_1BYTES.  This is
                                              valid for object files only. */
#define IMAGE_SCN_TYPE_COPY    0x00000010  /* Reserved for future use. */
#define IMAGE_SCN_CNT_CODE     0x00000020  /* Section contains executable code.
                                                */
#define IMAGE_SCN_CNT_INITIALIZED_DATA    0x00000040  /* Section contains
                                                         initialized data. */
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA  0x00000080  /* Section contains
                                                         uninitialized data. */
#define IMAGE_SCN_LNK_OTHER    0x00000100  /* Reserved for future use. */
#define IMAGE_SCN_LNK_INFO     0x00000200  /* Section contains comments or other
                                              information. The .drectve section
                                              has this type. This is valid for
                                              object files only. */
#define IMAGE_SCN_TYPE_OVER    0x00000400  /* Reserved for future use. */
#define IMAGE_SCN_LNK_REMOVE   0x00000800  /* Section will not become part of
                                              the image. This is valid for
                                              object files only. */
#define IMAGE_SCN_LNK_COMDAT   0x00001000  /* Section contains COMDAT data.  See
                                              Section 5.5.6, “COMDAT Sections,”
                                              for more information. This is
                                              valid for object files only. */
#define IMAGE_SCN_MEM_FARDATA     0x00008000  /* Reserved for future use. */
#define IMAGE_SCN_MEM_PURGEABLE   0x00020000  /* Reserved for future use. */
#define IMAGE_SCN_MEM_16BIT       0x00020000  /* Reserved for future use. */
#define IMAGE_SCN_MEM_LOCKED      0x00040000  /* Reserved for future use. */
#define IMAGE_SCN_MEM_PRELOAD     0x00080000  /* Reserved for future use. */
#define IMAGE_SCN_ALIGN_1BYTES    0x00100000  /* Align data on a 1-byte
                                                 boundary. This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_2BYTES    0x00200000  /* Align data on a 2-byte
                                                 boundary. This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_4BYTES    0x00300000  /* Align data on a 4-byte
                                                 boundary. This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_8BYTES    0x00400000  /* Align data on a 8-byte
                                                 boundary. This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_16BYTES   0x00500000  /* Align data on a 16-byte
                                                 boundary.  This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_32BYTES   0x00600000  /* Align data on a 32-byte
                                                 boundary.  This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_64BYTES   0x00700000  /* Align data on a 64-byte
                                                 boundary.  This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_128BYTES  0x00800000  /* Align data on a 128-byte
                                                 boundary. This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_256BYTES  0x00900000  /* Align data on a 256-byte
                                                 boundary. This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_512BYTES  0x00A00000  /* Align data on a 512-byte
                                                 boundary. This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_1024BYTES 0x00B00000  /* Align data on a 1024-byte
                                                 boundary.  This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_2048BYTES 0x00C00000  /* Align data on a 2048-byte
                                                 boundary. This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_4096BYTES 0x00D00000  /* Align data on a 4096-byte
                                                 boundary. This is valid for
                                                 object files only. */
#define IMAGE_SCN_ALIGN_8192BYTES 0x00E00000  /* Align data on a 8192-byte
                                                 boundary. This is valid for
                                                 object files only. */
#define IMAGE_SCN_LNK_NRELOC_OVFL 0x01000000  /* Section contains extended
                                                 relocations. */
#define IMAGE_SCN_MEM_DISCARDABLE 0x02000000  /* Section can be discarded as
                                                 needed. */
#define IMAGE_SCN_MEM_NOT_CACHED  0x04000000  /* Section cannot be cached. */
#define IMAGE_SCN_MEM_NOT_PAGED   0x08000000  /* Section is not pageable. */
#define IMAGE_SCN_MEM_SHARED      0x10000000  /* Section can be shared in
                                                 memory. */
#define IMAGE_SCN_MEM_EXECUTE     0x20000000  /* Section can be executed as
                                                 code. */
#define IMAGE_SCN_MEM_READ        0x40000000  /* Section can be read. */
#define IMAGE_SCN_MEM_WRITE       0x80000000  /* Section can be written to. */


/*
   5.2 COFF Relocations
 */

struct PE_Relocation {
        PED_DWORD  PER_VirtualAddress;    /* Address of the item to which
                                             relocation is applied: this is the
                                             offset from the beginning of the
                                             section, plus the value of the
                                             section’s RVA/Offset field (see
                                             Section 4, “Section Table.”). For
                                             example, if the first byte of the
                                             section has an address of 0x10, the
                                             third byte has an address of 0x12.
                                             */
        PED_DWORD  PER_SymbolTableIndex;  /* A zero-based index into the symbol
                                             table. This symbol gives the
                                             address to be used for the
                                             relocation. If the specified symbol
                                             has section storage class, then the
                                             symbol’s address is the address
                                             with the first section of the same
                                             name. */
        PED_WORD  PER_Type;               /* A value indicating what kind of
                                             relocation should be performed.
                                             Valid relocation types depend on
                                             machine type. See Section 5.2.1,
                                             “Type Indicators.” */
};

/*
   5.2.1 Symbol Table
 */

/*
   x64 Processors

   The following relocation type indicators are defined for x64 and compatible
   processors.
*/

#define IMAGE_REL_AMD64_ABSOLUTE   0x0000   /* The relocation is ignored. */
#define IMAGE_REL_AMD64_ADDR64     0x0001   /* The 64-bit VA of the relocation
                                               target. */
#define IMAGE_REL_AMD64_ADDR32     0x0002   /* The 32-bit VA of the relocation
                                               target. */
#define IMAGE_REL_AMD64_ADDR32NB   0x0003   /* The 32-bit address without an
                                               image base (RVA). */
#define IMAGE_REL_AMD64_REL32      0x0004   /* The 32-bit relative address from
                                               the byte following the
                                               relocation. */
#define IMAGE_REL_AMD64_REL32_1    0x0005   /* The 32-bit address relative to
                                               byte distance 1 from the
                                               relocation. */
#define IMAGE_REL_AMD64_REL32_2    0x0006   /* The 32-bit address relative to
                                               byte distance 2 from the
                                               relocation. */
#define IMAGE_REL_AMD64_REL32_3    0x0007   /* The 32-bit address relative to
                                               byte distance 3 from the
                                               relocation. */
#define IMAGE_REL_AMD64_REL32_4    0x0008   /* The 32-bit address relative to
                                               byte distance 4 from the
                                               relocation. */
#define IMAGE_REL_AMD64_REL32_5    0x0009   /* The 32-bit address relative to
                                               byte distance 5 from the
                                               relocation. */
#define IMAGE_REL_AMD64_SECTION    0x000A   /* The 16-bit section index of the
                                               section that contains the target.
                                               This is used to support debugging
                                               information. */
#define IMAGE_REL_AMD64_SECREL     0x000B   /* The 32-bit offset of the target
                                               from the beginning of its
                                               section. This is used to support
                                               debugging information and static
                                               thread local storage. */
#define IMAGE_REL_AMD64_SECREL7    0x000C   /* A 7-bit unsigned offset from the
                                               base of the section that contains
                                               the target. */
#define IMAGE_REL_AMD64_TOKEN      0x000D   /* CLR tokens. */
#define IMAGE_REL_AMD64_SREL32     0x000E   /* A 32-bit signed span-dependent
                                               value emitted into the object. */
#define IMAGE_REL_AMD64_PAIR       0x000F   /* A pair that must immediately
                                               follow every span-dependent
                                               value. */
#define IMAGE_REL_AMD64_SSPAN32    0x0010   /* A 32-bit signed span-dependent
                                               value that is applied at link
                                               time. */


/*
   5.4 COFF Symbol Table
 */

struct PE_Symbol {
        /*
           5.4.1. Symbol Name Representation

           The Name field in a symbol table consists of eight bytes that contain
           the name itself, if not too long, or else give an offset into the
           String Table. To determine whether the name itself or an offset is
           given, test the first four bytes for equality to zero.
         */
        union {
                char shortName[8];  /* An array of eight bytes. This array is
                                       padded with nulls on the right if the
                                       name is less than eight bytes long. */
                struct {
                        PED_DWORD zeroes;   /* Set to all zeros if the name is
                                               longer than eight bytes. */
                        PED_DWORD offset;   /* Offset into the String Table. */
                };
        } PESy_Name;

        PED_DWORD PESy_Value;    /* Value associated with the symbol. The
                                    interpretation of this field depends on
                                    Section Number and Storage Class. A typical
                                    meaning is the relocatable address. */

        PED_WORD  PESy_SectionNumber;  /* Signed integer identifying the
                                          section, using a one-based index into
                                          the Section Table. Some values have
                                          special meaning defined in 5.4.2.
                                          Section Number Values */

        PED_WORD  PESy_Type;    /* A number representing type. Microsoft tools
                                   set this field to 0x20 (function) or 0x0 (not
                                   a function). See Section 5.4.3, ¿Type
                                   Representation,¿ for more information. */

        char PESy_StorageClass;    /* Enumerated value representing storage
                                      class.  See Section 5.4.4, ¿Storage
                                      Class,¿ for more information. */

        char PESy_NumberOfAuxSymbols;    /* Number of auxiliary symbol table
                                            entries that follow this record. */
};

/*
   5.4.2. Section Number Values

   Normally, the Section Value field in a symbol table entry is a
   one-based index into the Section Table. However, this field is a
   signed integer and may take negative values. The following values,
   less than one, have special meanings:
 */

#define IMAGE_SYM_UNDEFINED    0    /* Symbol record is not yet assigned a
                                       section. If the value is 0 this indicates
                                       a references to an external symbol
                                       defined elsewhere. If the value is
                                       non-zero this is a common symbol with a
                                       size specified by the value. */
#define IMAGE_SYM_ABSOLUTE    -1    /* The symbol has an absolute
                                       (non-relocatable) value and is not an
                                       address. */
#define IMAGE_SYM_DEBUG       -2    /* The symbol provides general type or
                                       debugging information but does not
                                       correspond to a section.  Microsoft tools
                                       use this setting along with .file records
                                       (storage class FILE). */

/*
   5.4.3 Type Representation

   The Type field of a symbol table entry contains two bytes, each byte
   representing type information. The least-significant byte represents simple
   (base) data type, and the most-significant byte represents complex type, if
   any:

   MSB   Complex type: none, pointer, function, array.
   LSB   Base type: integer, floating-point, etc.

   The following values are defined for base type, although Microsoft tools
   generally do not use this field, setting the least-significant byte to 0.
   Instead, CodeView information is used to indicate types. However, the
   possible COFF values are listed here for completeness.
 */

#define IMAGE_SYM_TYPE_NULL     0 /* No type information or unknown base type.
                                     Microsoft tools use this setting. */
#define IMAGE_SYM_TYPE_VOID     1 /* No valid type; used with void pointers and
                                     functions. */
#define IMAGE_SYM_TYPE_CHAR     2 /* Character (signed byte). */
#define IMAGE_SYM_TYPE_SHORT    3 /* Two-byte signed integer. */
#define IMAGE_SYM_TYPE_INT      4 /* Natural integer type (normally four bytes
                                     in Windows NT). */
#define IMAGE_SYM_TYPE_LONG     5 /* Four-byte signed integer. */
#define IMAGE_SYM_TYPE_FLOAT    6 /* Four-byte floating-point number. */
#define IMAGE_SYM_TYPE_DOUBLE   7 /* Eight-byte floating-point number. */
#define IMAGE_SYM_TYPE_STRUCT   8 /* Structure. */
#define IMAGE_SYM_TYPE_UNION    9 /* Union. */
#define IMAGE_SYM_TYPE_ENUM    10 /* Enumerated type. */
#define IMAGE_SYM_TYPE_MOE     11 /* Member of enumeration (a specific value).
                                     */
#define IMAGE_SYM_TYPE_BYTE    12 /* Byte; unsigned one-byte integer. */
#define IMAGE_SYM_TYPE_WORD    13 /* Word; unsigned two-byte integer. */
#define IMAGE_SYM_TYPE_UINT    14 /* Unsigned integer of natural size
                                     (normally, four define bytes). */
#define IMAGE_SYM_TYPE_DWORD   15 /* Unsigned four-byte integer. */

/*
The most significant byte specifies whether the symbol is a pointer to, function
returning, or array of the base type specified in the least significant byte.
Microsoft tools use this field only to indicate whether or not the symbol is a
function, so that the only two resulting values are 0x0 and 0x20 for the Type
field. However, other tools can use this field to communicate more information.
It is very important to specify the function attribute correctly. This
information is required for incremental linking to work correctly. For some
architectures the information may be required for other purposes.
*/

#define IMAGE_SYM_DTYPE_NULL      0   /* No derived type; the symbol is a
                                          simple scalar variable. */
#define IMAGE_SYM_DTYPE_POINTER   1   /* Pointer to base type. */
#define IMAGE_SYM_DTYPE_FUNCTION  2   /* Function returning base type. */
#define IMAGE_SYM_DTYPE_ARRAY     3   /* Array of base type. */


/*
   5.4.4. Storage Class

   The Storage Class field of the Symbol Table indicates what kind of definition
   a symbol represents. The following table shows possible values. Note that the
   Storage Class field is an unsigned one-byte integer. The special value -1
   should therefore be taken to mean its unsigned equivalent, 0xFF.

   Although traditional COFF format makes use of many storage-class values,
   Microsoft tools rely on CodeView format for most symbolic information and
   generally use only four storage-class values: EXTERNAL (2), STATIC (3),
   FUNCTION (101), and STATIC (103). Except in the second column heading below,
   ¿Value¿ should be taken to mean the Value field of the symbol record (whose
   interpretation depends on the number found as the storage class).
 */

#define IMAGE_SYM_CLASS_END_OF_FUNCTION -1  /* (0xFF) A special symbol that
                                               represents the end of function,
                                               for debugging purposes. */
#define IMAGE_SYM_CLASS_NULL             0  /* No assigned storage class. */
#define IMAGE_SYM_CLASS_AUTOMATIC        1  /* The automatic (stack) variable.
                                               The Value field specifies the
                                               stack frame offset. */
#define IMAGE_SYM_CLASS_EXTERNAL         2  /* A value that Microsoft tools use
                                               for external symbols. The Value
                                               field indicates the size if the
                                               section number is
                                               IMAGE_SYM_UNDEFINED (0). If the
                                               section number is not zero, then
                                               the Value field specifies the
                                               offset within the section. */
#define IMAGE_SYM_CLASS_STATIC           3  /* The offset of the symbol within
                                               the section. If the Value field
                                               is zero, then the symbol
                                               represents a section name. */
#define IMAGE_SYM_CLASS_REGISTER         4  /* A register variable. The Value
                                               field specifies the register
                                               number. */
#define IMAGE_SYM_CLASS_EXTERNAL_DEF     5  /* A symbol that is defined
                                               externally. */
#define IMAGE_SYM_CLASS_LABEL            6  /* A code label that is defined
                                               within the module. The Value
                                               field specifies the offset of the
                                               symbol within the section. */
#define IMAGE_SYM_CLASS_UNDEFINED_LABEL  7  /* A reference to a code label that
                                               is not defined. */
#define IMAGE_SYM_CLASS_MEMBER_OF_STRUCT 8  /* The structure member. The Value
                                               field specifies the n th member.
                                               */
#define IMAGE_SYM_CLASS_ARGUMENT         9  /* A formal argument (parameter) of
                                               a function. The Value field
                                               specifies the n th argument. */
#define IMAGE_SYM_CLASS_STRUCT_TAG      10  /* The structure tag-name entry. */
#define IMAGE_SYM_CLASS_MEMBER_OF_UNION 11  /* A union member. The Value field
                                               specifies the n th member. */
#define IMAGE_SYM_CLASS_UNION_TAG       12  /* The Union tag-name entry. */
#define IMAGE_SYM_CLASS_TYPE_DEFINITION 13  /* A Typedef entry. */
#define IMAGE_SYM_CLASS_UNDEFINED_STATIC 14  /* A static data declaration. */
#define IMAGE_SYM_CLASS_ENUM_TAG         15  /* An enumerated type tagname
                                                entry. */
#define IMAGE_SYM_CLASS_MEMBER_OF_ENUM   16  /* A member of an enumeration. The
                                                Value field specifies the n th
                                                member. */
#define IMAGE_SYM_CLASS_REGISTER_PARAM   17  /* A register parameter. */
#define IMAGE_SYM_CLASS_BIT_FIELD        18  /* A bit-field reference. The Value
                                                field specifies the n th bit in
                                                the bit field. */
#define IMAGE_SYM_CLASS_BLOCK           100  /* A .bb (beginning of block) or
                                                .eb (end of block) record. The
                                                Value field is the relocatable
                                                address of the code location. */
#define IMAGE_SYM_CLASS_FUNCTION   101  /* A value that Microsoft tools use for
                                           symbol records that define the extent
                                           of a function: begin function (.bf ),
                                           end function ( .ef ), and lines in
                                           function ( .lf ). For .lf records,
                                           the Value field gives the number of
                                           source lines in the function. For .ef
                                           records, the Value field gives the
                                           size of the function code. */
#define IMAGE_SYM_CLASS_END_OF_STRUCT   102  /* An end-of-structure entry. */
#define IMAGE_SYM_CLASS_FILE            103  /* A value that Microsoft tools, as
                                                well as traditional COFF format,
                                                use for the source-file symbol
                                                record. The symbol is followed
                                                by auxiliary records that name
                                                the file. */
#define IMAGE_SYM_CLASS_SECTION         104  /* A definition of a section
                                                (Microsoft tools use STATIC
                                                storage class instead).  */
#define IMAGE_SYM_CLASS_WEAK_EXTERNAL   105 /* A weak external. For more
                                               information, see
                                               Auxiliary Format 3: Weak
                                               Externals.  */
#define IMAGE_SYM_CLASS_CLR_TOKEN       107  /* A CLR token symbol. The name is
                                                an ASCII string that consists of
                                                the hexadecimal value of the
                                                token. For more information, see
                                                CLR Token Definition (Object
                                                Only). */

/*
 * COMDAT Sections (Object only)
 */
#define IMAGE_COMDAT_SELECT_NODUPLICATES 1  /* If this symbol is already
                                               defined, the linker issues a
                                               "multiply defined symbol" error.
                                               */
#define IMAGE_COMDAT_SELECT_ANY          2  /* Any section that defines the same
                                               COMDAT symbol can be linked; the
                                               rest are removed. */
#define IMAGE_COMDAT_SELECT_SAME_SIZE    3  /* The linker chooses an arbitrary
                                               section among the definitions for
                                               this symbol. If all definitions
                                               are not the same size, a
                                               "multiply defined symbol" error
                                               is issued. */
#define IMAGE_COMDAT_SELECT_EXACT_MATCH  4  /* The linker chooses an arbitrary
                                               section among the definitions for
                                               this symbol. If all definitions
                                               do not match exactly, a "multiply
                                               defined symbol" error is issued.
                                               */
#define IMAGE_COMDAT_SELECT_ASSOCIATIVE  5  /* The section is linked if a
                                               certain other COMDAT section is
                                               linked. This other section is
                                               indicated by the Number field of
                                               the auxiliary symbol record for
                                               the section definition. This
                                               setting is useful for definitions
                                               that have components in multiple
                                               sections (for example, code in
                                               one and data in another), but
                                               where all must be linked or
                                               discarded as a set. The other
                                               section with which this section
                                               is associated must be a COMDAT
                                               section; it cannot be another
                                               associative COMDAT section (that
                                               is, the other section cannot have
                                               IMAGE_COMDAT_SELECT_ASSOCIATIVE
                                               set). */
#define IMAGE_COMDAT_SELECT_LARGEST      6  /* The linker chooses the largest
                                               definition from among all of the
                                               definitions for this symbol. If
                                               multiple definitions have this
                                               size, the choice between them is
                                               arbitrary. */


INTERNAL
void write_PED_BYTE(FILE *f, PED_BYTE x)
{
        fputc(x, f);
}

INTERNAL
void write_PED_WORD(FILE *f, PED_WORD x)
{
        fputc(x >> 0 & 0xFF, f);
        fputc(x >> 8 & 0xFF, f);
}

INTERNAL
void write_PED_DWORD(FILE *f, PED_DWORD x)
{
        fputc(x >>  0 & 0xFF, f);
        fputc(x >>  8 & 0xFF, f);
        fputc(x >> 16 & 0xFF, f);
        fputc(x >> 24 & 0xFF, f);
}

INTERNAL
void write_PE_FileHeader(FILE *f, const struct PE_FileHeader *x)
{
        //write_PED_DWORD (f, x->PEH_Signature);
        write_PED_WORD  (f, x->PEH_Machine);
        write_PED_WORD  (f, x->PEH_NumberOfSections);
        write_PED_DWORD (f, x->PEH_TimeDataStamp);
        write_PED_DWORD (f, x->PEH_PointerToSymbolTable);
        write_PED_DWORD (f, x->PEH_NumberOfSymbols);
        write_PED_WORD  (f, x->PEH_SizeOfOptionalHeader);
        write_PED_WORD  (f, x->PEH_Characteristics);
}

INTERNAL UNUSEDFUNC
void write_PE_OptionalHeader(FILE *f, const struct PE_OptionalHeader *x)
{
        write_PED_WORD   (f, x->PEO_Magic);
        write_PED_BYTE   (f, x->PEO_MajorLinkerVersion);
        write_PED_BYTE   (f, x->PEO_MinorLinkerVersion);
        write_PED_DWORD  (f, x->PEO_SizeOfCode);
        write_PED_DWORD  (f, x->PEO_SizeOfInitializedData);
        write_PED_DWORD  (f, x->PEO_SizeOfUninitializedData);
        write_PED_DWORD  (f, x->PEO_AddressOfEntryPoint);
        write_PED_DWORD  (f, x->PEO_BaseOfCode);
        write_PED_DWORD  (f, x->PEO_BaseOfData);
        write_PED_DWORD  (f, x->PEO_ImageBase);
        write_PED_DWORD  (f, x->PEO_SectionAlignment);
        write_PED_DWORD  (f, x->PEO_FileAlignment);
        write_PED_WORD   (f, x->PEO_MajorOperatingSystemVersion);
        write_PED_WORD   (f, x->PEO_MinorOperatingSystemVersion);
        write_PED_WORD   (f, x->PEO_MajorImageVersion);
        write_PED_WORD   (f, x->PEO_MinorImageVersion);
        write_PED_WORD   (f, x->PEO_MajorSubsystemVersion);
        write_PED_WORD   (f, x->PEO_MinorSubsystemVersion);
        write_PED_DWORD  (f, x->PEO_Reserved1);
        write_PED_DWORD  (f, x->PEO_SizeOfImage);
        write_PED_DWORD  (f, x->PEO_SizeOfHeaders);
        write_PED_DWORD  (f, x->PEO_CheckSum);
        write_PED_WORD   (f, x->PEO_Subsystem);
        write_PED_WORD   (f, x->PEO_DllCharacteristics);
        write_PED_DWORD  (f, x->PEO_SizeOfStackReserve);
        write_PED_DWORD  (f, x->PEO_SizeOfStackCommit);
        write_PED_DWORD  (f, x->PEO_SizeOfHeapReserve);
        write_PED_DWORD  (f, x->PEO_SizeOfHeapCommit);
        write_PED_DWORD  (f, x->PEO_LoaderFlags);
        write_PED_DWORD  (f, x->PEO_NumberOfRvaAndSizes);
}

INTERNAL
void write_PE_SectionHeader(FILE *f, const struct PE_SectionHeader *x)
{
        fwrite(x->PES_Name, sizeof x->PES_Name, 1, f);
        write_PED_DWORD  (f, x->PES_VirtualSize);
        write_PED_DWORD  (f, x->PES_VirtualAddress);
        write_PED_DWORD  (f, x->PES_SizeOfRawData);
        write_PED_DWORD  (f, x->PES_PointerToRawData);
        write_PED_DWORD  (f, x->PES_PointerToRelocations);
        write_PED_DWORD  (f, x->PES_PointerToLinenumbers);
        write_PED_WORD   (f, x->PES_NumberOfRelocations);
        write_PED_WORD   (f, x->PES_NumberOfLinenumbers);
        write_PED_DWORD  (f, x->PES_Characteristics);
}

INTERNAL
void write_PE_Relocation(FILE *f, const struct PE_Relocation *x)
{
        write_PED_DWORD  (f, x->PER_VirtualAddress);
        write_PED_DWORD  (f, x->PER_SymbolTableIndex);
        write_PED_WORD   (f, x->PER_Type);
}

INTERNAL
void write_PE_Symbol(FILE *f, const struct PE_Symbol *x)
{
        /* XXX: not clear about byte order??? */
        write_PED_DWORD(f, x->PESy_Name.zeroes);
        write_PED_DWORD(f, x->PESy_Name.offset);
        /**/

        write_PED_DWORD(f, x->PESy_Value);
        write_PED_WORD(f, x->PESy_SectionNumber);
        write_PED_WORD(f, x->PESy_Type);
        fputc(x->PESy_StorageClass, f);
        fputc(x->PESy_NumberOfAuxSymbols, f);
}


/* The sections that we use to map our own code to PE format */
enum {
        PESEC_DATA,  /* initialized data */
        PESEC_RDATA, /* read-only initialized data */
        PESEC_BSS,   /* uninitialized data */
        PESEC_TEXT,  /* executable code */
        NUM_PESEC_KINDS,
};


const char *const pesecName[NUM_PESEC_KINDS] = {
        [PESEC_DATA]  = ".data",
        [PESEC_RDATA] = ".rdata",
        [PESEC_BSS]   = ".bss",
        [PESEC_TEXT]  = ".text",
};

int sectionToPe64section[NUM_SECTIONS] = {
        [SECTION_CODE] = PESEC_TEXT,
        [SECTION_DATA] = PESEC_DATA,
        [SECTION_RODATA] = PESEC_RDATA,
        [SECTION_ZERODATA] = PESEC_BSS,
};

INTERNAL int pe64symCnt;
INTERNAL int pe64relocCnt;

INTERNAL struct PE_Relocation *pe64relocTab;
INTERNAL struct PE_Symbol *pe64symTab;
INTERNAL int *symbolToPe64sym;
INTERNAL int pe64sectionToPe64sym[NUM_PESEC_KINDS];

INTERNAL struct Alloc pe64relocTabAlloc;
INTERNAL struct Alloc pe64symTabAlloc;
INTERNAL struct Alloc symbolToPe64symAlloc;


INTERNAL
int pe64_add_to_strtab(const char *str, int len)
{
        int pos = pe64strCnt;
        pe64strCnt += len + 1;
        RESIZE_GLOBAL_BUFFER(pe64strtab, pe64strCnt);
        copy_mem(&pe64strtab[pos], str, len);
        pe64strtab[pos+len] = '\0';
        return pos;
}

INTERNAL
void set_PE_Section_Name(struct PE_SectionHeader *shdr, const char *name)
{
        int len = cstr_length(name);

        /* If the length is longer than 8 characters, we can store it somewhere
         * else. But that scheme is not implemented currently! */
        ASSERT(len <= 8);

        clear_mem(&shdr->PES_Name, 8);
        copy_mem(&shdr->PES_Name, name, len);
}

INTERNAL
void set_PE_Symbol_Name(struct PE_Symbol *stab, const char *name)
{
        int len = cstr_length(name);

        /* we put all symbol names in the string table */
        stab->PESy_Name.zeroes = 0;
        /* We need to add 4 because the first 4 bytes in the on-disk
         * representation are occupied by the length field indicating the size
         * of the string table */
        stab->PESy_Name.offset = pe64_add_to_strtab(name, len) + 4;
}

INTERNAL
void write_freak_show_section_symbol(FILE *f, int pesec, int sectionSize)
{
        struct PE_Symbol pesym = {0};
        set_PE_Symbol_Name(&pesym, pesecName[pesec]);
        pesym.PESy_Value = 0;  // XXX:??? or sectionSize???
        pesym.PESy_SectionNumber = (PED_WORD) (pesec + 1);
        pesym.PESy_Type = 0x00;
        pesym.PESy_StorageClass = IMAGE_SYM_CLASS_STATIC;
        pesym.PESy_NumberOfAuxSymbols = 1;

        write_PE_Symbol(f, &pesym);

        /*
         * now the extra special freak goo
         */

        write_PED_DWORD(f, sectionSize);   /* The size of section data; the same
                                              as SizeOfRawData in the section
                                              header. */
        write_PED_WORD(f, 0 /*XXX*/); /* The number of relocation entries for this
                                   section */
        write_PED_WORD(f, 0);   /* The number of line-number entries for this
                                   section */
        write_PED_DWORD(f, 0);  /* The checksum for communal data. It is
                                   applicable if the IMAGE_SCN_LINK_COMDAT
                                   flag is set in the section header. For more
                                   information see... */
        write_PED_WORD(f, 0);  /* One-based index into the section
                                          table for the associated section.
                                          This is used when the COMDAT selection
                                          setting is 5. */
        write_PED_BYTE(f, 0);   /* The COMDAT selection number. This is
                                   applicable if the section is a COMDAT section
                                   */
        /* 3 more unused bytes */
        write_PED_BYTE(f, 0);
        write_PED_BYTE(f, 0);
        write_PED_BYTE(f, 0);
}

void write_pe64_object(const char *filepath)
{
        FILE *f = fopen(filepath, "wb");
        if (f == NULL)
                FATAL("Failed to open %s\n", filepath);

        BUF_INIT(&pe64relocTab, &pe64relocTabAlloc);
        BUF_INIT(&pe64symTab, &pe64symTabAlloc);
        BUF_INIT(&symbolToPe64sym, &symbolToPe64symAlloc);

        BUF_RESERVE(&symbolToPe64sym, &symbolToPe64symAlloc, symbolCnt);
        for (Symbol sym = 0; sym < symbolCnt; sym++)
                symbolToPe64sym[sym] = -1;

        /* Add section symbols */
        for (int i = 0; i < NUM_PESEC_KINDS; i++) {
                int x = pe64symCnt++;
                BUF_RESERVE(&pe64symTab, &pe64symTabAlloc, pe64symCnt);
                set_PE_Symbol_Name(&pe64symTab[x], pesecName[i]);
                pe64symTab[x].PESy_Value = 0;
                pe64symTab[x].PESy_Type = 0x00;
                // TODO: only export symbols that have an export statement
                pe64symTab[x].PESy_StorageClass = IMAGE_SYM_CLASS_STATIC;
                pe64symTab[x].PESy_SectionNumber = (PED_WORD)(i + 1);  // 1-based
                pe64symTab[x].PESy_NumberOfAuxSymbols = 0;
                pe64sectionToPe64sym[i] = x;
        }

        /* Add defined symbols */
        for (int i = 0; i < symdefCnt; i++) {
                int isProc;
                int sectionNumber;
                int value = symdefInfo[i].offset;
                Symbol sym = symdefInfo[i].symbol;
                int sectionKind = symdefInfo[i].sectionKind;  // SECTION_
                switch (sectionKind) {
                case SECTION_DATA:
                        ASSERT(symbolInfo[sym].symbolKind == SYMBOL_DATA);
                        ASSERT(symbolInfo[sym].tData.optionaldata != -1);
                        isProc = 0;
                        sectionNumber = PESEC_DATA;
                        break;
                case SECTION_RODATA:
                        ASSERT(symbolInfo[sym].symbolKind == SYMBOL_DATA);
                        ASSERT(symbolInfo[sym].tData.optionaldata != -1);
                        isProc = 0;
                        sectionNumber = PESEC_RDATA;
                        break;
                case SECTION_ZERODATA:
                        ASSERT(symbolInfo[sym].symbolKind == SYMBOL_DATA);
                        ASSERT(symbolInfo[sym].tData.optionaldata != -1);
                        isProc = 0;
                        sectionNumber = PESEC_BSS;
                        break;
                case SECTION_CODE:
                        ASSERT(symbolInfo[sym].symbolKind == SYMBOL_PROC);
                        ASSERT(symbolInfo[sym].tProc.optionalproc != -1);
                        isProc = 1;
                        sectionNumber = PESEC_TEXT;
                        break;
                default:
                        UNHANDLED_CASE();
                }

                int x = pe64symCnt++;
                BUF_RESERVE(&pe64symTab, &pe64symTabAlloc, pe64symCnt);
                set_PE_Symbol_Name(&pe64symTab[x], SS(sym));
                pe64symTab[x].PESy_Value = value;  // XXX
                pe64symTab[x].PESy_Type = isProc ? 0x20 : 0x00;
                // TODO: only export symbols that have an export statement
                pe64symTab[x].PESy_StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
                pe64symTab[x].PESy_SectionNumber = (PED_WORD) (sectionNumber + 1);  // 1-based
                pe64symTab[x].PESy_NumberOfAuxSymbols = 0;

                symbolToPe64sym[sym] = x;
        }
        /* Add undefined symbols */
        for (Symbol sym = 0; sym < symbolCnt; sym++) {
                if (scopeInfo[symbolInfo[sym].scope].scopeKind != SCOPE_GLOBAL)
                        continue;
                int isProc;
                switch (symbolInfo[sym].symbolKind) {
                case SYMBOL_PROC:
                        if (symbolInfo[sym].tProc.optionalproc != -1)
                                continue;  /* symbol is defined */
                        isProc = 1;
                        break;
                case SYMBOL_DATA:
                        if (symbolInfo[sym].tData.optionaldata != -1)
                                continue;  /* symbol is defined */
                        isProc = 0;
                        break;
                default:
                        continue;
                }

                int x = pe64symCnt++;
                BUF_RESERVE(&pe64symTab, &pe64symTabAlloc, pe64symCnt);
                set_PE_Symbol_Name(&pe64symTab[x], SS(sym));
                pe64symTab[x].PESy_Value = 0;  // see IMAGE_SYM_UNDEFINED
                pe64symTab[x].PESy_Type = isProc ? 0x20 : 0x00;
                pe64symTab[x].PESy_StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
                pe64symTab[x].PESy_SectionNumber = IMAGE_SYM_UNDEFINED;
                pe64symTab[x].PESy_NumberOfAuxSymbols = 0;

                symbolToPe64sym[sym] = x;
        }

        /* Add relocations */
        for (int i = 0; i < relocCnt; i++) {
                int relocKind = relocInfo[i].relocKind;
                int addend = relocInfo[i].addend;
                int offset = relocInfo[i].codepos;
                int pesym;

                if (relocKind == RELOC_SECTION_RELATIVE) {
                        int sectionKind = relocInfo[i].tSectionKind;
                        int esec = sectionToPe64section[sectionKind];
                        pesym = pe64sectionToPe64sym[esec];
                        DEBUG("pesym is %d\n", pesym);
                }
                else if (relocKind == RELOC_SYMBOL_RELATIVE) {
                        Symbol symbol = relocInfo[i].tSymbol;
                        if (addend == 0) {
                                pesym = symbolToPe64sym[symbol];
                                if (pesym == -1)
                                        FATAL("There is a relocation for symbol %s "
                                                "which is not in the symbol table "
                                                "(is this an internal error?)\n",
                                                SS(symbol));
                        }
                        else {
                                ASSERT(addend > 0);
                                /* Make pesym a new unnamed symbol based on
                                the reference symbol */
                                int ref = symbolToPe64sym[symbol];
                                ASSERT(ref != -1); // should always be true
                                /* addend > 0 should only be the case for jumps to
                                 * function+offsets places. These functions must be
                                 * defined. */
                                ASSERT(pe64symTab[ref].PESy_SectionNumber > 0);
                                int x = pe64symCnt++;
                                BUF_RESERVE(&pe64symTab, &pe64symTabAlloc, pe64symCnt);
                                set_PE_Symbol_Name(&pe64symTab[x], "");
                                pe64symTab[x].PESy_Value =
                                        pe64symTab[ref].PESy_Value + addend;
                                pe64symTab[x].PESy_Type = 0x00;  // ???
                                pe64symTab[x].PESy_StorageClass = IMAGE_SYM_CLASS_STATIC;
                                pe64symTab[x].PESy_SectionNumber =
                                        pe64symTab[ref].PESy_SectionNumber;
                                pe64symTab[x].PESy_NumberOfAuxSymbols = 0;

                                pesym = x;
                        }
                }
                else {
                        UNHANDLED_CASE();
                }

                int x = pe64relocCnt++;
                BUF_RESERVE(&pe64relocTab, &pe64relocTabAlloc, pe64relocCnt);
                pe64relocTab[x].PER_VirtualAddress = offset;
                pe64relocTab[x].PER_SymbolTableIndex = pesym;
                pe64relocTab[x].PER_Type = IMAGE_REL_AMD64_ADDR64;
        }

        struct PE_SectionHeader sh[NUM_PESEC_KINDS];
        CLEAR(sh);

        for (int i = 0; i < NUM_PESEC_KINDS; i++)
                set_PE_Section_Name(&sh[i], pesecName[i]);

        sh[PESEC_DATA].PES_Characteristics =
                                        IMAGE_SCN_CNT_INITIALIZED_DATA |
                                        IMAGE_SCN_MEM_READ |
                                        IMAGE_SCN_MEM_WRITE |
                                        IMAGE_SCN_ALIGN_16BYTES;
        sh[PESEC_RDATA].PES_Characteristics =
                                        IMAGE_SCN_CNT_INITIALIZED_DATA |
                                        IMAGE_SCN_MEM_READ |
                                        IMAGE_SCN_ALIGN_16BYTES;
        sh[PESEC_BSS].PES_Characteristics =
                                        IMAGE_SCN_CNT_UNINITIALIZED_DATA |
                                        IMAGE_SCN_MEM_READ |
                                        IMAGE_SCN_MEM_WRITE |
                                        IMAGE_SCN_ALIGN_16BYTES;
        sh[PESEC_TEXT].PES_Characteristics =
                                        IMAGE_SCN_CNT_CODE |
                                        IMAGE_SCN_MEM_EXECUTE |
                                        IMAGE_SCN_MEM_READ |
                                        IMAGE_SCN_ALIGN_16BYTES;

        sh[PESEC_DATA ].PES_VirtualSize = dataSectionCnt;
        sh[PESEC_RDATA].PES_VirtualSize = rodataSectionCnt;
        sh[PESEC_BSS  ].PES_VirtualSize = zerodataSectionCnt;
        sh[PESEC_TEXT ].PES_VirtualSize = codeSectionCnt;

        sh[PESEC_DATA ].PES_SizeOfRawData = dataSectionCnt;
        sh[PESEC_RDATA].PES_SizeOfRawData = rodataSectionCnt;
        sh[PESEC_BSS  ].PES_SizeOfRawData = 0;
        sh[PESEC_TEXT ].PES_SizeOfRawData = codeSectionCnt;

        /* sections come after section headers */
        sh[PESEC_DATA ].PES_PointerToRawData =
                IMAGE_SIZEONDISK_OF_FileHeader +
                (IMAGE_SIZEONDISK_OF_SectionHeader * NUM_PESEC_KINDS);
#define AFTER(x) (sh[x].PES_PointerToRawData + sh[x].PES_SizeOfRawData)
        sh[PESEC_RDATA].PES_PointerToRawData = AFTER(PESEC_DATA);
        sh[PESEC_BSS  ].PES_PointerToRawData = AFTER(PESEC_RDATA);
        sh[PESEC_TEXT ].PES_PointerToRawData = AFTER(PESEC_BSS);

        /* XXX: PE wants to have the relocation sorted by section. Currently we
         * only have relocations for the .text section, but that will change.
         */
        sh[PESEC_TEXT ].PES_PointerToRelocations = AFTER(PESEC_TEXT);
        sh[PESEC_TEXT ].PES_NumberOfRelocations = (PED_WORD) pe64relocCnt;

        struct PE_FileHeader fh;
        CLEAR(fh);

        fh.PEH_Machine = IMAGE_FILE_MACHINE_AMD64;
        fh.PEH_NumberOfSections = NUM_PESEC_KINDS;
        fh.PEH_TimeDataStamp = 0;
        fh.PEH_PointerToSymbolTable = AFTER(PESEC_TEXT)
                + pe64relocCnt * IMAGE_SIZEONDISK_OF_Reloc; // ACHTUNG
#undef AFTER
        /* Note: we have normal symbols + (2 * NUM_PESEC_KINDS) freak show
         * symbols. See below */
        fh.PEH_NumberOfSymbols = pe64symCnt + 2*NUM_PESEC_KINDS;
        fh.PEH_SizeOfOptionalHeader = 0;
        fh.PEH_Characteristics = IMAGE_FILE_LINE_NUMS_STRIPPED |
                                 IMAGE_FILE_LOCAL_SYMS_STRIPPED |
                                 IMAGE_FILE_DEBUG_STRIPPED |
                                 IMAGE_FILE_LARGE_ADDRESS_AWARE;

        /*
        for (int i = 0; i < NUM_PESEC_KINDS; i++)
                DEBUG("pointer to rawdata of secton %d: %d\n",
                      i, (int) sh[i].PES_PointerToRawData);
                      */

        /* Write File Header */
        write_PE_FileHeader(f, &fh);

        /* !! No optional header. This is an .obj file. */

        /*
         * XXX: FIXUP:
         *
         * ok... PECOFF has it totally wrong -.-
         * It seems like a needless special case for BSS. We set the correct
         * values (as seen by PECOFF) only shortly before committing to disk,
         * so that the rest of our logic isn't affected by the weirdness.
         */
        sh[PESEC_BSS  ].PES_SizeOfRawData = sh[PESEC_BSS].PES_VirtualSize;
        sh[PESEC_BSS  ].PES_VirtualSize = 0;
        sh[PESEC_BSS  ].PES_PointerToRawData = 0;
        /*
         * Write Section Header Table
         */
        for (int i = 0; i < NUM_PESEC_KINDS; i++)
                write_PE_SectionHeader(f, &sh[i]);
        /*
         * XXX: Revert this stupidity again because we have more logic depending
         * on it.
         */
        sh[PESEC_BSS  ].PES_VirtualSize = sh[PESEC_BSS].PES_SizeOfRawData;
        sh[PESEC_BSS  ].PES_SizeOfRawData = 0;

        /* Write section data */
        fwrite(dataSection, dataSectionCnt, 1, f);
        fwrite(rodataSection, rodataSectionCnt, 1, f);
        fwrite(codeSection, codeSectionCnt, 1, f);

        /* Write relocations */
        for (int i = 0; i < pe64relocCnt; i++)
                write_PE_Relocation(f, &pe64relocTab[i]);

        /* Write symbol table */
        for (int i = 0; i < pe64symCnt; i++)
                write_PE_Symbol(f, &pe64symTab[i]);

        /* We need to emit symbols for each section, too. Each section symbol
         * is followed up by one auxiliary record that has the same size as
         * symbol definitions but a different frankenstein format. Who the hell
         * invented this crap? */
        for (int i = 0; i < NUM_PESEC_KINDS; i++)
                write_freak_show_section_symbol(f, i, sh[i].PES_VirtualSize);

        /* Write string table */
        write_PED_DWORD(f, 4 + pe64strCnt);
        fwrite(pe64strtab, pe64strCnt, 1, f);

        if (ferror(f))
                FATAL("Errors while writing file %s\n", filepath);
        if (fclose(f))
                FATAL("Errors while closing file %s\n", filepath);

        BUF_EXIT(&pe64relocTab, &pe64relocTabAlloc);
        BUF_EXIT(&pe64symTab, &pe64symTabAlloc);
        BUF_EXIT(&symbolToPe64sym, &symbolToPe64symAlloc);
}
