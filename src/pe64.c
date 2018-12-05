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
void write_PE_FileHeader(FILE *f, const struct PE_FileHeader *hdr)
{
        write_PED_DWORD (f, hdr->PEH_Signature);
        write_PED_WORD  (f, hdr->PEH_Machine);
        write_PED_WORD  (f, hdr->PEH_NumberOfSections);
        write_PED_DWORD (f, hdr->PEH_TimeDataStamp);
        write_PED_DWORD (f, hdr->PEH_PointerToSymbolTable);
        write_PED_WORD  (f, hdr->PEH_SizeOfOptionalHeader);
        write_PED_WORD  (f, hdr->PEH_Characteristics);
}

INTERNAL
void write_PE_OptionalHeader(FILE *f, const struct PE_OptionalHeader *hdr)
{
        write_PED_WORD   (f, hdr->PEO_Magic);
        write_PED_BYTE   (f, hdr->PEO_MajorLinkerVersion);
        write_PED_BYTE   (f, hdr->PEO_MinorLinkerVersion);
        write_PED_DWORD  (f, hdr->PEO_SizeOfCode);
        write_PED_DWORD  (f, hdr->PEO_SizeOfInitializedData);
        write_PED_DWORD  (f, hdr->PEO_SizeOfUninitializedData);
        write_PED_DWORD  (f, hdr->PEO_AddressOfEntryPoint);
        write_PED_DWORD  (f, hdr->PEO_BaseOfCode);
        write_PED_DWORD  (f, hdr->PEO_BaseOfData);
        write_PED_DWORD  (f, hdr->PEO_ImageBase);
        write_PED_DWORD  (f, hdr->PEO_SectionAlignment);
        write_PED_DWORD  (f, hdr->PEO_FileAlignment);
        write_PED_WORD   (f, hdr->PEO_MajorOperatingSystemVersion);
        write_PED_WORD   (f, hdr->PEO_MinorOperatingSystemVersion);
        write_PED_WORD   (f, hdr->PEO_MajorImageVersion);
        write_PED_WORD   (f, hdr->PEO_MinorImageVersion);
        write_PED_WORD   (f, hdr->PEO_MajorSubsystemVersion);
        write_PED_WORD   (f, hdr->PEO_MinorSubsystemVersion);
        write_PED_DWORD  (f, hdr->PEO_Reserved1);
        write_PED_DWORD  (f, hdr->PEO_SizeOfImage);
        write_PED_DWORD  (f, hdr->PEO_SizeOfHeaders);
        write_PED_DWORD  (f, hdr->PEO_CheckSum);
        write_PED_WORD   (f, hdr->PEO_Subsystem);
        write_PED_WORD   (f, hdr->PEO_DllCharacteristics);
        write_PED_DWORD  (f, hdr->PEO_SizeOfStackReserve);
        write_PED_DWORD  (f, hdr->PEO_SizeOfStackCommit);
        write_PED_DWORD  (f, hdr->PEO_SizeOfHeapReserve);
        write_PED_DWORD  (f, hdr->PEO_SizeOfHeapCommit);
        write_PED_DWORD  (f, hdr->PEO_LoaderFlags);
        write_PED_DWORD  (f, hdr->PEO_NumberOfRvaAndSizes);
}

INTERNAL
void pe64_add_to_strtab(const char *str, int len)
{
        int pos = pe64strCnt;
        pe64strCnt += len + 1;
        RESIZE_GLOBAL_BUFFER(pe64strtab, pe64strCnt);
        copy_mem(&pe64strtab[pos], str, len);
        pe64strtab[pos+len] = '\0';
}

void write_pe64_object(const char *filepath)
{
        struct PE_FileHeader fh;

        FILE *f = fopen(filepath, "wb");
        if (f == NULL)
                FATAL("Failed to open %s\n", filepath);

        CLEAR(fh);
        fh.PEH_Machine = IMAGE_FILE_MACHINE_AMD64;
        fh.PEH_NumberOfSections = 0;
        fh.PEH_TimeDataStamp = 0;
        fh.PEH_PointerToSymbolTable = 0;
        fh.PEH_SizeOfOptionalHeader = 0;
        fh.PEH_Characteristics = IMAGE_FILE_LINE_NUMS_STRIPPED |
                                 IMAGE_FILE_LOCAL_SYMS_STRIPPED;

        write_PE_FileHeader(f, &fh);

        if (ferror(f))
                FATAL("Errors while writing file %s\n", filepath);
        if (fclose(f))
                FATAL("Errors while closing file %s\n", filepath);
}
