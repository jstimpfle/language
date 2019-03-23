#ifndef API_H_INCLUDED
#error This file must be included only from api.h !
#endif

/* Options selected at process invocation */
DATA int doDebug;
DATA int doPrettyPrintAst;
DATA int doDumpIr;
DATA int doPrintHelpString;
DATA int doWriteElfFile;
DATA int doWritePecoffFile;
DATA String ELF_ObjectFilepath;
DATA String PECOFF_ObjectFilepath;
