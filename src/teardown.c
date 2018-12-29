#include "defs.h"
#include "api.h"

void teardown_program(void)
{
        for (File i = 0; i < fileCnt; i++)
                BUF_EXIT(&fileInfo[i].buf, &fileInfo[i].bufAlloc);
        for (int i = 0; i < NUM_BUFFERS; i++)
                /* does not work because type of **globalBufferInfo[i].ptr is
                 * not known. */
                //BUF_EXIT(globalBufferInfo[i].ptr, &globalBufferAlloc[i]);
                /* This is the workaround. How to fix this ugly code? */
                _buf_exit(globalBufferInfo[i].ptr, &globalBufferAlloc[i],
                          globalBufferInfo[i].elemsize, __FILE__, __LINE__);
}
