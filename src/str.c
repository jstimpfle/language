#include "defs.h"
#include "api.h"

INTERNAL
String add_string(const char *buf, int len)
{
        int pos = strbufCnt;
        strbufCnt += len + 1;
        RESIZE_GLOBAL_BUFFER(strbuf, strbufCnt);
        copy_mem(&strbuf[pos], buf, len);
        strbuf[pos + len] = '\0';

        String s = stringCnt;
        stringCnt += 1;
        // Allocate one more to support string_length()
        RESIZE_GLOBAL_BUFFER(stringInfo, stringCnt + 1);
        stringInfo[s].pos = pos;
        stringInfo[s+1].pos = pos + len + 1;

        return s;
}

INTERNAL
unsigned hash_string(const void *str, int len)
{
        unsigned hsh = 5381;
        for (int i = 0; i < len; i++)
                hsh = 33*hsh + ((const unsigned char *)str)[i];
        return hsh;
}

INTERNAL
String lookup_string_with_hash(const void *buf, int len, unsigned hsh)
{
        unsigned bck;
        String s;

        bck = hsh & (strBucketCnt - 1);
        for (s = strBucketInfo[bck].firstString; s != -1;
             s = stringInfo[s].next) {
                if (string_length(s) == len &&
                    compare_mem(string_buffer(s), buf, len) == 0)
                        return s;
        }
        return -1;
}

INTERNAL
void insert_string_with_hash(String s, unsigned hsh)
{
        unsigned bck;
       
        bck = hsh & (strBucketCnt - 1);
        stringInfo[s].next = strBucketInfo[bck].firstString;
        strBucketInfo[bck].firstString = s;
}

String intern_string(const void *buf, int len)
{
        int i;
        unsigned hsh;
        String s;

        /* resize hash map if load factor exceeds 66% */
        if (2 * strBucketCnt <= 3 * stringCnt) {
                if (strBucketCnt == 0)
                        strBucketCnt = 256;
                while (2 * strBucketCnt < 3 * stringCnt)
                        strBucketCnt *= 2;

                RESIZE_GLOBAL_BUFFER(strBucketInfo, strBucketCnt);
                for (i = 0; i < strBucketCnt; i++)
                        strBucketInfo[i].firstString = -1;

                for (s = 0; s < stringCnt; s++) {
                        hsh = hash_string(string_buffer(s), string_length(s));
                        insert_string_with_hash(s, hsh);
                }
        }
        // MUST BE POWER OF 2 !!!!
        ASSERT((strBucketCnt & (strBucketCnt-1)) == 0);

        hsh = hash_string(buf, len);
        s = lookup_string_with_hash(buf, len, hsh);
        if (s == -1) {
                s = add_string(buf, len);
                insert_string_with_hash(s, hsh);
        }
        return s;
}

String intern_cstring(const char *str)
{
        return intern_string((const void *)str, cstr_length(str));
}
