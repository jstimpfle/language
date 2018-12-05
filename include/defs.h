#ifdef _MSC_VER
  /* avoid silly warnings for "deprecated" functions like fopen(). Is there a
   * way to disable these warnings only for specific functions? */
  #define _CRT_SECURE_NO_WARNINGS
  #pragma warning(disable: 4201)  // warning C4201: nameless struct/union
  #pragma warning(disable: 4710)  // function not inlined
  #pragma warning(disable: 4820)  //  '4' bytes padding added after data member
#endif

#ifdef _MSC_VER
  #define UNUSED __pragma(warning(suppress: 4100 4101))
  #define UNUSEDFUNC
  #define NORETURN __declspec(noreturn)
  #define UNREACHABLE() ASSERT(0);
#else
  #define UNUSED __attribute__((unused))
  #define UNUSEDFUNC __attribute__((unused))
  #define NORETURN __attribute__((noreturn))
  #define UNREACHABLE() __builtin_unreachable()
#endif
#define INTERNAL static
