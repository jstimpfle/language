rem Build script for Windows (MSVC compiler).
rem You must setup the environment using vcvars64 before calling this script

if not exist build mkdir build
cl /Fe"build\language.exe" /I include\ src\*.c /Zi
