rem Build script for Windows (MSVC compiler).
rem You must setup the environment using vcvars64 before calling this script

mkdir build
cl /o build\language.exe /I include\ src\*.c
