rem Build script for Windows (MSVC compiler).
rem You must setup the environment using vcvars64 before calling this script

@echo on
cl /W4 /Fe"blunt.exe" /I include\ src\*.c /Zi

@echo off
if %ERRORLEVEL% NEQ 0 (
    REM wtf everything about this environment is so illogical. Why can't I just
    REM echo to get a newline?
    echo.
    echo COMPILER ERROR %ERRORLEVEL%
    exit /B 1
)
