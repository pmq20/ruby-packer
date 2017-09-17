@echo off
setlocal enabledelayedexpansion
REM	tlstest.bat

set tlstest_bin=Debug\tlstest.exe
if not exist %tlstest_bin% exit /b 1

if "%srcdir%"=="" (
	set srcdir=.
)

%tlstest_bin% %srcdir%\ca.pem %srcdir%\server.pem %srcdir%\server.pem
if !errorlevel! neq 0 (
	exit /b 1
)

endlocal
