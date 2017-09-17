@echo off
setlocal enabledelayedexpansion
REM	servertest.bat

set servertest_bin=Debug\servertest.exe
if not exist %servertest_bin% exit /b 1

if "%srcdir%"=="" (
	set srcdir=.
)

%servertest_bin% %srcdir%\server.pem %srcdir%\server.pem %srcdir%\ca.pem
if !errorlevel! neq 0 (
	exit /b 1
)

endlocal
