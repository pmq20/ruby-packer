@echo off
setlocal enabledelayedexpansion
REM	ssltest.bat

set ssltest_bin=Debug\ssltest.exe
if not exist %ssltest_bin% exit /b 1

set openssl_bin=..\apps\openssl\Debug\openssl.exe
if not exist %openssl_bin% exit /b 1

if "%srcdir%"=="" (
	set srcdir=.
)

%srcdir%\testssl.bat %srcdir%\server.pem %srcdir%\server.pem %srcdir%\ca.pem ^
    %ssltest_bin% %openssl_bin%
if !errorlevel! neq 0 (
	exit /b 1
)

endlocal
