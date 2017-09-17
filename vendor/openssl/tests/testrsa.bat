@echo off
setlocal enabledelayedexpansion
REM	testrsa.bat


REM # Test RSA certificate generation of openssl

set cmd=..\apps\openssl\Debug\openssl.exe
if not exist %cmd% exit /b 1

if "%srcdir%"=="" (
	set srcdir=.
)

REM # Generate RSA private key
%cmd% genrsa -out rsakey.pem
if !errorlevel! neq 0 (
	exit /b 1
)


REM # Generate an RSA certificate
%cmd% req -config %srcdir%\openssl.cnf -key rsakey.pem -new -x509 -days 365 -out rsacert.pem
if !errorlevel! neq 0 (
	exit /b 1
)


REM # Now check the certificate
%cmd% x509 -text -in rsacert.pem
if !errorlevel! neq 0 (
	exit /b 1
)

del rsacert.pem rsakey.pem

exit /b 0
endlocal
