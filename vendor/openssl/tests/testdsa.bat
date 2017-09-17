@echo off
setlocal enabledelayedexpansion
REM	testdsa.bat


REM # Test DSA certificate generation of openssl

set cmd=..\apps\openssl\Debug\openssl.exe
if not exist %cmd% exit /b 1

if "%srcdir%"=="" (
	set srcdir=.
)

REM # Generate DSA paramter set
%cmd% dsaparam 512 -out dsa512.pem
if !errorlevel! neq 0 (
	exit /b 1
)


REM # Generate a DSA certificate
%cmd% req -config %srcdir%\openssl.cnf -x509 -newkey dsa:dsa512.pem -out testdsa.pem -keyout testdsa.key
if !errorlevel! neq 0 (
	exit /b 1
)


REM # Now check the certificate
%cmd% x509 -text -in testdsa.pem
if !errorlevel! neq 0 (
	exit /b 1
)

del testdsa.key dsa512.pem testdsa.pem

exit /b 0
endlocal
