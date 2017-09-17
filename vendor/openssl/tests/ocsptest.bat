@echo off
setlocal enabledelayedexpansion
REM	ocsptest.bat

set TEST=Debug\ocsp_test.exe
if not exist %TEST% exit /b 1

%TEST% www.amazon.com 443 & if !errorlevel! neq 0 exit /b 1
%TEST% cloudflare.com 443 & if !errorlevel! neq 0 exit /b 1

endlocal
