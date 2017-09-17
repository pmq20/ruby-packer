@echo off
setlocal enabledelayedexpansion
REM	pq_test.bat

set TEST=Debug\pq_test.exe
if not exist %TEST% exit /b 1

set pq_output=pq_output.txt
if exist %pq_output% del %pq_output%

%TEST% > %pq_output%
fc /b %pq_output% %srcdir%\pq_expected.txt

endlocal
