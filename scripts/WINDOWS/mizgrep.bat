@echo off
FOR /F "tokens=1 delims=" %%A in ('type %MIZFILES%\mml.lar') do grep -H %1 %2 %3  %MIZFILES%\%4\%%A.%5
