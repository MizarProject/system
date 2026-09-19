@echo off
:;
:; The batch file for cleaning environment declaration.
:; Enter the filename without an extension. The article
:; should report no errors. The list of removed identifiers
:; is in the file clearenv.log.
:;
if "%1"=="" goto end
set MIZ_YES=
%MIZFILES%\accom text\%1
%MIZFILES%\chkerr text\%1
if ErrorLevel 1 goto finish
%MIZFILES%\verifier -qs text\%1
%MIZFILES%\chkerr text\%1
if ErrorLevel 1 goto finish
call :clrw32 %1 notations
call :clrw32 %1 definitions
call :clrw32 %1 expansions
call :clrw32 %1 equalities
call :clrw32 %1 requirements
call :clrw32 %1 registrations
call :clrw32 %1 constructors
%MIZFILES%\remflags text\%1.miz
goto bye
:end
echo.
echo        Enter article name as a parameter.
echo.
goto bye
:finish
echo.
echo   Your article reports errors. Remove them and try once more.
echo.
goto bye
:clrw32
%MIZFILES%\msplit text\%1
%MIZFILES%\commextr text\%1
echo 0 >text\%1.cospr.txt
echo %2>>text\%1.cospr.txt
echo. >>text\%1.cospr.txt
%MIZFILES%\clearenv text\%1 ZERO
set /P MIZ_NBR= <text\%1.cospr.txt
if %MIZ_NBR%==0 goto end
:loop
%MIZFILES%\clearenv text\%1 PREP %MIZ_NBR%
%MIZFILES%\createvd text\%1
%MIZFILES%\mglue text\%1
%MIZFILES%\accom text\%1
%MIZFILES%\errflag text\%1
%MIZFILES%\chkerr text\%1
if ErrorLevel 1 goto bad
%MIZFILES%\verifier -sq text\%1
%MIZFILES%\errflag text\%1
%MIZFILES%\chkerr text\%1
if ErrorLevel 1 goto bad
set MIZ_YES=NIE
%MIZFILES%\clearenv text\%1 %MIZ_YES% %MIZ_NBR%
goto good
:bad
set MIZ_YES=TAK
%MIZFILES%\clearenv text\%1 %MIZ_YES% %MIZ_NBR%
goto last
:good
:last
%MIZFILES%\createvd text\%1
%MIZFILES%\mglue text\%1
%MIZFILES%\accom text\%1
set /A MIZ_NBR-=1
if not %MIZ_NBR%==0 goto loop
:end
set MIZ_NBR=
:bye

