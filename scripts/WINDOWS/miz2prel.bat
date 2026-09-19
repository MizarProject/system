@echo off
rem   
rem                    PC Mizar exporter BATCH command
rem
if "%1"=="" goto end
if "%mizfiles%"=="" set mizfiles=c:\mizar
if exist %mizfiles%\exporter.exe goto mizfiles
echo Can't find %mizfiles%\exporter.exe
goto end
:mizfiles
%mizfiles%\accom %1
%mizfiles%\chkerr %1
if not ErrorLevel 1 goto continue
%mizfiles%\errflag %1
%mizfiles%\addfmsg %1 %mizfiles%\mizar
goto end
:continue
%mizfiles%\exporter %1
%mizfiles%\chkerr %1
if not ErrorLevel 1 goto exit
%mizfiles%\errflag %1
%mizfiles%\addfmsg %1 %mizfiles%\mizar
goto end
:exit
%mizfiles%\transfer %1
if exist %1.dno del %1.dno >nul
if exist %1.dcl del %1.dcl >nul
if exist %1.dco del %1.dco >nul
if exist %1.def del %1.def >nul
if exist %1.the del %1.the >nul
if exist %1.sch del %1.sch >nul
echo Local data base created.
:end

