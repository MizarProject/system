@echo off
rem   
rem                    PC Mizar Verifier,  BATCH command
rem
if "%1"=="" goto exit
for %%F in (A,a) do if "%1"=="/%%F-" shift
if "%mizfiles%"=="" set mizfiles=c:\mizar
if exist %mizfiles%\verifier.exe goto mizfiles
echo Can't find %mizfiles%\verifier.exe
goto exit
:mizfiles
for %%F in (A,a) do if "%1"=="/%%F+" goto s_accom
for %%F in (A,a) do if "%2"=="/%%F+" goto accom
%mizfiles%\makeenv %1
%mizfiles%\chkerr %1
if not ErrorLevel 1 goto continue
%mizfiles%\errflag %1
%mizfiles%\addfmsg %1 %mizfiles%\mizar
goto exit
:s_accom
shift
:accom
%mizfiles%\accom %1
%mizfiles%\chkerr %1
if not ErrorLevel 1 goto continue
%mizfiles%\errflag %1
%mizfiles%\addfmsg %1 %mizfiles%\mizar
goto exit
:continue
%mizfiles%\verifier %1
%mizfiles%\errflag %1
%mizfiles%\addfmsg %1 %mizfiles%\mizar
:exit
if exist *.$$$ del *.$$$
