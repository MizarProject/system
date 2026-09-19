@echo off
rem
rem  Mizar revision utility batch
rem
if "%1"=="" goto usage
for %%F in (A,a) do if "%1"=="/%%F-" shift
if "%mizfiles%"==""  set mizfiles=c:\mizar
:mizfiles
for %%F in (A,a) do if "%1"=="/%%F+" goto s_accom
for %%F in (A,a) do if "%3"=="/%%F+" goto accom
%mizfiles%\makeenv %2
%mizfiles%\chkerr %2
if not ErrorLevel 1 goto continue
%mizfiles%\errflag %2
%mizfiles%\addfmsg %2 %mizfiles%\mizar
goto exit
:s_accom
shift
:accom
%mizfiles%\accom %2
%mizfiles%\chkerr %2
if not ErrorLevel 1 goto continue
%mizfiles%\errflag %2
%mizfiles%\addfmsg %2 %mizfiles%\mizar
goto exit
:continue
%1 %2
%mizfiles%\edtfile %2
copy %2.$-$ %2.miz
goto exit
:usage
echo  ÿ
echo  Mizar revision utility batch command
echo  ÿ
echo  Usage: revedt [ /a+ ] utility-name file-name
echo  ÿ
:exit
