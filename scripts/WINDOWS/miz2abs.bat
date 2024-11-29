@echo off
rem
rem                    PC Mizar abstractor BATCH command
rem
if "%1"=="" goto exit
if "%mizfiles%"==""  set mizfiles=c:\mizar
if exist %mizfiles%\absedt.exe goto mizfiles
echo Can't find %mizfiles%\absedt.exe
goto exit
:mizfiles
%mizfiles%\accom %1
%mizfiles%\chkerr %1
if not ErrorLevel 1 goto continue
echo There are some accomodator errors
goto exit
:continue
%mizfiles%\absedt %1
%mizfiles%\edtfile %1
copy %1.$-$ %1.abs
echo Abstract file %1.abs created.
:exit
