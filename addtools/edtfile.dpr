(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program EdtFile;

 uses pcmizver,mizenv,edt_han,monitor,mconsole;

 var Copying: boolean;
     TargetFileName: string;
begin
 DrawMizarScreen('Editor');
 if ParamCount = 0 then
  begin
    writeln('Syntax:  edtfile ArticleName [-c] [-79]');
    halt(1);
  end;
 InitExitProc;
 GetArticleName;
 TargetFileName:=MizFileName+'.$-$';
 FileExam(MizFileName+ArticleExt);
 FileExam(MizFileName+'.edt');
 writeln(MizFileName+ArticleExt,' => ',TargetFileName);
 Copying:=true;
 if paramcount>1 then
  if ParamStr(2) = '-c' then Copying:=false
  else if paramcount>2 then
   if ParamStr(3) = '-c' then Copying:=false;
 if paramcount>1 then
  if ParamStr(2) = '-79' then gForceShortLines:=false
  else if paramcount>2 then
   if ParamStr(3) = '-79' then gForceShortLines:=false;
 EditFile(MizFileName+ArticleExt,TargetFileName,Copying);
 FinishDrawing;
end.
