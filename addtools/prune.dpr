(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program Pruner;

uses pcmizver,mizenv;

var InFile,OutFile: text;
    InFileBuf,OutFileBuf: array[1..$6000] of char;
    Line: string; Options: string;
    lCom,i,lEmptyLinesNbr: integer;
    RemEmptyLines,RemComments,Prunning: boolean;
    TargetFileName: string;
begin
 writeln('Pruner,  Version 2.0  ',Copyright);
 if ParamCount = 0 then
  begin
    writeln('Syntax:  prune ArticleName [-[lc]]');
    writeln;
    writeln('Options:');
    writeln('   -l  Remove empty lines');
    writeln('   -c  Remove comments');
    writeln;
   halt(2);
  end;
 GetMizFileName('.miz');
 TargetFileName:=MizFileName+'.$-$';
 RemEmptyLines:=false;
 RemComments:=false;
 Prunning:=false;
 if paramcount>1 then
  begin
   Options:=ParamStr(2);
   if Options[1] <> '-' then
    if paramcount>2 then
     begin Options:=ParamStr(3);
      if Options[1] <> '-' then Options:='';
     end
    else Options:='';
   if Pos('l',Options) <> 0 then RemEmptyLines:=true;
   if Pos('c',Options) <> 0 then RemComments:=true;
  end;
 FileExam(MizFileName+ArticleExt);
 writeln(MizFileName+ArticleExt,' => ',TargetFileName);
 assign(InFile,MizFileName+ArticleExt); settextbuf(InFile,InFileBuf);
 reset(InFile);
 assign(OutFile,TargetFileName); settextbuf(OutFile,OutFileBuf);
 rewrite(OutFile);
 lEmptyLinesNbr:=0;
 while not eof(InFile) do
  begin
   readln(InFile,Line);
   if RemComments then
    begin
     lCom:=pos('::',Line);
     if lCom > 0 then
      begin
       delete(Line,lCom,length(Line));
       Prunning:=true;
      end;
    end;
   if length(line) <> 0 then while Line[length(line)] = ' ' do
    begin
     delete(Line,length(Line),1);
     Prunning:=true;
    end;
   if length(Line) = 0 then inc(lEmptyLinesNbr)
   else
    begin
     if lEmptyLinesNbr > 0 then
      if RemEmptyLines then Prunning:=true
      else
       begin
        for i:=1 to lEmptyLinesNbr do writeln(OutFile);
        lEmptyLinesNbr:=0;
       end;
     writeln(OutFile,Line);
    end;
  end;
  close(InFile);
  close(OutFile);
  if not Prunning then halt(1);
end.

