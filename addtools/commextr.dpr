(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

program CommentsExtract;

uses pcmizver,monitor,mizenv;

 var InFile,CMM:text;
     InFileBuf: array[1..$6000] of char;
     CommBuf: array[1..$1000] of char;
     Line:string; x:integer;
begin
 writeln('Comments Extractor,  Version 1.0  ',Copyright);
 if ParamCount = 0 then
  begin
    writeln('Syntax:  COMMEXTR ArticleName');
    writeln;
  end;
 GetMizFileName('.evd');
 InitExitProc;
 FileExam(MizFileName+ArticleExt);
 writeln('Processing ',MizFileName+ArticleExt); writeln;
 assign(InFile,MizFileName+ArticleExt); settextbuf(InFile,InFileBuf);
 reset(InFile);
 assign(CMM,MizFileName+'.cmm'); settextbuf(CMM,CommBuf);
 rewrite(CMM);
 while not seekeof(InFile) do
  begin
   readln(InFile,Line); x:=Pos('::',Line);
   if x > 0 then writeln(CMM,copy(Line,x,length(Line)));
  end;
 close(CMM); close(InFile)
end.
