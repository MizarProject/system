(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program MSplit;

 uses pcmizver,mizenv,librenv,monitor,errhan,mscanner,mconsole;

 var  SourceBuff, TargetBuff: array[0..$6000] of char;
      PosBegin: Position;
      SourceText, TargetText: text;
      SourceLine: string;
      LineNbr: integer;

function EmptyLine: boolean;
 var K:integer; Line: string;
begin Line:=Copy(SourceLine,1,PosBegin.Col-1);
 for K:=1 to PosBegin.Col-1 do
  if Line[K]<>' ' then begin EmptyLine:=false; exit end;
 EmptyLine:=true;
end;

begin
 writeln('Msplit, ',PCMizarVersionStr);
 writeln(Copyright);
 if ParamCount = 0 then
  begin
    writeln('Syntax:  msplit [-l] ArticleName');
    writeln;
  end;
 GetOptions;
 GetMizFileName('.miz');
 FileExam(MizFileName+'.miz');
 writeln('Processing ',MizFileName,'.miz');
 OpenErrors(MizFileName);
 FileExam(MizFiles+'mizar.dct');
 InitSourceFile(MizFileName+ArticleExt,MizFiles+'mizar');
 ReadToken;
 if CurWord.Kind <> sy_Environ then ErrImm(212);
 while (CurWord.Kind <> sy_Begin) and (CurWord.Kind <> EOT) do ReadToken;
 if CurWord.Kind <> sy_Begin then ErrImm(213);
 PosBegin:=CurPos; dec(PosBegin.Col,4);
 CloseSourceFile;
 if ErrorNbr = 1 then
   begin writeln('**** One error detected.'); halt(1) end
  else if ErrorNbr > 1 then
   begin writeln('**** ',ErrorNbr,' error(s) detected.'); halt(1) end;
 assign(SourceText,MizFileName+'.miz'); settextbuf(SourceText,SourceBuff);
 reset(SourceText);
 assign(TargetText,MizFileName+'.evd'); settextbuf(TargetText,TargetBuff);
 rewrite(TargetText);
 LineNbr:=1; readln(SourceText,SourceLine);
 while LineNbr < PosBegin.Line do
  begin writeln(TargetText,SourceLine);
   readln(SourceText,SourceLine); inc(LineNbr);
  end;
 if PosBegin.Col > 1 then
  begin
   if not EmptyLine then
    writeln(TargetText,Copy(SourceLine,1,PosBegin.Col-1));
   delete(SourceLine,1,PosBegin.Col-1);
  end;
 close(TargetText);
 assign(TargetText,MizFileName+'.tpr'); settextbuf(TargetText,TargetBuff);
 rewrite(TargetText);
 writeln(TargetText,SourceLine);
 while not eof(SourceText) do
  begin readln(SourceText,SourceLine); writeln(TargetText,SourceLine) end;
 close(TargetText);
 close(SourceText);
end.
