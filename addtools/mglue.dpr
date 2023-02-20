(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program MGlue;

 uses pcmizver,mizenv,errhan,monitor;

 const BuffSize = $E000;
 var Buffer: array[0..BuffSize] of char;
     SourceText,TargetText: file;
     i: word;
     NumRead, NumWritten: integer;
 label 1;
begin
 writeln('Mglue, ',PCMizarVersionStr);
 writeln(Copyright);
 if ParamCount = 0 then
  begin
    writeln('Syntax:  mglue ArticleName');
    writeln;
    Halt(1);
  end;
 GetMizFileName('');
 FileExam(MizFileName+'.evd'); FileExam(MizFileName+'.tpr');
 writeln('Processing ',MizFileName);
 InitExitProc;
 assign(TargetText,MizFileName+'.miz');
 rewrite(TargetText,1);
 assign(SourceText,MizFileName+'.evd');
 reset(SourceText,1);
 repeat BlockRead(SourceText,Buffer, SizeOf(Buffer),NumRead);
  for i:=NumRead downto 1 do if Buffer[i] = ^Z then
   begin Numread:=i; goto 1 end;
1:
  BlockWrite(TargetText,Buffer,NumRead,NumWritten);
 until (NumRead = 0) or (NumWritten <> NumRead);
 close(SourceText);
 assign(SourceText,MizFileName+'.tpr');
 reset(SourceText,1);
 repeat BlockRead(SourceText,Buffer, SizeOf(Buffer),NumRead);
  BlockWrite(TargetText,Buffer,NumRead,NumWritten);
 until (NumRead = 0) or (NumWritten <> NumRead);
 close(SourceText);
 close(TargetText);
end.
