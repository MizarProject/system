(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program Errflag;

uses mizenv,mconsole;

const  MaxErrNbr   = 5000;

type   ErrDes = record Line,Col,Code: integer; end;

var
  Source, Listing: text; SourceBuff, ListingBuff: array[0..$6000] of char;
  Errors: text; ErrorsBuff: array[0..$1000] of char;
  Error: array[0..MaxErrNbr+1] of ErrDes;
  CurrentError: ErrDes;
  ErrNbr, ErrNr, RmFlagNbr: integer;
  ErrorOvfl: boolean;
  i, j: integer;
  Buffer: string; Ch: char;
  SourceLine,TargetCol: integer;
  aFileName,FileExt: string;

label Earlier,EndOfErrors;
begin
 if paramcount <1 then
  begin
   Noise;
   DrawMizarScreen('Put Error Flags');
   writeln('Syntax:  errflag  filename');
   FinishDrawing;
   halt(1);
  end;
 GetFileExtName(1,'.miz',aFileName,FileExt);
 assign(Source,aFileName+FileExt);
 settextbuf(Source,SourceBuff);
 {$I-} reset(Source); {$I+}
 if ioresult<>0 then
  begin writeln(^G+'Can''t open ',aFileName+FileExt); halt(2); end;
 ErrNbr:=0; ErrorOvfl:=false;
 assign(Errors,aFileName+'.err'); settextbuf(Errors,ErrorsBuff);
 {$I-} reset(Errors); {$I+}
 if ioresult<>0 then halt(2);
 while not seekeof(Errors) do
  begin
    if ErrNbr >= MaxErrNbr then begin ErrorOvfl:=true; goto EndOfErrors end;
    inc(ErrNbr);
    with Error[ErrNbr] do readln(Errors,Line,Col,Code);
  end;
EndOfErrors:
 close(Errors);
 for i := 2 to ErrNbr do
  begin CurrentError:=Error[i];
   for j:=i-1 downto 1 do
    begin if CurrentError.Line > Error[j].Line then goto Earlier;
     if CurrentError.Line = Error[j].Line then
      if CurrentError.Col >= Error[j].Col then goto Earlier;
     Error[j+1]:=Error[j];
    end;
   j:=0;
Earlier:
   Error[j+1]:=CurrentError
  end;
 if ErrorOvfl then Error[MaxErrNbr].Code:=499;
 Error[ErrNbr+1].Line:=-1;
 ErrNr:=1;
 assign(Listing,aFileName+'.$$$'); settextbuf(Listing,ListingBuff);
 rewrite(Listing);
 SourceLine:=0; RmFlagNbr:=0;
 while not eof(Source) do
  begin inc(SourceLine); read(Source,Buffer);
   if pos('::>',Buffer) <> 1 then
    begin
     write(Listing,Buffer);
     while not eoln(Source) do begin read(Source,Ch); write(Listing,Ch) end;
     writeln(Listing);
     if SourceLine=Error[ErrNr].Line then
      begin  write(Listing,'::>'); TargetCol:=4;
        while (SourceLine=Error[ErrNr].Line) do
        with Error[ErrNr] do
         begin
           if col>=TargetCol then
             begin if col>TargetCol then write(Listing,' ': col-TargetCol);
               TargetCol:=col;   write(Listing,'*');
             end
            else write(Listing,',');
           if Code<10  then inc(TargetCol,2)
            else if Code<100 then inc(TargetCol,3)
             else inc(TargetCol,4);
           write(Listing,Code:1); inc(ErrNr);
         end;
        writeln(Listing);
       while SourceLine = Error[ErrNr].Line do inc(ErrNr);
      end;
    end else inc(RmFlagNbr);
    readln(Source);
  end;
 close(Source); close(Listing);
 if (ErrNbr = 0) and (RmFlagNbr = 0)
  then erase(Listing)
  else begin erase(Source); rename(Listing,aFileName+FileExt) end;
end.
