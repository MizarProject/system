(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program RemErrf;

uses mizenv,mconsole;

var
  Source, Listing: text; SourceBuff, ListingBuff: array[0..$6000] of char;
  RmFlagNbr: integer;
  Buffer: string;

begin
 if paramcount <1 then
  begin 
   Noise; 
   DrawMizarScreen('Remove Error Flags');
   writeln('Syntax:  remflags  articlename');
   FinishDrawing;
   halt(1) 
  end;
 GetMizFileName('');
 assign(Source,MizFileName+ArticleExt);
 settextbuf(Source,SourceBuff);
 {$I-} reset(Source); {$I+}
 if ioresult<>0 then
  begin writeln(^G+'Can''t open ',MizFileName); halt(2); end;
 assign(Listing,MizFileName+'.$$$'); settextbuf(Listing,ListingBuff);
 rewrite(Listing);
 RmFlagNbr:=0;
 while not eof(Source) do
  begin readln(Source,Buffer);
   if pos('::>',Buffer) <> 1 then writeln(Listing,Buffer)
    else inc(RmFlagNbr);
  end;
 close(Source); close(Listing);
 if RmFlagNbr = 0 then erase(Listing) else
  begin erase(Source);
    rename(Listing,MizFileName+ArticleExt);
  end
end.
