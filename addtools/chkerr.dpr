(******************************************************************************
   This file is part of the Mizar system.
   Copyright (c) Association of Mizar Users.
   License terms: GNU General Public License Version 3 or any later version.
******************************************************************************)

{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}
program ChkErr;

uses mizenv;
var ErrorsFile: text;

begin if paramcount = 0 then halt(1);
 GetMizFileName('');
 assign(ErrorsFile,MizFileName+'.err'); {$I-} reset(ErrorsFile); {$I+}
 if ioresult<> 0 then
  begin writeln(^G+^G+'Can''t open ',MizFileName+'.err'); halt(2) end;
 if not seekeof(ErrorsFile) then begin close(ErrorsFile); halt(1) end;
 close(ErrorsFile);
end.
